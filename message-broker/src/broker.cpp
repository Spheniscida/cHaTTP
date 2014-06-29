# include "broker.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"
# include "conf.hpp"
# include "broker2broker.hpp"
# include "transaction-maps.hpp"
# include "user-cache.hpp"

namespace
{

}

/**
 * @brief Receive and process incoming messages.
 *
 * This function receives incoming messages and gives them to the appropriate handlers for each protocol event. It's
 * just a single infinite loop.
 *
 */
void ProtocolDispatcher::dispatch(void)
{
    vector<Receivable*> received_messages;
    unsigned int received_size = 0;

    received_messages.resize(4);

    while ( true )
    {
	// received_messages is an output argument!
	received_size = communicator.receiveMessages(received_messages);

	for ( unsigned int i = 0; i < received_size; i++ )
	{
	    Receivable *sndr = received_messages[i];

	    if ( PersistenceLayerResponse* resp = dynamic_cast<PersistenceLayerResponse*>(sndr) )
		handlePersistenceMessage(shared_ptr<PersistenceLayerResponse>(resp));
	    else if ( WebappRequest* req = dynamic_cast<WebappRequest*>(sndr) )
		handleWebappMessage(shared_ptr<WebappRequest>(req));
	    else if ( MessageRelayResponse* resp = dynamic_cast<MessageRelayResponse*>(sndr) )
		handleMessagerelayMessage(shared_ptr<MessageRelayResponse>(resp));
	    else if ( B2BIncoming* msg = dynamic_cast<B2BIncoming*>(sndr) )
		handleBrokerMessage(shared_ptr<B2BIncoming>(msg));
	    else
		throw BrokerError(ErrorType::unimplemented,"dispatch(): Unimplemented origin (all casts failed).");

	    // Shared pointers are destroyed automatically until control is here.
	}
    }
}
/***************************** Message handlers ****************************/

void ProtocolDispatcher::handlePersistenceMessage(shared_ptr<PersistenceLayerResponse> msg)
{

    debug_log("tid ", thread_id, " received from Persistence: ", msg->get_protobuf().DebugString());
    switch ( msg->type() )
    {
	case chattp::PersistenceResponse::LOOKEDUP:
	    onPersistenceULKDUP(*msg);
	    break;
	case chattp::PersistenceResponse::SAVEDMESSAGE:
	    onPersistenceMSGSVD(*msg);
	    break;
	case chattp::PersistenceResponse::REGISTERED:
	    onPersistenceUREGD(*msg);
	    break;
	case chattp::PersistenceResponse::CHECKEDPASS:
	    onPersistenceCHKDPASS(*msg);
	    break;
	case chattp::PersistenceResponse::LOGGEDIN:
	    onPersistenceLGDIN(*msg);
	    break;
	case chattp::PersistenceResponse::LOGGEDOUT:
	    onPersistenceLGDOUT(*msg);
	    break;
	case chattp::PersistenceResponse::GOTMESSAGES:
	    onPersistenceMSGS(*msg);
	    break;
	case chattp::PersistenceResponse::SAVEDSETTINGS:
	case chattp::PersistenceResponse::GOTSETTINGS:
	    onPersistenceSettingsResponse(*msg);
	    break;
	case chattp::PersistenceResponse::HEARTBEAT_RECEIVED:
	    onPersistenceHeartbeated(*msg);
	    break;
    }
}

void ProtocolDispatcher::handleWebappMessage(shared_ptr<WebappRequest> msg)
{
    debug_log("tid ", thread_id, " received from Webapp: ", msg->get_protobuf().DebugString());
    switch ( msg->type() )
    {
	case WebappRequestMessage::QUERYSTATUS:
	    onWebAppUONLQ(*msg);
	    break;
	case WebappRequestMessage::SENDMESSAGE:
	    onWebAppSNDMSG(*msg);
	    break;
	case WebappRequestMessage::LOGIN:
	    onWebAppLOGIN(*msg);
	    break;
	case WebappRequestMessage::LOGOUT:
	    onWebAppLOGOUT(*msg);
	    break;
	case WebappRequestMessage::REGISTER:
	    onWebAppUREG(*msg);
	    break;
	case WebappRequestMessage::GETMESSAGES:
	    onWebAppMSGGT(*msg);
	    break;
	case WebappRequestMessage::AUTHORIZED:
	    onWebAppISAUTH(*msg);
	    break;
	case WebappRequestMessage::GETSETTINGS:
	case WebappRequestMessage::SAVESETTINGS:
	    onWebAppSettingsRequest(*msg);
	    break;
	case WebappRequestMessage::CHANNEL_HEARTBEAT:
	    onWebappHeartbeat(*msg);
	    break;
    }
}

void ProtocolDispatcher::handleMessagerelayMessage(shared_ptr<MessageRelayResponse> msg)
{
    debug_log("tid ", thread_id, " received from Message Relay: ", msg->get_protobuf().DebugString());
    switch ( msg->type() )
    {
	case chattp::MessageRelayResponse::SENTMESSAGE:
	    onMessagerelayMSGSNT(*msg);
	    break;
	case chattp::MessageRelayResponse::DELETEDCHANNEL:
	    onMessagerelayDELTDCHAN(*msg);
	    break;
	case chattp::MessageRelayResponse::CREATEDCHANNEL:
	    onMessagerelayCHANCREAT(*msg);
	    break;
    }
}

void ProtocolDispatcher::handleBrokerMessage(shared_ptr<B2BIncoming> msg)
{
    debug_log("tid ", thread_id, " received from other Broker: ", msg->get_protobuf().DebugString());
    switch ( msg->type() )
    {
	case B2BMessage::SENDMESSAGE:
	    onB2BSNDMSG(*msg);
	    break;
	case B2BMessage::MESSAGESENT:
	    onB2BMSGSNT(*msg);
	    break;
    }
}

/***************************** Event handlers ******************************/

void ProtocolDispatcher::onB2BSNDMSG(const B2BIncoming& msg)
{
    const sequence_t seqnum = msg.sequence_number();
    OutstandingTransaction transaction;

    if ( ! global_broker_settings.getClusteredMode() ) // Shouldn't happen.
    {
	MessageForB2B failmsg(seqnum,false);

	communicator.send(failmsg,msg.origin_broker);

	return;
    }

    transaction.type = OutstandingType::messagerelayB2BMSGSNT;
    transaction.original_sequence_number = seqnum;

    MessageForRelay relaymsg(msg.channel_id(),msg.get_protobuf().mesg());

    try
    {
	communicator.send(relaymsg);
	transaction_cache.insertB2BOrigin(seqnum,msg.origin_broker);
	transaction_cache.insertTransaction(relaymsg.sequence_number(),transaction);
    } catch (libsocket::socket_exception e)
    {
	MessageForB2B failmsg(seqnum,false);

	// This one should work because we received this message!
	communicator.send(failmsg,msg.origin_broker);
	throw e;
    }
}

void ProtocolDispatcher::onB2BMSGSNT(const B2BIncoming& msg)
{
    const sequence_t seqnum = msg.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction (B2B MSGSNT)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    if ( ! msg.status() )
    {
	transaction.type = OutstandingType::persistenceB2BMSGSVD;

	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE,original_webapp_request.get_protobuf().mesg());

	transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);

	communicator.send(cmd);
    }

    if ( --(*transaction.remaining_count) == 0 )
    {
	WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::SENTMESSAGE,msg.status(),"9,Couldn't deliver message");
	communicator.send(resp);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	delete transaction.saved;
	delete transaction.remaining_count;
    }

    transaction_cache.eraseTransaction(seqnum);
}

void ProtocolDispatcher::onWebAppUREG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUREGD;
    transaction.original_sequence_number = rq.sequence_number();

    PersistenceLayerCommand cmd(PersistenceRequest::REGISTER,rq.user_name(),rq.password());

    try
    {
	communicator.send(cmd);
	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(rq.sequence_number(),rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(rq.sequence_number(),WebappResponseMessage::REGISTERED,false,"4,Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();
    sequence_t new_seqnum;

    OutstandingTransaction transaction;
    transaction.original_sequence_number = seqnum;

    // Next step is ULKUP, then CHKPASS, then LOGIN
    transaction.type = OutstandingType::persistenceLoginULKDUP;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

    try
    {
	communicator.send(cmd);

	new_seqnum = cmd.sequence_number();
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::LOGGEDIN,false,string(""),"4,Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }

    transaction_cache.insertTransaction(new_seqnum,transaction);
    transaction_cache.insertWebappRequest(seqnum,rq);
}

void ProtocolDispatcher::onWebAppLOGOUT(const WebappRequest& rq)
{
    OutstandingTransaction transaction;
    const sequence_t seqnum = rq.sequence_number();

    transaction.type = OutstandingType::persistenceLogoutULKDUP;
    transaction.original_sequence_number = seqnum;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP, rq.user_name());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(seqnum,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::LOGGEDOUT,false,"4,Internal error! (Persistence down)");
	communicator.send(failresp);
    }
}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;
    transaction.type = OutstandingType::persistenceSndmsgULKDUP;

    if ( rq.user_name() == rq.message_receiver() )
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"19,You may not send messages to yourself");
	communicator.send(failresp);
	return;
    }

    if ( rq.user_name() != rq.get_protobuf().mesg().sender() )
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"2,You are not who you pretend to be");
	communicator.send(failresp);
	return;
    }

    const std::list<chattp::PersistenceResponse::UserLocation>& cached_sender = user_cache.getLocations(rq.user_name()),
							       &cached_receiver = user_cache.getLocations(rq.message_receiver());

    if ( cached_receiver.empty() || cached_sender.empty() )
    {
	// Sender must remain at first position!
	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,vector<string>({rq.message_sender(), rq.message_receiver()}));

	try
	{
	    communicator.send(cmd);

	    messages_processed++;
	    transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"4,Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else
    {
	sendMessage(cached_sender.begin(),cached_sender.end(),cached_receiver.begin(),cached_receiver.end(),rq,seqnum,rq.get_protobuf().mesg());

	transaction_cache.insertWebappRequest(rq.sequence_number(),rq);
    }
}

void ProtocolDispatcher::onWebAppUONLQ(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUonlqULKDUP;
    transaction.original_sequence_number = seqnum;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(seqnum,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::USERSTATUS,false,"4,Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onWebAppMSGGT(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;
    transaction.type = OutstandingType::persistenceMessageGetULKDUP;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(seqnum,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponseMessage mesg;

	WebappResponse failresp(seqnum,WebappResponseMessage::GOTMESSAGES,false,mesg.mesgs().begin(), mesg.mesgs().end(),"4,Internal error (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onWebAppISAUTH(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceIsauthULKDUP;
    transaction.original_sequence_number = seqnum;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(seqnum,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::AUTHORIZED,false,false,"4,Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

// Because it's so simple, handle both SAVESETTINGS and GETSETTINGS requests.
void ProtocolDispatcher::onWebAppSettingsRequest(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;
    transaction.type = rq.type() == WebappRequestMessage::SAVESETTINGS ? OutstandingType::persistenceSaveSettingsULKDUP : OutstandingType::persistenceGetSettingsULKDUP;

    PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	transaction_cache.insertWebappRequest(seqnum,rq);
    } catch (libsocket::socket_exception e)
    {
	if ( rq.type() == WebappRequestMessage::SAVESETTINGS )
	{
	    WebappResponse failresp(seqnum,
				    WebappResponseMessage::SAVEDSETTINGS,
				    false,"4,Internal error (Persistence down)");

	    communicator.send(failresp);
	} else
	{
	    WebappResponse failresp(seqnum,
				    WebappResponseMessage::GOTSETTINGS,
				    false,
				    string(""),
				    "4,Internal error (Persistence down)");

	    communicator.send(failresp);
	}
	return;
    }

}

void ProtocolDispatcher::onWebappHeartbeat(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;
    transaction.type = OutstandingType::persistenceHeartbeated;

    PersistenceLayerCommand cmd(PersistenceRequest::CHANNEL_HEARTBEAT,rq.user_name(),string(""),rq.channel_id());

    try
    {
	communicator.send(cmd);

	transaction_cache.insertWebappRequest(seqnum,rq);
	transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::HEARTBEAT_RECEIVED,false,"4,Internal error (Persistence down)");

	communicator.send(failresp);
    }
}

void ProtocolDispatcher::onPersistenceUREGD(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceUREGD )
    {
	WebappResponse resp(original_webapp_request.sequence_number(), WebappResponseMessage::REGISTERED, rp.status(),"15,Probably, this user already exists");

	communicator.send(resp);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceUREGD: Expected transaction type persistenceUREG, but received other.");
    }
}

void ProtocolDispatcher::onPersistenceCHKDPASS(const PersistenceLayerResponse& rp)
{
    // This is called on a response to a request made by onWebAppLOGIN.

    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transaction_cache.eraseTransaction(seqnum);

	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceCHKDPASS )
    {
	WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	// Password/user incorrect?
	if ( rp.status() == false )
	{
	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"13,Wrong password!");

	    transaction_cache.eraseTransaction(seqnum);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());

	    communicator.send(resp);
	} else
	{
	    string channel_id = generateChannelId();
	    PersistenceLayerCommand cmd(PersistenceRequest::LOGIN,original_webapp_request.user_name(),global_broker_settings.getMessageBrokerName(),channel_id);

	    /* Here, we're doing something which is not very clean: We save the channel id in the "original" webapp request
	    * although that didn't actually bear a channel id. However, this is necessary so the onPersistenceLGDIN() handler
	    * may reply with a sequence number.
	    */
	    original_webapp_request.channel_id_ = channel_id;

	    try
	    {
		communicator.send(cmd);

		transaction.type = OutstandingType::persistenceLGDIN;
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"4,Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	}
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceCHKDPASS: Expected transaction type persistenceCHKDPASS, but received other.");
    }
}

void ProtocolDispatcher::onPersistenceLGDIN(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transaction_cache.eraseTransaction(seqnum);

	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceLGDIN )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);
	if ( rp.status() )
	{
	    // That's this hacky construction again -- the channel_id was not actually in that request!
	    MessageForRelay newchanmsg(original_webapp_request.channel_id_,MessageRelayRequest::CREATECHANNEL);

	    try
	    {
		communicator.send(newchanmsg);

		transaction.type = OutstandingType::messagerelayCHANCREAT;
		transaction_cache.eraseAndInsertTransaction(seqnum,newchanmsg.sequence_number(),transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"6,Internal error! (Channel registration failed b/c relay is down)");
		communicator.send(failresp);

		PersistenceLayerCommand logout_cmd(PersistenceRequest::LOGOUT,original_webapp_request.user_name());

		transaction.original_sequence_number = original_webapp_request.sequence_number();
		transaction.type = OutstandingType::persistenceAfterFailedChancreatLogout;

		transaction_cache.eraseAndInsertTransaction(seqnum,logout_cmd.sequence_number(),transaction);

		communicator.send(logout_cmd);

		throw e;
	    }
	} else
	{
	    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"17,Persistence denied login");

	    communicator.send(failresp);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);
	    return;
	}
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceLGDIN: Expected transaction type persistenceLGDIN, but received other.");
    }
}

void ProtocolDispatcher::onPersistenceULKDUP(const PersistenceLayerResponse& rp)
{
    // We have received an ULKDUP response. What do we do next?

    const sequence_t seqnum = rp.sequence_number();

    // No reference! We need this after the transaction is removed from the cache.
    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    // We abort the transaction if it failed or no users were returned (both conditions should normally be true)
    if ( ! rp.status() || rp.get_protobuf().user_locations_size() < 1 )
    {
	WebappResponseMessage::WebappResponseType response_type;

	switch ( transaction.type )
	{
	    case OutstandingType::persistenceSndmsgULKDUP:
		response_type = WebappResponseMessage::SENTMESSAGE;
		break;
	    case OutstandingType::persistenceUonlqULKDUP:
		response_type = WebappResponseMessage::USERSTATUS;
		break; //  login logout messageget isauth
	    case OutstandingType::persistenceLoginULKDUP:
		response_type = WebappResponseMessage::LOGGEDIN;
		break;
	    case OutstandingType::persistenceLogoutULKDUP:
		response_type = WebappResponseMessage::LOGGEDOUT;
		break;
	    case OutstandingType::persistenceMessageGetULKDUP:
		response_type = WebappResponseMessage::GOTMESSAGES;
		break;
	    case OutstandingType::persistenceIsauthULKDUP:
		response_type = WebappResponseMessage::AUTHORIZED;
		break;
	    case OutstandingType::persistenceSaveSettingsULKDUP:
		response_type = WebappResponseMessage::SAVEDSETTINGS;
		break;
	    case OutstandingType::persistenceGetSettingsULKDUP:
		response_type = WebappResponseMessage::GOTSETTINGS;
	    default:
		throw BrokerError(ErrorType::genericImplementationError,"This should not have happened – unexpected transaction type in onPersistenceULKDUP");
	}

	const string error_message = "15,User doesn't exist (or Persistence failed otherwise)";

	switch ( response_type )
	{
	    case WebappResponseMessage::LOGGEDOUT:
	    case WebappResponseMessage::SENTMESSAGE:
	    case WebappResponseMessage::REGISTERED:
	    case WebappResponseMessage::SAVEDSETTINGS:
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),response_type,false,error_message);
		communicator.send(failresp);
	    }
		break;
	    case WebappResponseMessage::GOTMESSAGES:
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),response_type,false,
		    rp.get_protobuf().mesgs().begin(),rp.get_protobuf().mesgs().begin(),error_message); // no iterations allowed (it1 == it2)
		communicator.send(failresp);
	    }
		break;
	    case WebappResponseMessage::USERSTATUS:
	    case WebappResponseMessage::AUTHORIZED:
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),response_type,false,false,error_message);
		communicator.send(failresp);
	    }
		break;
	    case WebappResponseMessage::LOGGEDIN:
	    case WebappResponseMessage::GOTSETTINGS:
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),response_type,false,string(""),error_message);
		communicator.send(failresp);
	    }
		break;
	    default:;
	}

	transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    // Vectored lookup
    if ( transaction.type == OutstandingType::persistenceSndmsgULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::SENDMESSAGE )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type SENDMESSAGE; however, this is not the case.");

	google::protobuf::RepeatedPtrField<const chattp::PersistenceResponse::UserLocation>::iterator sender_begin, sender_end, receiver_begin, receiver_end;

	sender_end = sender_begin = rp.get_protobuf().user_locations().begin();

	for ( ; sender_end->user_name() == original_webapp_request.message_sender(); sender_end++ );

	receiver_begin = sender_end; // sender_end is past-the-last-sender
	receiver_end = rp.get_protobuf().user_locations().end();

	// We use the "set" function because it's authoritative what persistence says.
	user_cache.setForUser(original_webapp_request.get_protobuf().mesg().sender(),sender_begin,receiver_end); // No worries, setForUser inserts only the entries for that user.
	user_cache.setForUser(original_webapp_request.get_protobuf().mesg().receiver(),sender_begin,receiver_end);

	sendMessage(sender_begin,sender_end,receiver_begin,receiver_end,original_webapp_request,seqnum,original_webapp_request.get_protobuf().mesg());

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	if ( original_webapp_request.type() != WebappRequestMessage::QUERYSTATUS )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type QUERYSTATUS; however, this is not the case.");

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	// Respond to the "is user online?" request. It's enough to examine only the first location that has been returned.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::USERSTATUS,rp.status(), // is true here!
			    loc.online() &&
			    (global_broker_settings.getClusteredMode() || loc.broker_name() == global_broker_settings.getMessageBrokerName()),
			    "15,User probably doesn't exist"
			);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	transaction_cache.eraseTransaction(seqnum);

	communicator.send(resp);
    } else if ( transaction.type == OutstandingType::persistenceLoginULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::LOGIN )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type LOGIN; however, this is not the case.");

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	transaction.type = OutstandingType::persistenceCHKDPASS;
	PersistenceLayerCommand cmd(PersistenceRequest::CHECKPASS,original_webapp_request.user_name(),original_webapp_request.password());

	try
	{
	    communicator.send(cmd);
	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDIN,false,string(""),"4,Internal error! (Persistence down)");
	    communicator.send(resp);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    throw e;
	}
    } else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::LOGOUT )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type LOGOUT; however, this is not the case.");

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	bool online = false, found_user = false, found_channel = false, right_broker = false;

	for ( auto it = rp.get_protobuf().user_locations().begin(); it != rp.get_protobuf().user_locations().end(); it++ )
	{
	    if ( ! online && it->online() )
		online = true;
	    if ( ! found_user && it->user_name() == original_webapp_request.user_name() )
		found_user = true;
	    if ( ! found_channel && it->channel_id() == original_webapp_request.channel_id() )
	    {
		found_channel = true;
		if ( ! right_broker && it->broker_name() == global_broker_settings.getMessageBrokerName() )
		    right_broker = true;
	    }
	}

	if ( online && found_channel && found_user && right_broker )
	{
	    PersistenceLayerCommand cmd(PersistenceRequest::LOGOUT, original_webapp_request.user_name(),string(""),original_webapp_request.channel_id());

	    transaction.type = OutstandingType::persistenceLGDOUT;

	    try
	    {
		communicator.send(cmd);

		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);

	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,false,"4,Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	} else
	{
	    string error_message;

	    if ( ! found_user )
		error_message = "15,User doesn't exist"; // Won't happen because this is checked at the beginning
	    else if ( ! online )
		error_message = "1,User is offline";
	    else if ( ! found_channel )
		error_message = "2,unauthorized to perform logout.";
	    else if ( ! right_broker )
		error_message = "10,Wrong broker";

	    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,false,error_message);

	    communicator.send(failresp);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	    transaction_cache.eraseTransaction(seqnum);

	}

    } else if ( transaction.type == OutstandingType::persistenceMessageGetULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::GETMESSAGES )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type GETMESSAGES; however, this is not the case.");
	// Next: send msggt to persistence.

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	if ( !loc.online() || original_webapp_request.channel_id() != loc.channel_id() )
	{
	    string error_message;

	    if ( ! loc.online() )
		error_message = "1,User is offline";
	    else
		error_message = "2,User not authorized";

	    WebappResponse failresp(original_webapp_request.sequence_number(),
				    WebappResponseMessage::GOTMESSAGES,
				    false,
				    rp.get_protobuf().mesgs().begin(),
				    rp.get_protobuf().mesgs().end(),
				    error_message
   				);

	    communicator.send(failresp);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    return;
	}

	transaction.type = OutstandingType::persistenceMSGS;

	PersistenceLayerCommand cmd(PersistenceRequest::GETMESSAGES,original_webapp_request.user_name());

	try
	{
	    communicator.send(cmd);

	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	} catch ( libsocket::socket_exception e )
	{

	    WebappResponse failresp(original_webapp_request.sequence_number(),
				    WebappResponseMessage::GOTMESSAGES,
				    false,
				    rp.get_protobuf().mesgs().begin(),
				    rp.get_protobuf().mesgs().end(),
				    "4,Internal error! (Persistence down)"
   				);
	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(failresp);

	    throw e;
	}

    } else if ( transaction.type == OutstandingType::persistenceIsauthULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::AUTHORIZED )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type LOGOUT; however, this is not the case.");

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	bool auth_status = loc.online()
			&& loc.broker_name() == global_broker_settings.getMessageBrokerName()
			&& loc.channel_id() == original_webapp_request.channel_id();

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::AUTHORIZED,rp.status(),auth_status,"15,User probably doesn't exist");
	communicator.send(resp);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

    } else if ( transaction.type == OutstandingType::persistenceSaveSettingsULKDUP || transaction.type == OutstandingType::persistenceGetSettingsULKDUP )
    {
	if ( original_webapp_request.type() != chattp::WebappRequestMessage::SAVESETTINGS && original_webapp_request.type() != chattp::WebappRequestMessage::GETSETTINGS )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type SAVESETTINGS or GETSETTINGS; however, this is not the case.");

	user_cache.setForUser(original_webapp_request.user_name(),rp.get_protobuf().user_locations().begin(),rp.get_protobuf().user_locations().end());

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	if ( !loc.online() || loc.channel_id() != original_webapp_request.channel_id() || loc.broker_name() != global_broker_settings.getMessageBrokerName() )
	{
	    string err_msg = ! loc.online() ? "1,User is offline" : "2,User not authorized";

	    if ( transaction.type == OutstandingType::persistenceSaveSettingsULKDUP )
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),
			WebappResponseMessage::SAVEDSETTINGS,
			false,err_msg);

		communicator.send(failresp);
	    } else
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),
			WebappResponseMessage::GOTSETTINGS,
			false,string(""),err_msg);

		communicator.send(failresp);
	    }

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	    transaction_cache.eraseTransaction(seqnum);

	    return;
	}

	if ( original_webapp_request.type() == WebappRequestMessage::SAVESETTINGS )
	{
	    PersistenceLayerCommand cmd(PersistenceRequest::SAVESETTINGS,original_webapp_request.user_name(),original_webapp_request.settings());

	    transaction.type = OutstandingType::persistenceSAVEDSETTINGS;

	    try
	    {
		communicator.send(cmd);

		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	    } catch ( libsocket::socket_exception e )
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SAVEDSETTINGS,false,"4,Internal error! (Persistence down)");

		transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
		transaction_cache.eraseTransaction(seqnum);

		communicator.send(failresp);

		throw e;
	    }

	} else if ( original_webapp_request.type() == WebappRequestMessage::GETSETTINGS )
	{
	    PersistenceLayerCommand cmd(PersistenceRequest::GETSETTINGS,original_webapp_request.user_name());

	    transaction.type = OutstandingType::persistenceGOTSETTINGS;

	    try
	    {
		communicator.send(cmd);

		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	    } catch ( libsocket::socket_exception e )
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::GOTSETTINGS,false,string(""),"4,Internal error! (Persistence down)");

		transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	}
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceULKDUP: Unexpected transaction type.");
    }
}

void ProtocolDispatcher::onPersistenceMSGSVD(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	transaction_cache.eraseTransaction(seqnum);

	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceMSGSVD || transaction.type == OutstandingType::persistenceB2BMSGSVD )
    {
	if ( --(*transaction.remaining_count) == 0 )
	{
	    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,rp.status(),"14,Internal error! (Persistence didn't accept message)");

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    delete transaction.saved;
	    delete transaction.remaining_count;

	    communicator.send(resp);
	}
	transaction_cache.eraseTransaction(seqnum);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGSVD: Expected transaction type persistenceMSGSVD, but received other.");
    }
}

void ProtocolDispatcher::onPersistenceLGDOUT(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	transaction_cache.eraseTransaction(seqnum);

	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceLGDOUT )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,rp.status(),"2,Probably unauthorized (or offline)");

	communicator.send(resp);

	if ( rp.status() )
	{
	    MessageForRelay delchanmsg(original_webapp_request.channel_id(),MessageRelayRequest::DELETECHANNEL);

	    transaction.type = OutstandingType::messagerelayDELTDCHAN;
	    transaction_cache.eraseAndInsertTransaction(seqnum,delchanmsg.sequence_number(),transaction);

	    // Cache ops
	    user_cache.removeForUser(original_webapp_request.user_name(),original_webapp_request.channel_id());
	    if ( user_cache.getLocations(original_webapp_request.user_name()).empty() )
	    {
		chattp::PersistenceResponse::UserLocation loc;

		loc.set_user_name(original_webapp_request.user_name());
		loc.set_online(false);
		user_cache.addForUser(original_webapp_request.user_name(),loc);
	    }

	    communicator.send(delchanmsg);
	} else
	{
	    transaction_cache.eraseTransaction(seqnum);
	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	}
    } else if ( transaction.type == OutstandingType::persistenceAfterFailedChancreatLogout )
    {
	// No user cache modification because the failed location hadn't been added anyway.
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"7,Channel couldn't be created");

	communicator.send(resp);

	transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	transaction_cache.eraseTransaction(seqnum);

    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceLGDOUT: Expected transaction type persistenceLGDOUT or persistenceAfterFailedChancreatLogout, but received other.");
    }

}

void ProtocolDispatcher::onPersistenceMSGS(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();
    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference (persistence layer)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    if ( transaction.type != OutstandingType::persistenceMSGS )
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGS: Expected type persistenceMSGS, but got other.");
    }

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::GOTMESSAGES,
			rp.status(),
			rp.get_protobuf().mesgs().begin(),
			rp.get_protobuf().mesgs().end());

    communicator.send(resp);

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
    transaction_cache.eraseTransaction(seqnum);
}

void ProtocolDispatcher::onPersistenceSettingsResponse(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    const OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference (persistence layer)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    if ( transaction.type == OutstandingType::persistenceSAVEDSETTINGS )
    {
	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SAVEDSETTINGS,rp.status(),"17,Persistence refused to save settings.");

	communicator.send(resp);

    } else if ( transaction.type == OutstandingType::persistenceGOTSETTINGS )
    {
	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::GOTSETTINGS,rp.status(),rp.get_protobuf().settings(),"17,Persistence didn't deliver settings.");

	communicator.send(resp);

    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGS: Expected type persistenceMSGS, but got other.");
    }

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
    transaction_cache.eraseTransaction(seqnum);
}

void ProtocolDispatcher::onPersistenceHeartbeated(const PersistenceLayerResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    const OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	error_log("Received dangling transaction reference. (Persistence)");

	transaction_cache.eraseTransaction(seqnum);
    }

    if ( transaction.type == OutstandingType::persistenceHeartbeated )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::HEARTBEAT_RECEIVED,rp.status(),"2,User not-authorized or non-existing.");

	communicator.send(resp);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayMSGSNT: Expected transaction type persistenceHeartbeated, but received other.");
    }

}

void ProtocolDispatcher::onMessagerelayMSGSNT(const MessageRelayResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	error_log("Received dangling transaction reference. (message relay)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    if ( transaction.type == OutstandingType::messagerelayMSGSNT )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( rp.status() || (original_webapp_request.get_protobuf().mesg().is_typing() || original_webapp_request.get_protobuf().mesg().has_seen()) )
	{
	    if ( --(*transaction.remaining_count) == 0 )
	    {
		WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,true,"");

		transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
		delete transaction.saved;
		delete transaction.remaining_count;

		communicator.send(resp);
	    }

	    transaction_cache.eraseTransaction(seqnum);
	} else // save message to persistence (only if non-meta, check is in previous condition)
	{
	    if ( ! *transaction.saved )
	    {
		PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, original_webapp_request.get_protobuf().mesg());

		transaction.type = OutstandingType::persistenceMSGSVD;

		try
		{
		    communicator.send(cmd);
		    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
		} catch (libsocket::socket_exception e)
		{
		    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"4,Internal error! (Persistence down)");
		    communicator.send(failresp);

		    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
		    transaction_cache.eraseTransaction(seqnum);

		    throw e;
		}

		*transaction.saved = true;
	    }
	}
    } else if ( transaction.type == OutstandingType::messagerelayB2BMSGSNT )
    {
	const string& message_sender_broker = transaction_cache.lookupB2BOrigin(transaction.original_sequence_number);

	MessageForB2B mesg(transaction.original_sequence_number,rp.status());

	// UDP doesn't fail.
	communicator.send(mesg,message_sender_broker);

	transaction_cache.eraseB2BOrigin(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayMSGSNT: Expected transaction type messagerelayMSGSNT or messagerelayB2BMSGSNT, but received other.");
    }
}

void ProtocolDispatcher::onMessagerelayDELTDCHAN(const MessageRelayResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference. (message relay)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
    transaction_cache.eraseTransaction(seqnum);
}

void ProtocolDispatcher::onMessagerelayCHANCREAT(const MessageRelayResponse& rp)
{
    const sequence_t seqnum = rp.sequence_number();

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference. (message relay)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    if ( transaction.type == OutstandingType::messagerelayCHANCREAT )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

        if ( rp.status() )
	{
	    WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDIN,rp.status(),original_webapp_request.channel_id_,"12,Channel couldn't be created");


	    chattp::PersistenceResponse::UserLocation loc;

	    loc.set_broker_name(global_broker_settings.getMessageBrokerName());
	    loc.set_user_name(original_webapp_request.user_name());
	    loc.set_channel_id(original_webapp_request.channel_id_);
	    loc.set_online(true);

	    user_cache.addForUser(original_webapp_request.user_name(),loc);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    communicator.send(resp);

	} else
	{
	    // Log out in database, send fail code.
	    PersistenceLayerCommand logout_cmd(PersistenceRequest::LOGOUT,original_webapp_request.user_name(),string(""),original_webapp_request.channel_id());
	    OutstandingTransaction logout_transaction;

	    logout_transaction.original_sequence_number = original_webapp_request.sequence_number();
	    logout_transaction.type = OutstandingType::persistenceAfterFailedChancreatLogout;

	    transaction_cache.insertTransaction(logout_cmd.sequence_number(),logout_transaction);

	    communicator.send(logout_cmd);
	}

	transaction_cache.eraseTransaction(seqnum);

    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayCHANCREAT: Expected transaction type messagerelayCHANCREAT, but received other.");
    }
}

/********************************** OUT-SOURCED FUNCTIONALITY ****************************/

template<typename SenderIteratorT, typename ReceiverIteratorT>
void ProtocolDispatcher::sendMessage(SenderIteratorT sender_begin, SenderIteratorT sender_end,
				     ReceiverIteratorT recv_begin, ReceiverIteratorT recv_end,
				     const WebappRequest& original_webapp_request, sequence_t seqnum, const ChattpMessage& mesg)
{
    bool authorized = false;

    // Check if any of the sender locations shows that the sender is authorized to send
    for ( auto it = sender_begin; it != sender_end; it++ )
    {
	if ( it->user_name() == original_webapp_request.user_name()
	    && it->online()
	    && it->channel_id() == original_webapp_request.channel_id()
	    && it->broker_name() == global_broker_settings.getMessageBrokerName() )
	{
	    authorized = true;
	    break;
	}
    }

    // Sender not authorized.
    if ( ! authorized )
    {
	WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"20,Not authorized or offline");

	communicator.send(failresp);

	transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    transaction_cache.eraseTransaction(seqnum);

    unsigned int n_receivers = 0;

    for ( auto it = recv_begin; it != recv_end; it++, n_receivers++ );

    // Fast-forward to receiver information

    OutstandingTransaction new_transaction;

    new_transaction.original_sequence_number = original_webapp_request.sequence_number();

    new_transaction.remaining_count = new std::atomic<unsigned short>;
    new_transaction.saved = new bool;

    *new_transaction.remaining_count = n_receivers;
    new_transaction.original_count = *new_transaction.remaining_count;

    *new_transaction.saved = false;

    for ( auto it = recv_begin; it != recv_end; it++ )
    {
	const PersistenceResponse::UserLocation& receiver = *it;

	if ( receiver.online() )
	{
	    if ( receiver.broker_name() == global_broker_settings.getMessageBrokerName() ) // User is on this broker?
	    {
		MessageForRelay msg(receiver.channel_id(),original_webapp_request.get_protobuf().mesg());
		new_transaction.type = OutstandingType::messagerelayMSGSNT;

		try
		{
		    communicator.send(msg);
		    transaction_cache.insertTransaction(msg.sequence_number(),new_transaction);
		} catch (libsocket::socket_exception e)
		{
		    // Message relay is offline, therefore save that message.
		    if ( ! original_webapp_request.get_protobuf().mesg().is_typing() && ! original_webapp_request.get_protobuf().mesg().has_seen() )
		    {
			PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, original_webapp_request.get_protobuf().mesg());

			new_transaction.type = OutstandingType::persistenceMSGSVD;

			try
			{
			    communicator.send(cmd);
			    transaction_cache.insertTransaction(cmd.sequence_number(),new_transaction);
			} catch (libsocket::socket_exception e)
			{
			    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"6,Internal error! (Message relay down, Persistence too)");
			    communicator.send(failresp);

			    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());

			    // Do not throw in the loop, otherwise no other messages will be delivered.
// 				throw e;
			}
		    } else
		    {
			WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,string("6,Internal error! (Message relay down)"));

			communicator.send(resp);

			transaction_cache.eraseWebappRequest(new_transaction.original_sequence_number);
		    }

		    // Do not throw in the loop, otherwise no other messages will be delivered.
// 			throw e;
		}
	    } else if ( global_broker_settings.getClusteredMode() ) // B2B communication!
	    {
		MessageForB2B broker_message(original_webapp_request.get_protobuf().mesg(),receiver.channel_id());

		new_transaction.type = OutstandingType::b2bMSGSNT;
		transaction_cache.insertTransaction(broker_message.sequence_number(),new_transaction);

		// UDP doesn't fail
		communicator.send(broker_message,receiver.broker_name());
	    } else // not on this broker and non-clustered mode.
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"5,Internal error! (clustering disabled)");
		communicator.send(failresp);

		transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	    }
	} else // Save message to persistence layer...
	{
	    // ...but only if it's no "meta message" (those are useless if not transmitted in real-time)
	    if ( ! original_webapp_request.get_protobuf().mesg().is_typing() && ! original_webapp_request.get_protobuf().mesg().has_seen() )
	    {
		PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, original_webapp_request.get_protobuf().mesg());

		new_transaction.type = OutstandingType::persistenceMSGSVD;

		try
		{
		    communicator.send(cmd);
		    transaction_cache.insertTransaction(cmd.sequence_number(),new_transaction);
		} catch (libsocket::socket_exception e)
		{
		    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"4,Internal error! (Persistence down)");
		    communicator.send(failresp);

		    transaction_cache.eraseWebappRequest(new_transaction.original_sequence_number);

		    // Do not throw in the loop, otherwise no other messages will be delivered.
// 			throw e;
		}
	    } else
	    {
		WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,true,string(""));

		communicator.send(resp);

		transaction_cache.eraseWebappRequest(new_transaction.original_sequence_number);
	    }
	}
    }

}

/********************************** TIMEOUT FUNCTIONS ************************************/

void ProtocolDispatcher::runTimeoutFinder(void)
{
    std::list<sequence_t> timedout;

    transaction_cache.findTimedout(2,timedout);

    if ( timedout.size() < 1 )
	return;

    for ( sequence_t transaction_id : timedout )
    {
	const OutstandingTransaction& transaction = transaction_cache.lookupTransaction(transaction_id);
	//const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( transaction.type == OutstandingType::messagerelayB2BMSGSNT || transaction.type == OutstandingType::persistenceB2BMSGSVD )
	{
	    const string& origin = transaction_cache.lookupB2BOrigin(transaction_id);
	    MessageForB2B failmsg(transaction.original_sequence_number,false);
	    communicator.send(failmsg,origin);
	    transaction_cache.eraseB2BOrigin(transaction_id);
	}

	// LOGIN-related
	if ( transaction.type == OutstandingType::messagerelayCHANCREAT
	    || transaction.type == OutstandingType::persistenceLoginULKDUP
	    || transaction.type == OutstandingType::persistenceCHKDPASS
	    || transaction.type == OutstandingType::persistenceLGDIN
	    || transaction.type == OutstandingType::persistenceAfterFailedChancreatLogout )
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDIN,false,string(""),"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP // LOGOUT-related
	    || transaction.type == OutstandingType::persistenceLGDOUT )
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDOUT,false,"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceSndmsgULKDUP // SENDMESSAGE-related
	    || transaction.type == OutstandingType::messagerelayMSGSNT
	    || transaction.type == OutstandingType::persistenceMSGSVD
	    || transaction.type == OutstandingType::b2bMSGSNT )
	{
	    // Only send error if the message couldn't be delivered to at least one instance of the receiver.
	    if ( ! (*transaction.remaining_count < transaction.original_count) )
	    {
		WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::SENTMESSAGE,false,"21,Timeout in broker");

		communicator.send(failresp);
	    } else // send success (to not worry the sender :P)
	    {
		WebappResponse partial_resp(transaction.original_sequence_number,WebappResponseMessage::SENTMESSAGE,true,"");

		communicator.send(partial_resp);

	    }
	} else if ( transaction.type == OutstandingType::persistenceSaveSettingsULKDUP // SAVESETTINGS-related
		 || transaction.type == OutstandingType::persistenceSAVEDSETTINGS )
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::SAVEDSETTINGS,false,"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceGetSettingsULKDUP // GETSETTINGS-related
		 || transaction.type == OutstandingType::persistenceGOTSETTINGS )
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::GOTSETTINGS,false,string(""),"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceUREGD ) // REGISTER-related
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::REGISTERED,false,"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP ) // QUERYSTATUS-related
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::USERSTATUS,false,false,"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceIsauthULKDUP ) // AUTHORIZED-related
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::AUTHORIZED,false,false,"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceMessageGetULKDUP // GETMESSAGES-related
		 || transaction.type == OutstandingType::persistenceMSGS )
	{
	    google::protobuf::RepeatedPtrField<chattp::ChattpMessage> dummy_field;
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::GOTMESSAGES,false,dummy_field.begin(),dummy_field.end(),"21,Timeout in broker");

	    communicator.send(failresp);
	} else if ( transaction.type == OutstandingType::persistenceHeartbeated ) // CHANNEL_HEARTBEAT-related
	{
	    WebappResponse failresp(transaction.original_sequence_number,WebappResponseMessage::HEARTBEAT_RECEIVED,false,"21,Timeout in broker");

	    communicator.send(failresp);
	}

	// Free memory
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(transaction_id);

	if ( transaction.remaining_count )
	    delete transaction.remaining_count;
	if ( transaction.saved )
	    delete transaction.saved;
    }
}
