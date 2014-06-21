# include "broker.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"
# include "user_cache.hpp"
# include "conf.hpp"
# include "broker2broker.hpp"
# include "transaction-maps.hpp"

namespace {
    TransactionMap transaction_cache;
    UserCache user_cache;
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
    }
}

void ProtocolDispatcher::handleWebappMessage(shared_ptr<WebappRequest> msg)
{
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
    }
}

void ProtocolDispatcher::handleMessagerelayMessage(shared_ptr<MessageRelayResponse> msg)
{
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

    transaction.type = OutstandingType::messagerelayB2BMSGSNT;
    transaction.original_sequence_number = seqnum;

    if ( ! global_broker_settings.getClusteredMode() ) // Shouldn't happen.
    {
	MessageForB2B failmsg(seqnum,false);

	communicator.send(failmsg,msg.origin_broker);

	return;
    }

    transaction_cache.insertB2BOrigin(seqnum,msg.origin_broker);

    ChattpMessage mesg;
    mesg.set_body("<dummybody>");
    mesg.set_sender("<dummysender>");
    mesg.set_receiver("<dummyreceiver>");
    mesg.set_timestamp("<dummytimestamp>");
    MessageForRelay relaymsg(msg.channel_id(),mesg);

    try
    {
	communicator.send(relaymsg);
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

    WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::SENTMESSAGE,msg.status(),"9,Couldn't deliver message");

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
    transaction_cache.eraseTransaction(seqnum);

    communicator.send(resp);
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

    UserCache::CachedUser cached_user = user_cache.lookupUserInCache(rq.user_name());

    // Can't log-in if already online
    if ( cached_user.found && cached_user.online )
    {
	WebappResponse resp(seqnum,WebappResponseMessage::LOGGEDIN,false,string("")/*no channel id needed*/,"3,User is already online");

	communicator.send(resp);

	return;
    } if ( cached_user.found && ! cached_user.online ) // up next: CHKPASS, then LOGIN
    {
	transaction.type = OutstandingType::persistenceCHKDPASS;

	PersistenceLayerCommand cmd(PersistenceRequest::CHECKPASS,rq.user_name(),rq.password());

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
    } else
    {
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
    }

    transaction_cache.insertTransaction(new_seqnum,transaction);
    transaction_cache.insertWebappRequest(seqnum,rq);
}

void ProtocolDispatcher::onWebAppLOGOUT(const WebappRequest& rq)
{
    OutstandingTransaction transaction;
    sequence_t new_seqnum;
    const sequence_t seqnum = rq.sequence_number();

    UserCache::CachedUser cached_user = user_cache.lookupUserInCache(rq.user_name());

    if ( cached_user.found && (! cached_user.online || cached_user.channel_id != rq.channel_id() || cached_user.broker_name != global_broker_settings.getMessageBrokerName()) ) // Unauthorized/invalid
    {
	string error_string;

	if ( ! cached_user.online )
	    error_string = "1,User is already offline";
	else if ( cached_user.channel_id != rq.channel_id() )
	    error_string = "2,Logout: authentication error (wrong channel id)";
	else if ( cached_user.broker_name != global_broker_settings.getMessageBrokerName() )
	    error_string = "15,Logout on wrong broker.";

	// offline or unauthenticated -- deny!
	WebappResponse resp(seqnum,WebappResponseMessage::LOGGEDOUT,false,error_string);

	communicator.send(resp);

	return;
    } else if ( cached_user.found ) // Authorized
    {
	// Online and authenticated -- logout!
	transaction.type = OutstandingType::persistenceLGDOUT;
	transaction.original_sequence_number = seqnum;

	PersistenceLayerCommand cmd(PersistenceRequest::LOGOUT, rq.user_name());

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number();
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::LOGGEDOUT,false,"4,Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else // not found
    {
	// Do procedure via ULKUP
	transaction.type = OutstandingType::persistenceLogoutULKDUP;
	transaction.original_sequence_number = seqnum;

	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number();
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::LOGGEDOUT,false,"4,Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    }

    transaction_cache.insertTransaction(new_seqnum,transaction);
    transaction_cache.insertWebappRequest(seqnum,rq);

    return;
}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;

    if ( rq.is_group_message() )
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"8,Group messages are not implemented yet");
	communicator.send(failresp);
	return;
    }

    UserCache::CachedUser sender = user_cache.lookupUserInCache(rq.message_sender()), receiver = user_cache.lookupUserInCache(rq.message_receiver());

    if ( ! sender.found ) // We don't have this sender in cache, do normal procedure with two look-ups. Normal in clustered mode.
    {
	transaction.type = OutstandingType::persistenceSndmsgSenderULKDUP;

	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.message_sender());

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

    } else if ( (sender.found && ! receiver.found)  ) // We only have the sender in cache, look up the receiver and send afterwards. Implicit: !clustered_mode
    {
	// Unauthorized!
	if ( ! sender.online || rq.channel_id() != sender.channel_id || sender.broker_name != global_broker_settings.getMessageBrokerName() )
	{
	    WebappResponse resp(seqnum,WebappResponseMessage::SENTMESSAGE,false,sender.online ? "2,Sender unauthorized (wrong channel id/broker)"
											    : "1,Sender is offline");
	    communicator.send(resp);

	    return;
	}

	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;

	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.message_receiver());

	try
	{
	    communicator.send(cmd);

	    messages_processed++;
	    transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}

    } else if ( sender.found && receiver.found ) // We have both users in cache, the receiver is fully in cache. Implicit: !clustered_mode
    {
	// Unauthorized!
	if ( ! sender.online || rq.channel_id() != sender.channel_id )
	{
	    WebappResponse resp(seqnum,WebappResponseMessage::SENTMESSAGE,false,!sender.online ? "Sender offline" : "Sender unauthorized");
	    communicator.send(resp);

	    return;
	}
	// Send to message relay (other implementation is in onPersistenceULKDUP)
	if ( receiver.online && receiver.broker_name == global_broker_settings.getMessageBrokerName() )
	{
	    MessageForRelay msg(receiver.channel_id,rq.get_protobuf().mesg());

	    try
	    {
		communicator.send(msg);

		messages_processed++;
		transaction.type = OutstandingType::messagerelayMSGSNT;
		transaction_cache.insertTransaction(msg.sequence_number(),transaction);
	    } catch (libsocket::socket_exception e)
	    {
		// Message relay is offline (unreachable), therefore save that message.
		PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, rq.get_protobuf().mesg());

		transaction.type = OutstandingType::persistenceMSGSVD;

		try
		{
		    communicator.send(cmd);
		    transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
		} catch (libsocket::socket_exception e)
		{
		    WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"Internal error! (Message relay down, Persistence too)");
		    communicator.send(failresp);

		    transaction_cache.eraseTransaction(seqnum);

		    throw e;
		}
	    }

	} else if ( receiver.online && receiver.broker_name != global_broker_settings.getMessageBrokerName() ) // ...and not on this broker. Implicit: !clustered_mode
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"Internal error! (clustering disabled)");
	    communicator.send(failresp);

	} else if ( ! receiver.online )
	{
	    PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, rq.get_protobuf().mesg());

	    try
	    {
		communicator.send(cmd);

		messages_processed++;
		transaction.type = OutstandingType::persistenceMSGSVD;
		transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(seqnum,WebappResponseMessage::SENTMESSAGE,false,"Internal error! (Persistence down)");
		communicator.send(failresp);

		throw e;
	    }
	}

	transaction_cache.insertWebappRequest(seqnum,rq);
    }
    // Next up: onPersistenceULKDUP (sender) → onPersistenceULKDUP (receiver) → onMessagerelayMSGSNT/onPersistenceMSGSVD
}

void ProtocolDispatcher::onWebAppUONLQ(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    UserCache::CachedUser cache_entry = user_cache.lookupUserInCache(rq.user_name());

    if ( cache_entry.found )
    {
	// non-clustered mode.
	WebappResponse resp(seqnum,WebappResponseMessage::USERSTATUS,true,
			    cache_entry.online && cache_entry.broker_name == global_broker_settings.getMessageBrokerName(),
			    "" /* Always successful */);

	communicator.send(resp);

	return;
    }

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
	WebappResponse failresp(seqnum,WebappResponseMessage::USERSTATUS,false,"Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onWebAppMSGGT(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;

    UserCache::CachedUser user = user_cache.lookupUserInCache(rq.user_name());

    if ( user.found && (user.online && user.channel_id == rq.channel_id()) )
    {
	PersistenceLayerCommand cmd(PersistenceRequest::GETMESSAGES,rq.user_name());

	transaction.type = OutstandingType::persistenceMSGS;

	try
	{
	    communicator.send(cmd);

	    transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch ( libsocket::socket_exception e )
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::GOTMESSAGES,false,"Internal error (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else if ( ! user.found )
    {
	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,rq.user_name());

	transaction.type = OutstandingType::persistenceMessageGetULKDUP;

	try
	{
	    communicator.send(cmd);

	    transaction_cache.insertTransaction(cmd.sequence_number(),transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseMessage::GOTMESSAGES,false,"Internal error (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else if ( user.found && (! user.online || user.channel_id != rq.channel_id()) )
    {
	WebappResponse failresp(seqnum,WebappResponseMessage::GOTMESSAGES,false,user.online ? "Wrong channel id" : "User is offline");

	communicator.send(failresp);
    }
}

void ProtocolDispatcher::onWebAppISAUTH(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number();

    UserCache::CachedUser user = user_cache.lookupUserInCache(rq.user_name());

    if ( !user.found )
    {
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
    } else
    {
	bool auth_status = user.online
			&& user.channel_id == rq.channel_id()
			&& user.broker_name == global_broker_settings.getMessageBrokerName();

	WebappResponse resp(seqnum,WebappResponseMessage::AUTHORIZED,true,auth_status,"" /* Never fails */);

	communicator.send(resp);
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
	// That's this hacky construction again -- the channel_id was not actually in that request!
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

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

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);
	    throw e;
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

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    // We are in the process of sending a message and we just received information on the sender.
    // Is the sender authorized? If yes, ask for the receiver's information and return. If not,
    // respond with an error.
    if ( transaction.type == OutstandingType::persistenceSndmsgSenderULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	if ( !rp.status() || ! loc.online() || loc.channel_id() != original_webapp_request.channel_id() || loc.broker_name() != global_broker_settings.getMessageBrokerName() )
	{
	    string error_message;

	    if ( ! rp.status() )
		error_message = "15,Sender doesn't exist";
	    else if ( ! loc.online() )
		error_message = "1,Sender is offline";
	    else if ( loc.channel_id() != original_webapp_request.channel_id() )
		error_message = "2,Sender unauthorized";

	    // Unauthorized sender!
	    WebappResponse wr(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,error_message);

	    communicator.send(wr);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	    transaction_cache.eraseTransaction(seqnum);

	    return;
	}

	PersistenceLayerCommand cmd(PersistenceRequest::LOOKUP,original_webapp_request.message_receiver());

	try {
	    communicator.send(cmd);

	    // Next message will be a ULKDUP for the receiver
	    transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;
	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse wr(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"4,Internal error! (Persistence down)");
	    communicator.send(wr);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	    transaction_cache.eraseTransaction(seqnum);

	    throw e;
	}

    } else if ( transaction.type == OutstandingType::persistenceSndmsgReceiverULKDUP )
    {
	// send to persistence
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.insertUserInCache(original_webapp_request.message_receiver(),loc.channel_id(),loc.broker_name(),loc.online());

	// Is receiver online? If so, send to message relay, else send to persistence
	if ( loc.online() )
	{
	    if ( loc.broker_name() == global_broker_settings.getMessageBrokerName() ) // User is on this broker?
	    {
		MessageForRelay msg(loc.channel_id(),original_webapp_request.get_protobuf().mesg());
		transaction.type = OutstandingType::messagerelayMSGSNT;

		try
		{
		    communicator.send(msg);
		    transaction_cache.eraseAndInsertTransaction(seqnum,msg.sequence_number(),transaction);
		} catch (libsocket::socket_exception e)
		{
		    // Message relay is offline, therefore save that message.
		    PersistenceLayerCommand cmd(PersistenceRequest::SAVEMESSAGE, original_webapp_request.get_protobuf().mesg());

		    transaction.type = OutstandingType::persistenceMSGSVD;

		    try
		    {
			communicator.send(cmd);
			transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
		    } catch (libsocket::socket_exception e)
		    {
			WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"6,Internal error! (Message relay down, Persistence too)");
			communicator.send(failresp);

			transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
			transaction_cache.eraseTransaction(seqnum);

			throw e;
		    }

		    throw e;
		}
	    } else if ( global_broker_settings.getClusteredMode() ) // B2B communication!
	    {
		MessageForB2B broker_message(original_webapp_request.get_protobuf().mesg(),loc.channel_id());

		transaction.type = OutstandingType::b2bMSGSNT;
		transaction_cache.eraseAndInsertTransaction(seqnum,broker_message.sequence_number(),transaction);

		// UDP doesn't fail
		communicator.send(broker_message,loc.broker_name());
	    } else // not on this broker and non-clustered mode.
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,false,"5,Internal error! (clustering disabled)");
		communicator.send(failresp);

		transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
		transaction_cache.eraseTransaction(seqnum);
	    }
	} else // Save message to persistence layer
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
	}

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	// Respond to the "is user online?" request.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::USERSTATUS,rp.status(),
			    loc.online() &&
			    (global_broker_settings.getClusteredMode() || loc.broker_name() == global_broker_settings.getMessageBrokerName()),
			    "15,User probably doesn't exist"
			);

	user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	transaction_cache.eraseTransaction(seqnum);

	communicator.send(resp);
    } else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( original_webapp_request.type() != chattp::WebappRequestMessage::LOGOUT )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type logOut; however, this is not the case.");

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	// May log off (authenticated).
	if ( loc.online() && loc.channel_id() == original_webapp_request.channel_id() && loc.broker_name() == global_broker_settings.getMessageBrokerName() )
	{
	    // User is authorized to log off.
	    // Now mark user as offline in persistence.
	    PersistenceLayerCommand cmd(PersistenceRequest::LOGOUT,original_webapp_request.user_name());

	    transaction.type = OutstandingType::persistenceLGDOUT;

	    try
	    {
		communicator.send(cmd);
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number(),transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,false,"4,Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	} else
	{
	    string error_message;

	    if ( ! loc.online() )
		error_message = "1,User is already offline";
	    else if ( loc.channel_id() != original_webapp_request.channel_id() )
		error_message = "2,User not authorized to do logout";
	    else if ( ! rp.status() )
		error_message = "15,User doesn't exist/Internal error";

	    // User is not authorized to do a LOGOUT operation.
	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,false,error_message);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(resp);
	}
    } else if ( transaction.type == OutstandingType::persistenceLoginULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	if ( loc.online() || !rp.status() ) // must be offline and registered to log-in
	{
	    string error_message;

	    if ( loc.online() )
		error_message = "3,Already online";
	    else if ( !rp.status() )
		error_message = "15,User doesn't exist/Internal error";

	    WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDIN,false,string(""),error_message);

	    communicator.send(resp);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    return;
	}

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

    } else if ( transaction.type == OutstandingType::persistenceMessageGetULKDUP )
    {
	// Next: send msggt to persistence.

	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	if ( !loc.online() || original_webapp_request.channel_id() != loc.channel_id() )
	{
	    string error_message;

	    if ( ! loc.online() )
		error_message = "1,User is offline";
	    else
		error_message = "2,User not authorized";

	    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::GOTMESSAGES,false,error_message);

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
	    WebappResponse failresp(original_webapp_request.sequence_number(),WebappResponseMessage::GOTMESSAGES,false,"4,Internal error! (Persistence down)");

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(failresp);

	    throw e;
	}

    } else if ( transaction.type == OutstandingType::persistenceIsauthULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( rp.status() )
	{
	    PersistenceResponse::UserLocation loc = rp.get_protobuf().user_locations(0);

	    user_cache.insertUserInCache(original_webapp_request.user_name(),loc.channel_id(),loc.broker_name(),loc.online());

	    bool auth_status = loc.online()
			    && loc.broker_name() == global_broker_settings.getMessageBrokerName()
			    && loc.channel_id() == original_webapp_request.channel_id();

	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::AUTHORIZED,rp.status(),auth_status,"15,User probably doesn't exist");
	    communicator.send(resp);
	} else
	{
	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::AUTHORIZED,rp.status(),false,"15,User probably doesn't exist");

	    communicator.send(resp);
	}

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

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

    if ( transaction.type == OutstandingType::persistenceMSGSVD )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,rp.status(),"14,Internal error! (Persistence didn't accept message)");

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	communicator.send(resp);
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

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDOUT,rp.status(),"11,Unknown error");

	communicator.send(resp);

	// online predicate is checked before broker_name and channel_id; those may be left empty.
	user_cache.insertUserInCache(original_webapp_request.user_name(),string(),string(),false);

	MessageForRelay delchanmsg(original_webapp_request.channel_id(),MessageRelayRequest::DELETECHANNEL);

	transaction.type = OutstandingType::messagerelayDELTDCHAN;
	transaction_cache.eraseAndInsertTransaction(seqnum,delchanmsg.sequence_number(),transaction);

	communicator.send(delchanmsg);
    } else if ( transaction.type == OutstandingType::persistenceAfterFailedChancreatLogout )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::LOGGEDIN,false,string(""),"7,Channel couldn't be created");

	communicator.send(resp);

	transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number());
	transaction_cache.eraseTransaction(seqnum);

    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceLGDOUT: Expected transaction type persistencLGDOUT, but received other.");
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

void ProtocolDispatcher::onMessagerelayMSGSNT(const MessageRelayResponse& rp)
{
    // After onPersistenceULKDUP

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

	if ( rp.status() )
	{
	    WebappResponse resp(original_webapp_request.sequence_number(),WebappResponseMessage::SENTMESSAGE,true,"");

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(resp);
	} else // save message to persistence
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
            user_cache.insertUserInCache(original_webapp_request.user_name(),original_webapp_request.channel_id_,global_broker_settings.getMessageBrokerName(),true);

	    WebappResponse resp(transaction.original_sequence_number,WebappResponseMessage::LOGGEDIN,rp.status(),original_webapp_request.channel_id_,"12,Channel couldn't be created");

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	    communicator.send(resp);
	}
        else
	{
	    // Log out in database, send fail code.
	    PersistenceLayerCommand logout_cmd(PersistenceRequest::LOGOUT,original_webapp_request.user_name());
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
