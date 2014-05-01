# include "broker.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"
# include "cache.hpp"
# include "conf.hpp"
# include "broker2broker.hpp"
# include "transaction-maps.hpp"

namespace {
    TransactionMap transaction_cache;
}

/*
 * LOCKING POLICY
 *
 * For reading operations, the mutex of that map has to be obtained shared, e.g. using a shared_lock object.
 * For writing operations, the lock has to be exclusive and therefore unique_lock has to be used.
 *
 * R/W-references may be taken from the maps and written to without existing lock because there are
 * currently no signs that one transaction object may be used by two threads concurrently (if the other servers
 * are implementing the protocol correctly). The only situtation in which a catastrophe (unrestricted concurrent access
 * to data) could happen is if, for example, Persistence sends two messages with the same sequence number in a very short interval.
 * I that case, two threads might access the same map key; however, it is very unlikely that this will happen.
 *
 */

/*
 * CACHING
 *
 * A user's information is cached whenever there has been a ULKUP which was not triggered by UONLQ (in that case, no information is saved),
 * and LOGIN/LOGOUT operations update that information.
 */

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

    received_messages.resize(3);

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

    switch ( msg->response_type )
    {
	case PersistenceLayerResponseCode::lookedUpUser:
	    onPersistenceULKDUP(*msg);
	    break;
	case PersistenceLayerResponseCode::savedMessage:
	    onPersistenceMSGSVD(*msg);
	    break;
	case PersistenceLayerResponseCode::userRegistered:
	    onPersistenceUREGD(*msg);
	    break;
	case PersistenceLayerResponseCode::passwordChecked:
	    onPersistenceCHKDPASS(*msg);
	    break;
	case PersistenceLayerResponseCode::loggedIn:
	    onPersistenceLGDIN(*msg);
	    break;
	case PersistenceLayerResponseCode::loggedOut:
	    onPersistenceLGDOUT(*msg);
	    break;
	case PersistenceLayerResponseCode::messages:
	    throw BrokerError(ErrorType::unimplemented,"handlePersistenceMessage(): Unimplemented response handler for MSGS.");
    }
}

void ProtocolDispatcher::handleWebappMessage(shared_ptr<WebappRequest> msg)
{
    switch ( msg->request_type )
    {
	case WebappRequestCode::isOnline:
	    onWebAppUONLQ(*msg);
	    break;
	case WebappRequestCode::sendMessage:
	    onWebAppSNDMSG(*msg);
	    break;
	case WebappRequestCode::logIn:
	    onWebAppLOGIN(*msg);
	    break;
	case WebappRequestCode::logOut:
	    onWebAppLOGOUT(*msg);
	    break;
	case WebappRequestCode::registerUser:
	    onWebAppUREG(*msg);
	    break;
    }
}

void ProtocolDispatcher::handleMessagerelayMessage(shared_ptr<MessageRelayResponse> msg)
{
    switch ( msg->response_type )
    {
	case MessageRelayResponseType::messageSent:
	    onMessagerelayMSGSNT(*msg);
	    break;
	case MessageRelayResponseType::channelDeleted:
	    onMessagerelayDELTDCHAN(*msg);
	    break;
	case MessageRelayResponseType::channelCreated:
	    onMessagerelayCHANCREAT(*msg);
	    break;
    }
}

void ProtocolDispatcher::handleBrokerMessage(shared_ptr<B2BIncoming> msg)
{
    switch ( msg->type )
    {
	case B2BMessageType::B2BSNDMSG:
	    onB2BSNDMSG(*msg);
	    break;
	case B2BMessageType::B2BMSGSNT:
	    onB2BMSGSNT(*msg);
	    break;
    }
}

/***************************** Event handlers ******************************/

void ProtocolDispatcher::onB2BSNDMSG(const B2BIncoming& msg)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::messagerelayB2BMSGSNT;
    transaction.original_sequence_number = msg.sequence_number;

    transaction_cache.insertB2BOrigin(msg.sequence_number,msg.origin_broker);

    MessageForRelay relaymsg(msg.sender_username,msg.message,msg.channel_id);

    try
    {
	communicator.send(relaymsg);
	transaction_cache.insertTransaction(relaymsg.seq_num,transaction);
    } catch (libsocket::socket_exception e)
    {
	MessageForB2B failmsg(msg.sequence_number,false);

	// This one should work because we received this message!
	communicator.send(failmsg,msg.origin_broker);
	throw e;
    }
}

void ProtocolDispatcher::onB2BMSGSNT(const B2BIncoming& msg)
{
    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(msg.sequence_number);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction (B2B MSGSNT)");

	transaction_cache.eraseTransaction(msg.sequence_number);

	return;
    }

    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::acceptedMessage,msg.status,"Couldn't deliver message");

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(msg.sequence_number);

    communicator.send(resp);
}

void ProtocolDispatcher::onWebAppUREG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUREGD;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::registerUser,rq.user,rq.password);

    try
    {
	communicator.send(cmd);
	transaction_cache.insertTransaction(cmd.sequence_number,transaction);
	transaction_cache.insertWebappRequest(rq.sequence_number,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(rq.sequence_number,WebappResponseCode::registeredUser,false,"Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    const sequence_t seqnum = rq.sequence_number;
    sequence_t new_seqnum;

    OutstandingTransaction transaction;
    transaction.original_sequence_number = seqnum;

    CachedUser cached_user = lookupUserInCache(rq.user);

    // Can't log-in if already online
    if ( cached_user.found && cached_user.online )
    {
	WebappResponse resp(seqnum,WebappResponseCode::loggedIn,false,"User is already online");

	communicator.send(resp);

	return;
    } if ( cached_user.found && ! cached_user.online ) // up next: CHKPASS, then LOGIN
    {
	transaction.type = OutstandingType::persistenceCHKDPASS;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,rq.user,rq.password);

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number;
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(rq.sequence_number,WebappResponseCode::loggedIn,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else
    {
	// Next step is ULKUP, then CHKPASS, then LOGIN
	transaction.type = OutstandingType::persistenceLoginULKDUP;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number;
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(rq.sequence_number,WebappResponseCode::loggedIn,false,"Internal error! (Persistence down)");
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

    CachedUser cached_user = lookupUserInCache(rq.user);

    if ( cached_user.found && (! cached_user.online || cached_user.channel_id != rq.channel_id || cached_user.broker_name != global_broker_settings.getMessageBrokerName()) ) // Unauthorized/invalid
    {
	// offline or unauthenticated -- deny!
	WebappResponse resp(rq.sequence_number,WebappResponseCode::loggedOut,false,
			    !cached_user.online ? "User is already offline" : "Logout: authentication error");

	communicator.send(resp);

	return;
    } else if ( cached_user.found ) // Authorized
    {
	// Online and authenticated -- logout!
	transaction.type = OutstandingType::persistenceLGDOUT;
	transaction.original_sequence_number = rq.sequence_number;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut, rq.user);

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number;
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(rq.sequence_number,WebappResponseCode::loggedOut,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    } else // not found
    {
	// Do procedure via ULKUP
	transaction.type = OutstandingType::persistenceLogoutULKDUP;
	transaction.original_sequence_number = rq.sequence_number;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

	try
	{
	    communicator.send(cmd);

	    new_seqnum = cmd.sequence_number;
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(rq.sequence_number,WebappResponseCode::loggedOut,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}
    }

    transaction_cache.insertTransaction(new_seqnum,transaction);
    transaction_cache.insertWebappRequest(rq.sequence_number,rq);

}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    sequence_t seqnum = rq.sequence_number;

    OutstandingTransaction transaction;

    transaction.original_sequence_number = seqnum;

    CachedUser sender = lookupUserInCache(rq.user), receiver = lookupUserInCache(rq.dest_user);

    if ( ! sender.found ) // We don't have this sender in cache, do normal procedure with two look-ups.
    {
	transaction.type = OutstandingType::persistenceSndmsgSenderULKDUP;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

	try
	{
	    communicator.send(cmd);

	    transaction_cache.insertTransaction(cmd.sequence_number,transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(rq.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}

    } else if ( (sender.found && ! receiver.found)  ) // We only have the sender in cache, look up the receiver and send afterwards.
    {
	// Unauthorized!
	if ( ! sender.online || (rq.channel_id != sender.channel_id) || sender.broker_name != global_broker_settings.getMessageBrokerName() )
	{
	    WebappResponse resp(seqnum,WebappResponseCode::acceptedMessage,false,sender.online ? "Sender unauthorized (wrong channel id)"
											    : "Sender is offline");
	    communicator.send(resp);

	    return;
	}

	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.dest_user);

	try
	{
	    communicator.send(cmd);

	    transaction_cache.insertTransaction(cmd.sequence_number,transaction);
	    transaction_cache.insertWebappRequest(seqnum,rq);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(seqnum,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
	    communicator.send(failresp);

	    throw e;
	}

    } else if ( sender.found && receiver.found ) // We have both users in cache, the receiver is fully in cache.
    {
	// Unauthorized!
	if ( ! sender.online || rq.channel_id != sender.channel_id )
	{
	    WebappResponse resp(seqnum,WebappResponseCode::acceptedMessage,false,!sender.online ? "Sender offline" : "Sender unauthorized");
	    communicator.send(resp);

	    return;
	}
	// Send to message relay (other implementation is in onPersistenceULKDUP)
	if ( receiver.online && receiver.broker_name == global_broker_settings.getMessageBrokerName() )
	{
	    MessageForRelay msg(rq.user,rq.message, receiver.channel_id);

	    try
	    {
		communicator.send(msg);

		transaction.type = OutstandingType::messagerelayMSGSNT;
		transaction_cache.insertTransaction(msg.seq_num,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		// Message relay is offline (unreachable), therefore save that message.
		PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, rq.dest_user, rq.message);

		transaction.type = OutstandingType::persistenceMSGSVD;

		try
		{
		    communicator.send(cmd);
		    transaction_cache.insertTransaction(cmd.sequence_number,transaction);
		} catch (libsocket::socket_exception e)
		{
		    WebappResponse failresp(rq.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Message relay down, Persistence too)");
		    communicator.send(failresp);

		    transaction_cache.eraseTransaction(seqnum);

		    throw e;
		}
	    }

	} else if ( receiver.online ) // ...and not on this broker
	{
	    MessageForB2B broker_message(rq.user,rq.message,receiver.channel_id);

	    try
	    {
		communicator.send(broker_message,receiver.broker_name);

		transaction.type = OutstandingType::b2bMSGSNT;
		transaction_cache.insertTransaction(broker_message.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(rq.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (B2B failed)");
		communicator.send(failresp);

		throw e;
	    }

	} else if ( ! receiver.online )
	{
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, rq.dest_user, rq.message);

	    try
	    {
		communicator.send(cmd);

		transaction.type = OutstandingType::persistenceMSGSVD;
		transaction_cache.insertTransaction(cmd.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(rq.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
		communicator.send(failresp);

		throw e;
	    }

	}

	transaction_cache.insertWebappRequest(rq.sequence_number,rq);
    }
    // Next up: onPersistenceULKDUP (sender) → onPersistenceULKDUP (receiver) → onMessagerelayMSGSNT/onPersistenceMSGSVD
}

void ProtocolDispatcher::onWebAppUONLQ(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUonlqULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    CachedUser cache_entry = lookupUserInCache(rq.user);

    if ( cache_entry.found )
    {
	WebappResponse resp(rq.sequence_number,WebappResponseCode::isOnline,cache_entry.online);

	communicator.send(resp);

	return;
    }

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    try
    {
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number,transaction);
	transaction_cache.insertWebappRequest(rq.sequence_number,rq);
    } catch (libsocket::socket_exception e)
    {
	WebappResponse failresp(rq.sequence_number,WebappResponseCode::isOnline,false,"Internal error! (Persistence down)");
	communicator.send(failresp);

	throw e;
    }
}

void ProtocolDispatcher::onPersistenceUREGD(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type == OutstandingType::persistenceUREGD )
    {
	WebappResponse resp(original_webapp_request.sequence_number, WebappResponseCode::registeredUser, rp.status,"Probably, this user already exists");

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

    sequence_t seqnum = rp.sequence_number;

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
	if ( rp.status == false )
	{
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false,"Wrong password!");

	    transaction_cache.eraseTransaction(seqnum);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number);

	    communicator.send(resp);
	} else
	{
	    channel_id_t channel_id = generateChannelId();
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logIn,original_webapp_request.user,global_broker_settings.getMessageBrokerName(),channel_id);

	    /* Here, we're doing something which is not very clean: We save the channel id in the "original" webapp request
	    * although that didn't actually bear a channel id. However, this is necessary so the onPersistenceLGDIN() handler
	    * may reply with a sequence number.
	    */
	    original_webapp_request.channel_id = channel_id;

	    try
	    {
		communicator.send(cmd);

		transaction.type = OutstandingType::persistenceLGDIN;
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false,"Internal error! (Persistence down)");
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
    sequence_t seqnum = rp.sequence_number;

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

	MessageForRelay newchanmsg(original_webapp_request.channel_id,MessageForRelayType::createChannel);

	try
	{
	    communicator.send(newchanmsg);

	    transaction.type = OutstandingType::messagerelayCHANCREAT;
	    transaction_cache.eraseAndInsertTransaction(seqnum,newchanmsg.seq_num,transaction);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false,"Internal error! (Channel registration failed)");
	    communicator.send(failresp);

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

    sequence_t seqnum = rp.sequence_number;

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

	insertUserInCache(original_webapp_request.user,rp.channel_id,rp.broker_name,rp.online);

	if ( ! rp.online || rp.channel_id != original_webapp_request.channel_id || rp.broker_name != global_broker_settings.getMessageBrokerName() )
	{
	    // Unauthorized sender!
	    WebappResponse wr(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false,rp.online ? "Sender unauthorized (wrong channel id)"
															  : "Sender is offline");

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number);
	    transaction_cache.eraseTransaction(seqnum);
	    communicator.send(wr);

	    return;
	}

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,original_webapp_request.dest_user);

	try {
	    communicator.send(cmd);

	    // Next message will be a ULKDUP for the receiver
	    transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;
	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse wr(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
	    communicator.send(wr);

	    transaction_cache.eraseTransaction(seqnum);

	    throw e;
	}

    } else if ( transaction.type == OutstandingType::persistenceSndmsgReceiverULKDUP )
    {
	// send to persistence
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	insertUserInCache(original_webapp_request.dest_user,rp.channel_id,rp.broker_name,rp.online);

	// Is receiver online? If so, send to message relay, else send to persistence
	if ( rp.online )
	{
	    if ( rp.broker_name == global_broker_settings.getMessageBrokerName() ) // User is on this broker?
	    {
		MessageForRelay msg(original_webapp_request.user,original_webapp_request.message, rp.channel_id);

		transaction.type = OutstandingType::messagerelayMSGSNT;

		try
		{
		    communicator.send(msg);
		    transaction_cache.eraseAndInsertTransaction(seqnum,msg.seq_num,transaction);
		} catch (libsocket::socket_exception e)
		{
		    // Message relay is offline, therefore save that message.
		    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

		    transaction.type = OutstandingType::persistenceMSGSVD;

		    try
		    {
			communicator.send(cmd);
			transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
		    } catch (libsocket::socket_exception e)
		    {
			WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Message relay down, Persistence too)");
			communicator.send(failresp);

			transaction_cache.eraseTransaction(seqnum);

			throw e;
		    }

		    throw e;
		}
	    } else // B2B communication!
	    {
		MessageForB2B broker_message(original_webapp_request.user,original_webapp_request.message,rp.channel_id);

		transaction.type = OutstandingType::b2bMSGSNT;
		transaction_cache.eraseAndInsertTransaction(seqnum,broker_message.sequence_number,transaction);

		// UDP doesn't fail
		communicator.send(broker_message,rp.broker_name);
	    }

	} else // Save message to persistence layer
	{
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;

	    try
	    {
		communicator.send(cmd);
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	}

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	// Respond to the "is user online?" request.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::isOnline,rp.online);

	insertUserInCache(original_webapp_request.user,rp.channel_id,rp.broker_name,rp.online);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	transaction_cache.eraseTransaction(seqnum);

	communicator.send(resp);
    } else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( original_webapp_request.request_type != WebappRequestCode::logOut )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type logOut; however, this is not the case.");

	insertUserInCache(original_webapp_request.user,rp.channel_id,rp.broker_name,rp.online);

	// May log off (authenticated).
	if ( rp.online && rp.channel_id == original_webapp_request.channel_id && rp.broker_name == global_broker_settings.getMessageBrokerName() )
	{
	    // User is authorized to log off.
	    // Now mark user as offline in persistence.
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut,original_webapp_request.user);

	    transaction.type = OutstandingType::persistenceLGDOUT;

	    try
	    {
		communicator.send(cmd);
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,false,"Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	} else
	{
	    // User is not authorized to do a LOGOUT operation.
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,false,
				!rp.online ? "User is already offline" : "User unauthorized to do logout");

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(resp);
	}
    } else if ( transaction.type == OutstandingType::persistenceLoginULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( rp.online || !rp.status ) // must be offline and registered to log-in
	{
	    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,false,
				rp.online ? "User is already online" : "User not known");

	    communicator.send(resp);
	    return;
	}

	transaction.type = OutstandingType::persistenceCHKDPASS;
	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,original_webapp_request.user,original_webapp_request.password);

	try
	{
	    communicator.send(cmd);
	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	} catch (libsocket::socket_exception e)
	{
	    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,false,"Internal error! (Persistence down)");
	    communicator.send(resp);

	    transaction_cache.eraseTransaction(seqnum);

	    throw e;
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
    sequence_t seqnum = rp.sequence_number;

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

	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status,"Internal error! (Persistence didn't accept message)");

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
    sequence_t seqnum = rp.sequence_number;

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

	// We send "OK" back regardless of what the message relay returned because it's only a question of keeping nginx tidy.
	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,true);

	communicator.send(resp);

	// online predicate is checked before broker_name and channel_id; those may be left empty.
	insertUserInCache(original_webapp_request.user,string(),string(),false);

	MessageForRelay delchanmsg(original_webapp_request.channel_id,MessageForRelayType::deleteChannel);

	transaction.type = OutstandingType::messagerelayDELTDCHAN;
	transaction_cache.eraseAndInsertTransaction(seqnum,delchanmsg.seq_num,transaction);

	communicator.send(delchanmsg);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceLGDOUT: Expected transaction type persistencLGDOUT, but received other.");
    }

}

void ProtocolDispatcher::onPersistenceMSGS(const PersistenceLayerResponse& rp)
{
    // There is no GETMSGS command in the webapp protocol yet, therefore this empty handler.
}

void ProtocolDispatcher::onMessagerelayMSGSNT(const MessageRelayResponse& rp)
{
    // After onPersistenceULKDUP

    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transaction_cache.lookupTransaction(seqnum);

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference. (message relay)");

	transaction_cache.eraseTransaction(seqnum);

	return;
    }

    if ( transaction.type == OutstandingType::messagerelayMSGSNT )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( rp.status )
	{
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,true);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    communicator.send(resp);
	} else // save message to persistence
	{
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;

	    try
	    {
		communicator.send(cmd);
		transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);
	    } catch (libsocket::socket_exception e)
	    {
		WebappResponse failresp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false,"Internal error! (Persistence down)");
		communicator.send(failresp);

		transaction_cache.eraseTransaction(seqnum);

		throw e;
	    }
	}
    } else if ( transaction.type == OutstandingType::messagerelayB2BMSGSNT )
    {
	const string& message_sender_broker = transaction_cache.lookupB2BOrigin(transaction.original_sequence_number);

	MessageForB2B mesg(transaction.original_sequence_number,rp.status);

	transaction_cache.eraseB2BOrigin(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	// UDP doesn't fail.
	communicator.send(mesg,message_sender_broker);
    } else
    {
	transaction_cache.eraseTransaction(seqnum);
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayMSGSNT: Expected transaction type messagerelayMSGSNT or messagerelayB2BMSGSNT, but received other.");
    }
}

void ProtocolDispatcher::onMessagerelayDELTDCHAN(const MessageRelayResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

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
    sequence_t seqnum = rp.sequence_number;

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

	WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,rp.status,"Channel could not be created.",original_webapp_request.channel_id);

        if ( rp.status )
            insertUserInCache(original_webapp_request.user,original_webapp_request.channel_id,global_broker_settings.getMessageBrokerName(),true);
        else
            insertUserInCache(original_webapp_request.user,string(),string(),false); // Not logged-in, but obviously existent

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	communicator.send(resp);
    } else
    {
	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);
	transaction_cache.eraseTransaction(seqnum);

	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayCHANCREAT: Expected transaction type messagerelayCHANCREAT, but received other.");
    }
}
