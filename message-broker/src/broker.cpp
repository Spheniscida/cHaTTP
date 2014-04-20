# include "broker.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"
# include "cache.hpp"
# include "conf.hpp"
# include "broker2broker.hpp"
# include "transaction-maps.hpp"

TransactionMap transaction_cache;

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
	    switch ( received_messages[i]->sender )
	    {
		// shared_ptr -- because the delivered message is dynamically allocated.
		case MessageOrigin::fromPersistence:
		    handlePersistenceMessage(shared_ptr<Receivable>(received_messages[i]));
		    break;
		case MessageOrigin::fromWebApp:
		    handleWebappMessage(shared_ptr<Receivable>(received_messages[i]));
		    break;
		case MessageOrigin::fromMessageRelay:
		    handleMessagerelayMessage(shared_ptr<Receivable>(received_messages[i]));
		    break;
		case MessageOrigin::fromBroker:
		    handleBrokerMessage(shared_ptr<Receivable>(received_messages[i]));
		    break;
		default:
		    throw BrokerError(ErrorType::unimplemented,"dispatch(): Unimplemented origin.");
	    }
	    // Shared pointers are destroyed automatically until control is here.
	}
    }
}
/***************************** Message handlers ****************************/

void ProtocolDispatcher::handlePersistenceMessage(shared_ptr<Receivable> msg)
{
    shared_ptr<PersistenceLayerResponse> response = dynamic_pointer_cast<PersistenceLayerResponse>(msg);

    if ( ! response )
	throw BrokerError(ErrorType::genericImplementationError,"handlePersistenceMessage(): Expected type PersistenceLayerResponse, but dynamic_cast failed.");

    switch ( response->response_type )
    {
	case PersistenceLayerResponseCode::lookedUpUser:
	    onPersistenceULKDUP(*response);
	    break;
	case PersistenceLayerResponseCode::savedMessage:
	    onPersistenceMSGSVD(*response);
	    break;
	case PersistenceLayerResponseCode::userRegistered:
	    onPersistenceUREGD(*response);
	    break;
	case PersistenceLayerResponseCode::passwordChecked:
	    onPersistenceCHKDPASS(*response);
	    break;
	case PersistenceLayerResponseCode::loggedIn:
	    onPersistenceLGDIN(*response);
	    break;
	case PersistenceLayerResponseCode::loggedOut:
	    onPersistenceLGDOUT(*response);
	    break;
	case PersistenceLayerResponseCode::messages:
	    throw BrokerError(ErrorType::unimplemented,"handlePersistenceMessage(): Unimplemented response handler for MSGS.");
    }
}

void ProtocolDispatcher::handleWebappMessage(shared_ptr<Receivable> msg)
{
    shared_ptr<WebappRequest> request = dynamic_pointer_cast<WebappRequest>(msg);

    if ( ! request )
	throw BrokerError(ErrorType::genericImplementationError,"handleWebappMessage(): Expected type WebappRequest, but dynamic_cast failed.");

    switch ( request->request_type )
    {
	case WebappRequestCode::isOnline:
	    onWebAppUONLQ(*request);
	    break;
	case WebappRequestCode::sendMessage:
	    onWebAppSNDMSG(*request);
	    break;
	case WebappRequestCode::logIn:
	    onWebAppLOGIN(*request);
	    break;
	case WebappRequestCode::logOut:
	    onWebAppLOGOUT(*request);
	    break;
	case WebappRequestCode::registerUser:
	    onWebAppUREG(*request);
	    break;
    }
}

void ProtocolDispatcher::handleMessagerelayMessage(shared_ptr<Receivable> msg)
{
    shared_ptr<MessageRelayResponse> response = dynamic_pointer_cast<MessageRelayResponse>(msg);

    if ( ! response )
	throw BrokerError(ErrorType::genericImplementationError,"handleMessagerelayMessage(): Expected type MessageRelayResponse, but dynamic_cast failed.");

    // only one type.
    onMessagerelayMSGSNT(*response);
}

void ProtocolDispatcher::handleBrokerMessage(shared_ptr<Receivable> msg)
{
    shared_ptr<B2BIncoming> message = dynamic_pointer_cast<B2BIncoming>(msg);

    switch ( message->type )
    {
	case B2BMessageType::B2BSNDMSG:
	    onB2BSNDMSG(*message);
	    break;
	case B2BMessageType::B2BMSGSNT:
	    onB2BMSGSNT(*message);
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

    MessageForRelay relaymsg(msg.message,msg.channel_id);

    transaction_cache.insertTransaction(relaymsg.seq_num,transaction);

    communicator.send(relaymsg);
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

    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::acceptedMessage,msg.status);

    transaction_cache.eraseB2BOrigin(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(msg.sequence_number);

    communicator.send(resp);
}

void ProtocolDispatcher::onWebAppUREG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUREGD;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::registerUser,rq.user,rq.password);

    transaction_cache.insertTransaction(cmd.sequence_number,transaction);

    transaction_cache.insertWebappRequest(rq.sequence_number,rq);

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    CachedUser cache_entry = lookupUserInCache(rq.user);

    // Can't log-in twice.
    if ( cache_entry.found && cache_entry.online )
    {
	WebappResponse resp(rq.sequence_number,WebappResponseCode::loggedIn,false);

	communicator.send(resp);

	return;
    }

    // Next step is ULKDUP, then CHKDPASS, then LGDIN
    transaction.type = OutstandingType::persistenceLoginULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    transaction_cache.insertTransaction(cmd.sequence_number,transaction);

    transaction_cache.insertWebappRequest(rq.sequence_number,rq);

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGOUT(const WebappRequest& rq)
{
    OutstandingTransaction transaction;
    sequence_t new_seqnum;

    CachedUser cache_entry = lookupUserInCache(rq.user);

    if ( cache_entry.found && (! cache_entry.online || cache_entry.channel_id != rq.channel_id ) ) // Unauthorized/invalid
    {
	// offline or unauthenticated -- deny!
	WebappResponse resp(rq.sequence_number,WebappResponseCode::loggedOut,false);

	communicator.send(resp);

	return;
    } else if ( cache_entry.found && cache_entry.online && cache_entry.channel_id == rq.channel_id ) // Authorized
    {
	// Online and authenticated -- logout!
	transaction.type = OutstandingType::persistenceLGDOUT;
	transaction.original_sequence_number = rq.sequence_number;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut, rq.user);
	communicator.send(cmd);

	new_seqnum = cmd.sequence_number;
    } else // not found
    {
	// Do procedure via ULKUP
	transaction.type = OutstandingType::persistenceLogoutULKDUP;
	transaction.original_sequence_number = rq.sequence_number;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);
	communicator.send(cmd);

	new_seqnum = cmd.sequence_number;
    }

    transaction_cache.insertTransaction(new_seqnum,transaction);
    transaction_cache.insertWebappRequest(rq.sequence_number,rq);

}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.original_sequence_number = rq.sequence_number;

    CachedUser sender = lookupUserInCache(rq.user), receiver = lookupUserInCache(rq.dest_user);

    if ( ! sender.found ) // We don't have this sender in cache, do normal procedure with two look-ups.
    {
	transaction.type = OutstandingType::persistenceSndmsgSenderULKDUP;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number,transaction);

	transaction_cache.insertWebappRequest(rq.sequence_number,rq);

    } else if ( (sender.found && ! receiver.found)  ) // We only have the sender in cache, look up the receiver and send afterwards.
    {
	// Unauthorized!
	if ( ! sender.online || (rq.channel_id != sender.channel_id) )
	{
	    WebappResponse resp(rq.sequence_number,WebappResponseCode::acceptedMessage,false);
	    communicator.send(resp);

	    return;
	}

	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.dest_user);
	communicator.send(cmd);

	transaction_cache.insertTransaction(cmd.sequence_number,transaction);

	transaction_cache.insertWebappRequest(rq.sequence_number,rq);

    } else if ( sender.found && receiver.found ) // We have both users in cache, the receiver is fully in cache.
    {
	// Unauthorized!
	if ( ! sender.online || rq.channel_id != sender.channel_id )
	{
	    WebappResponse resp(rq.sequence_number,WebappResponseCode::acceptedMessage,false);
	    communicator.send(resp);

	    return;
	}
	// Send to message relay (other implementation is in onPersistenceULKDUP)
	if ( receiver.online && receiver.broker_name == global_broker_settings.getMessageBrokerName() )
	{
	    MessageForRelay msg(rq.message, receiver.channel_id);

	    transaction.type = OutstandingType::messagerelayMSGSNT;

	    transaction_cache.insertTransaction(msg.seq_num,transaction);

	    communicator.send(msg);
	} else if ( receiver.online ) // ...and not on this broker
	{
	    MessageForB2B broker_message(rq.message,receiver.channel_id);

	    transaction.type = OutstandingType::b2bMSGSNT;

	    transaction_cache.insertTransaction(broker_message.sequence_number,transaction);

	    communicator.send(broker_message,receiver.broker_name);
	} else if ( ! receiver.online )
	{
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, rq.dest_user, rq.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;

	    transaction_cache.insertTransaction(cmd.sequence_number,transaction);

	    communicator.send(cmd);
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

    transaction_cache.insertTransaction(cmd.sequence_number,transaction);

    transaction_cache.insertWebappRequest(rq.sequence_number,rq);

    communicator.send(cmd);
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

    WebappResponse resp(original_webapp_request.sequence_number, WebappResponseCode::registeredUser, rp.status);

    communicator.send(resp);

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(seqnum);
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

    if ( transaction.type != OutstandingType::persistenceCHKDPASS )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceCHKDPASS: Expected transaction.type to be persistenceCHKDPASS.");

    WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    // Password/user incorrect?
    if ( rp.status == false )
    {
	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false);

	communicator.send(resp);

	transaction_cache.eraseTransaction(seqnum);

	transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number);
    } else
    {
	channel_id_t channel_id = generateChannelId();
	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logIn,original_webapp_request.user,global_broker_settings.getMessageBrokerName(),channel_id);

	/* Here, we're doing something which is not very clean: We save the channel id in the "original" webapp request
	 * although that didn't actually bear a channel id. However, this is necessary so the onPersistenceLGDIN() handler
	 * may reply with a sequence number.
	 */
	original_webapp_request.channel_id = channel_id;

	transaction.type = OutstandingType::persistenceLGDIN;

	transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);

	communicator.send(cmd);
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

    // That's this hacky construction again -- the channel_id was not actually in that request!
    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,rp.status,original_webapp_request.channel_id);

    // We receive the LOGIN, so the user's on this broker. →→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→→↓
    insertUserInCache(original_webapp_request.user,original_webapp_request.channel_id,global_broker_settings.getMessageBrokerName(),true);

    communicator.send(resp);

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(seqnum);
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

	if ( (! rp.online) || rp.channel_id != original_webapp_request.channel_id || rp.broker_name != global_broker_settings.getMessageBrokerName() )
	{
	    // Unauthorized sender!
	    WebappResponse wr(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false);
	    communicator.send(wr);

	    transaction_cache.eraseWebappRequest(original_webapp_request.sequence_number);
	    transaction_cache.eraseTransaction(seqnum);

	    return;
	}

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,original_webapp_request.dest_user);

	// Next message will be a ULKDUP for the receiver
	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;

	transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);

	communicator.send(cmd);

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
		MessageForRelay msg(original_webapp_request.message, rp.channel_id);

		transaction.type = OutstandingType::messagerelayMSGSNT;

		transaction_cache.eraseAndInsertTransaction(seqnum,msg.seq_num,transaction);

		communicator.send(msg);
	    } else // B2B communication!
	    {
		MessageForB2B broker_message(original_webapp_request.message,rp.channel_id);

		transaction.type = OutstandingType::b2bMSGSNT;

		transaction_cache.eraseAndInsertTransaction(seqnum,broker_message.sequence_number,transaction);

		communicator.send(broker_message,rp.broker_name);
	    }

	} else // Save message to persistence layer
	{
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;

	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);

	    communicator.send(cmd);
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
	if ( rp.online == true && rp.channel_id == original_webapp_request.channel_id && rp.broker_name == global_broker_settings.getMessageBrokerName() )
	{
	    // User is authorized to log off.
	    // Now mark user as offline in persistence.
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut,original_webapp_request.user);

	    transaction.type = OutstandingType::persistenceLGDOUT;

	    transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);

	    communicator.send(cmd);
	} else
	{
	    // User is not authorized to do a LOGOUT operation.
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,false);

	    communicator.send(resp);

	    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

	    transaction_cache.eraseTransaction(seqnum);
	}
    } else if ( transaction.type == OutstandingType::persistenceLoginULKDUP )
    {
	const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

	if ( rp.online || !rp.status ) // must be offline and registered to log-in
	{
	    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,false);

	    communicator.send(resp);
	    return;
	}

	transaction.type = OutstandingType::persistenceCHKDPASS;
	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,original_webapp_request.user,original_webapp_request.password);

	communicator.send(cmd);

	transaction_cache.eraseAndInsertTransaction(seqnum,cmd.sequence_number,transaction);

    } else
	throw BrokerError(ErrorType::genericImplementationError,"Unhandled transaction type in onPersistenceULKDUP.");

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

    if ( transaction.type != OutstandingType::persistenceMSGSVD )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGSVD: Expected transaction type to be persistenceMSGSVD, but received other.");

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

    communicator.send(resp);

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(seqnum);
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

    const WebappRequest& original_webapp_request = transaction_cache.lookupWebappRequest(transaction.original_sequence_number);

    // online predicate is checked before broker_name and channel_id; those are left empty.
    insertUserInCache(original_webapp_request.user,string(),string(),false);

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,true);

    communicator.send(resp);

    transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    transaction_cache.eraseTransaction(seqnum);
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

	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

	communicator.send(resp);

	transaction_cache.eraseWebappRequest(transaction.original_sequence_number);

    } else if ( transaction.type == OutstandingType::messagerelayB2BMSGSNT ) // The last SNDMSG was triggered by an external message.
    {
	const string& message_sender_broker = transaction_cache.lookupB2BOrigin(transaction.original_sequence_number);

	MessageForB2B mesg(transaction.original_sequence_number,rp.status);

	communicator.send(mesg,message_sender_broker);

	transaction_cache.eraseB2BOrigin(transaction.original_sequence_number);
    }

    transaction_cache.eraseTransaction(seqnum);
}
