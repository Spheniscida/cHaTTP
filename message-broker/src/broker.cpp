# include "broker.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"

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
 * In that case, two threads might access the same map key; however, it is very unlikely that this will happen.
 *
 */

unordered_map<sequence_t,OutstandingTransaction> transactions;
shared_mutex transactions_mutex;

unordered_map<sequence_t,WebappRequest> webapp_requests;
shared_mutex webapp_requests_mutex;

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
    unsigned int received_size = 0, i = 0;

    while ( true )
    {
	received_messages = communicator.receiveMessages();
	received_size = received_messages.size();

	for ( i = 0; i < received_size; i++ )
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

/***************************** Event handlers ******************************/

void ProtocolDispatcher::onWebAppUREG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUREGD;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::registerUser,rq.user,rq.password);

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	transactions[cmd.sequence_number] = transaction;
    ta_lck.unlock();

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	webapp_requests[rq.sequence_number] = rq;
    wa_lck.unlock();

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    // Next packet for this transaction is CHKDPASS.
    transaction.type = OutstandingType::persistenceCHKDPASS;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,rq.user,rq.password);

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	transactions[cmd.sequence_number] = transaction;
    ta_lck.unlock();

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	webapp_requests[rq.sequence_number] = rq;
    wa_lck.unlock();

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGOUT(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceLogoutULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	transactions[cmd.sequence_number] = transaction;
    ta_lck.unlock();

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	webapp_requests[rq.sequence_number] = rq;
    wa_lck.unlock();

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.original_sequence_number = rq.sequence_number;
    transaction.type = OutstandingType::persistenceSndmsgSenderULKDUP;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	transactions[cmd.sequence_number] = transaction;
    ta_lck.unlock();

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	webapp_requests[rq.sequence_number] = rq;
    wa_lck.unlock();

    communicator.send(cmd);
    // Next up: onPersistenceULKDUP (sender) → onPersistenceULKDUP (receiver) → onMessagerelayMSGSNT/onPersistenceMSGSVD
}

void ProtocolDispatcher::onWebAppUONLQ(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUonlqULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	transactions[cmd.sequence_number] = transaction;
    ta_lck.unlock();

    // We only need the sequence number later (saved in original_sequence_number), therefore no webapp_requests access.

    communicator.send(cmd);
}

void ProtocolDispatcher::onPersistenceUREGD(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    shared_lock<shared_mutex> wa_lck(transactions_mutex);
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    wa_lck.unlock();

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    WebappResponse resp(original_webapp_request.sequence_number, WebappResponseCode::registeredUser, rp.status);

    communicator.send(resp);

    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	webapp_requests.erase(transaction.original_sequence_number);
    wa_wr_lck.unlock();

    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	transactions.erase(seqnum);
    ta_wr_lck.unlock();

}

void ProtocolDispatcher::onPersistenceCHKDPASS(const PersistenceLayerResponse& rp)
{
    // This is called on a response to a request made by onWebAppLOGIN.

    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type != OutstandingType::persistenceCHKDPASS )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceCHKDPASS: Expected transaction.type to be persistenceCHKDPASS.");

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	// No const for modifying channel_id.
	WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    wa_lck.unlock();

    // Password/user incorrect?
    if ( rp.status == false )
    {
	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false);

	communicator.send(resp);

	unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	    transactions.erase(seqnum);
	ta_wr_lck.unlock();

	unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	    webapp_requests.erase(original_webapp_request.sequence_number);
	wa_wr_lck.unlock();
    } else
    {
	channel_id_t channel_id = generateChannelId();
	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logIn,original_webapp_request.user,message_broker_name,channel_id);

	/* Here, we're doing something which is not very clean: We save the channel id in the "original" webapp request
	 * although that didn't actually bear a channel id. However, this is necessary so the onPersistenceLGDIN() handler
	 * may reply with a sequence number.
	 */
	original_webapp_request.channel_id = channel_id;

	transaction.type = OutstandingType::persistenceLGDIN;

	unique_lock<shared_mutex> ta_wr_lock(transactions_mutex);
	    transactions[cmd.sequence_number] = transaction;
	    transactions.erase(seqnum);
	ta_wr_lock.unlock();

	communicator.send(cmd);
    }
}

void ProtocolDispatcher::onPersistenceLGDIN(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	channel_id_t channel_id = webapp_requests[transaction.original_sequence_number].channel_id;
    wa_lck.unlock();

    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,rp.status,channel_id);

    communicator.send(resp);

    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	webapp_requests.erase(seqnum);
    wa_wr_lck.unlock();

    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	transactions.erase(seqnum);
    ta_wr_lck.unlock();
}

void ProtocolDispatcher::onPersistenceULKDUP(const PersistenceLayerResponse& rp)
{
    // We have received an ULKDUP response. What do we do next?

    sequence_t seqnum = rp.sequence_number;

    // This (rp) is an answer. Someone did send a request; what transaction was it?
    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	debug_log("Received dangling transaction reference. (Persistence)");

	unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	    transactions.erase(seqnum);
	ta_wr_lck.unlock();

	return;
    }

    // We are in the process of sending a message and we just received information on the sender.
    // Is the sender authorized? If yes, ask for the receiver's information and return. If not,
    // respond with an error.
    if ( transaction.type == OutstandingType::persistenceSndmsgSenderULKDUP )
    {
	shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
	wa_lck.unlock();

	if ( (! rp.online) || rp.channel_id != original_webapp_request.channel_id || rp.broker_name != message_broker_name )
	{
	    // Unauthorized sender!
	    WebappResponse wr(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false);
	    communicator.send(wr);

	    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
		webapp_requests.erase(original_webapp_request.sequence_number);
	    wa_wr_lck.unlock();

	    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
		transactions.erase(seqnum);
	    ta_wr_lck.unlock();

	    return;
	}

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,original_webapp_request.dest_user);

	// Next message will be a ULKDUP for the receiver
	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;

	unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	    transactions[cmd.sequence_number] = transaction;
	    transactions.erase(seqnum);
	ta_wr_lck.unlock();

	communicator.send(cmd);

    } else if ( transaction.type == OutstandingType::persistenceSndmsgReceiverULKDUP )
    {
	// Is receiver online? If so, send to message relay, else send to persistence
	if ( rp.online )
	{
	    if ( rp.broker_name == message_broker_name ) // User is on this broker?
	    {
		shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
		    MessageForRelay msg(webapp_requests[transaction.original_sequence_number].message, rp.channel_id);
		wa_lck.unlock();

		transaction.type = OutstandingType::messagerelayMSGSNT;

		unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
		    transactions[msg.seq_num] = transaction;
		    transactions.erase(seqnum);
		ta_wr_lck.unlock();

		communicator.send(msg);
	    } else
		throw BrokerError(ErrorType::unimplemented,"Broker2Broker not implemented yet.");

	} else
	{
	    // send to persistence
	    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
		const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
	    wa_lck.unlock();

	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;

	    unique_lock<shared_mutex> ta_wr_lock(transactions_mutex);
		transactions[cmd.sequence_number] = transaction;
		transactions.erase(seqnum);
	    ta_wr_lock.unlock();

	    communicator.send(cmd);
	}

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	// Respond to the "is user online?" request.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::isOnline,rp.online);

	transactions.erase(seqnum);

	communicator.send(resp);
    } else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP )
    {
	shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
	wa_lck.unlock();

	if ( original_webapp_request.request_type != WebappRequestCode::logOut )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type logOut; however, this is not the case.");

	if ( rp.online == true && rp.channel_id == original_webapp_request.channel_id )
	{
	    // Now mark user as offline in persistence.
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut,original_webapp_request.user);

	    transaction.type = OutstandingType::persistenceLGDOUT;

	    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
		transactions[cmd.sequence_number] = transaction;
		transactions.erase(seqnum);
	    ta_wr_lck.unlock();

	    communicator.send(cmd);
	} else
	{
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,false);

	    communicator.send(resp);

	    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
		webapp_requests.erase(transaction.original_sequence_number);
	    wa_wr_lck.unlock();

	    unique_lock<shared_mutex> ta_wr_lck(webapp_requests_mutex);
		transactions.erase(seqnum);
	    ta_wr_lck.unlock();
	}

    } else
	throw BrokerError(ErrorType::genericImplementationError,"Unhandled transaction type in onPersistenceULKDUP.");

}

void ProtocolDispatcher::onPersistenceMSGSVD(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number )
    {
	unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	    transactions.erase(seqnum);
	ta_wr_lck.unlock();

	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    if ( transaction.type != OutstandingType::persistenceMSGSVD )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGSVD: Expected transaction type to be persistenceMSGSVD, but received other.");

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    wa_lck.unlock();

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

    communicator.send(resp);

    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	webapp_requests.erase(transaction.original_sequence_number);
    wa_wr_lck.unlock();

    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	transactions.erase(seqnum);
    ta_wr_lck.unlock();
}

void ProtocolDispatcher::onPersistenceLGDOUT(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number )
    {
	transactions.erase(seqnum);
	debug_log("Received dangling transaction reference. (Persistence)");
	return;
    }

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    wa_lck.unlock();

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,true);

    communicator.send(resp);

    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	webapp_requests.erase(transaction.original_sequence_number);
    wa_wr_lck.unlock();

    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	transactions.erase(seqnum);
    ta_wr_lck.unlock();
}

void ProtocolDispatcher::onPersistenceMSGS(const PersistenceLayerResponse& rp)
{
    // There is no GETMSGS command in the webapp protocol yet, therefore this empty handler.
}


void ProtocolDispatcher::onMessagerelayMSGSNT(const MessageRelayResponse& rp)
{
    // After onPersistenceULKDUP

    sequence_t seqnum = rp.sequence_number;

    shared_lock<shared_mutex> ta_lck(transactions_mutex);
	OutstandingTransaction& transaction = transactions[seqnum];
    ta_lck.unlock();

    if ( ! transaction.original_sequence_number )
    {
	debug_log("Received dangling transaction reference. (message relay)");

	unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	    transactions.erase(seqnum);
	ta_wr_lck.unlock();

	return;
    }

    if ( transaction.type != OutstandingType::messagerelayMSGSNT )
	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayMSGSNT: Expected transaction type to be messagerelayMSGSNT, but received other.");

    shared_lock<shared_mutex> wa_lck(webapp_requests_mutex);
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    wa_lck.unlock();

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

    communicator.send(resp);

    unique_lock<shared_mutex> wa_wr_lck(webapp_requests_mutex);
	webapp_requests.erase(transaction.original_sequence_number);
    wa_wr_lck.unlock();

    unique_lock<shared_mutex> ta_wr_lck(transactions_mutex);
	transactions.erase(seqnum);
    ta_wr_lck.unlock();
}