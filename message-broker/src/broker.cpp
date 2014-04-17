# include "broker.hpp"
# include "broker-util.hpp"

unordered_map<sequence_t,OutstandingTransaction> transactions;
unordered_map<sequence_t,WebappRequest> webapp_requests;

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
		case MessageOrigin::fromPersistence:
		    handlePersistenceMessage(received_messages[i]);
		    break;
		case MessageOrigin::fromWebApp:
		    handleWebappMessage(received_messages[i]);
		    break;
		case MessageOrigin::fromMessageRelay:
		    handleMessagerelayMessage(received_messages[i]);
		    break;
		default:
		    throw BrokerError(ErrorType::unimplemented,"dispatch(): Unimplemented origin.");
	    }
	}
    }
}
/***************************** Message handlers ****************************/

void ProtocolDispatcher::handlePersistenceMessage(Receivable* msg)
{
    PersistenceLayerResponse* response = dynamic_cast<PersistenceLayerResponse*>(msg);

    if ( ! response )
	throw BrokerError(ErrorType::genericImplementationError,"handlePersistenceMessage(): Expected type PersistenceLayerResponse, but dynamic_cast failed.");

    // TODO: Rearrange items for performance?
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

void ProtocolDispatcher::handleWebappMessage(Receivable* msg)
{
    WebappRequest* request = dynamic_cast<WebappRequest*>(msg);

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

void ProtocolDispatcher::handleMessagerelayMessage(Receivable* msg)
{
    MessageRelayResponse* response = dynamic_cast<MessageRelayResponse*>(msg);

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

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq;

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    // Next packet for this transaction is CHKDPASS.
    transaction.type = OutstandingType::persistenceCHKDPASS;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,rq.user,rq.password);

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq;

    communicator.send(cmd);
}

void ProtocolDispatcher::onWebAppLOGOUT(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceLogoutULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    // FIXME: Security flaw -- anyone can log-out anyone else. Use channel-id authentication.
    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq;
}

void ProtocolDispatcher::onWebAppSNDMSG(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.original_sequence_number = rq.sequence_number;
    transaction.type = OutstandingType::persistenceSndmsgSenderULKDUP;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq;

    communicator.send(cmd);
    // Next up: onPersistenceULKDUP (sender) → onPersistenceULKDUP (receiver) → onMessagerelayMSGSNT/onPersistenceMSGSVD
}

void ProtocolDispatcher::onWebAppUONLQ(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceUonlqULKDUP;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,rq.user);

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq; // We probably don't need this.

    communicator.send(cmd);
}

void ProtocolDispatcher::onPersistenceUREGD(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	return;
    }

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
    WebappResponse resp(original_webapp_request.sequence_number, WebappResponseCode::registeredUser, rp.status);

    webapp_requests.erase(transaction.original_sequence_number);
    transactions.erase(seqnum);

    communicator.send(resp);
}

void ProtocolDispatcher::onPersistenceCHKDPASS(const PersistenceLayerResponse& rp)
{
    // This is called on a response to a request made by onWebAppLOGIN.

    sequence_t seqnum = rp.sequence_number;
    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	return;
    }

    if ( transaction.type != OutstandingType::persistenceCHKDPASS )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceCHKDPASS: Expected transaction.type to be persistenceCHKDPASS.");

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

    // Password/user incorrect?
    if ( rp.status == false )
    {
	WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedIn,false);

	communicator.send(resp);

	transactions.erase(seqnum);
	webapp_requests.erase(original_webapp_request.sequence_number);
    } else
    {
	string channel_id = generateChannelId();
	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logIn,original_webapp_request.user,message_broker_name,channel_id);

	transaction.type = OutstandingType::persistenceLGDIN;
	transactions[cmd.sequence_number] = transaction;
	transactions.erase(seqnum);

	communicator.send(cmd);
    }
}

void ProtocolDispatcher::onPersistenceLGDIN(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	return;
    }

    WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::loggedIn,rp.status);

    communicator.send(resp);

    transactions.erase(seqnum);
}

void ProtocolDispatcher::onPersistenceULKDUP(const PersistenceLayerResponse& rp)
{
    // We have received an ULKDUP response. What do we do next?

    sequence_t seqnum = rp.sequence_number;

    // This (rp) is an answer. Someone did send a request; what transaction was it?
    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number ) // Hit non-existent transaction
    {
	transactions.erase(seqnum);
	return;
    }

    // We are in the process of sending a message and we just received information on the sender.
    // Is the sender authorized? If yes, ask for the receiver's information and return. If not,
    // respond with an error.
    if ( transaction.type == OutstandingType::persistenceSndmsgSenderULKDUP )
    {
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

	if ( rp.channel_id != original_webapp_request.channel_id )
	{
	    // Unauthorized sender!
	    WebappResponse wr(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,false);
	    communicator.send(wr);

	    webapp_requests.erase(original_webapp_request.sequence_number);
	    transactions.erase(seqnum);
	    return;
	}

	PersistenceLayerCommand cmd(PersistenceLayerCommandCode::lookUpUser,original_webapp_request.dest_user);

	// Next message will be a ULKDUP for the receiver
	transaction.type = OutstandingType::persistenceSndmsgReceiverULKDUP;
	transactions[cmd.sequence_number] = transaction;
	transactions.erase(seqnum);

	communicator.send(cmd);
    } else if ( transaction.type == OutstandingType::persistenceSndmsgReceiverULKDUP )
    {
	// Is receiver online? If so, send to message relay, else send to persistence
	if ( rp.online )
	{
	    if ( rp.broker_name == message_broker_name ) // User is on this broker?
	    {
		MessageForRelay msg(webapp_requests[transaction.original_sequence_number].message, rp.channel_id);

		transaction.type = OutstandingType::messagerelayMSGSNT;
		transactions[msg.seq_num] = transaction;
		// FIXME: Maybe we should delete the message only if the message relay sent OK?
		webapp_requests.erase(transaction.original_sequence_number);
		transactions.erase(seqnum);

		communicator.send(msg);
	    } else
		throw BrokerError(ErrorType::unimplemented,"Broker2Broker not implemented yet.");

	} else
	{
	    // send to persistence
	    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transaction.type = OutstandingType::persistenceMSGSVD;
	    transactions[cmd.sequence_number] = transaction;
	    transactions.erase(seqnum);

	    communicator.send(cmd);
	}

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	// Respond to the "is user online?" request.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::isOnline,rp.online);

	// This transaction is finished and may be purged.
	webapp_requests.erase(transaction.original_sequence_number); // We probably don't need this.
	transactions.erase(seqnum);

	communicator.send(resp);
    } else if ( transaction.type == OutstandingType::persistenceLogoutULKDUP )
    {
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

	if ( original_webapp_request.request_type != WebappRequestCode::logOut )
	    throw BrokerError(ErrorType::genericImplementationError,"Expected original webapp request to be of type logOut; however, this is not the case.");

	//
	if ( rp.online == true && rp.channel_id == original_webapp_request.channel_id )
	{
	    // Now mark user as offline in persistence.
	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::logOut,original_webapp_request.user);

	    transaction.type = OutstandingType::persistenceLGDOUT;
	    transactions[cmd.sequence_number] = transaction;
	    transactions.erase(seqnum);

	    communicator.send(cmd);
	} else
	{
	    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,false);

	    communicator.send(resp);

	    webapp_requests.erase(transaction.original_sequence_number);
	    transactions.erase(seqnum);
	}

    } else
	throw BrokerError(ErrorType::genericImplementationError,"Unhandled transaction type in onPersistenceULKDUP.");

}

void ProtocolDispatcher::onPersistenceMSGSVD(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number )
    {
	transactions.erase(seqnum);
	return;
    }

    if ( transaction.type != OutstandingType::persistenceMSGSVD )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceMSGSVD: Expected transaction type to be persistenceMSGSVD, but received other.");

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

    communicator.send(resp);

    webapp_requests.erase(transaction.original_sequence_number);
    transactions.erase(seqnum);
}

void ProtocolDispatcher::onPersistenceLGDOUT(const PersistenceLayerResponse& rp)
{
    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transactions[seqnum];

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::loggedOut,true);

    communicator.send(resp);

    webapp_requests.erase(transaction.original_sequence_number);
    transactions.erase(seqnum);
}

void ProtocolDispatcher::onPersistenceMSGS(const PersistenceLayerResponse& rp)
{
    // There is no GETMSGS command in the webapp protocol yet, therefore this empty handler.
}


void ProtocolDispatcher::onMessagerelayMSGSNT(const MessageRelayResponse& rp)
{
    // After onPersistenceULKDUP

    sequence_t seqnum = rp.sequence_number;

    OutstandingTransaction& transaction = transactions[seqnum];

    if ( ! transaction.original_sequence_number )
    {
	transactions.erase(seqnum);
	return;
    }

    if ( transaction.type != OutstandingType::messagerelayMSGSNT )
	throw BrokerError(ErrorType::genericImplementationError,"onMessagerelayMSGSNT: Expected transaction type to be messagerelayMSGSNT, but received other.");

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

    WebappResponse resp(original_webapp_request.sequence_number,WebappResponseCode::acceptedMessage,rp.status);

    communicator.send(resp);

    webapp_requests.erase(transaction.original_sequence_number);
    transactions.erase(seqnum);
}