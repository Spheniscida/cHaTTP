# include "broker.hpp"
# include "broker-util.hpp"

unordered_map<sequence_t,OutstandingTransaction> transactions;
unordered_map<sequence_t,WebappRequest> webapp_requests;

void ProtocolDispatcher::onWebAppLOGIN(const WebappRequest& rq)
{
    OutstandingTransaction transaction;

    transaction.type = OutstandingType::persistenceCHKDPASS;
    transaction.original_sequence_number = rq.sequence_number;

    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::checkPassword,rq.user,rq.password);

    transactions[cmd.sequence_number] = transaction;
    webapp_requests[rq.sequence_number] = rq;

    communicator.send(cmd);
}


void ProtocolDispatcher::onPersistenceCHKDPASS(const PersistenceLayerResponse& rp)
{
    // This is called on a response to a request made by onWebAppLOGIN.

    sequence_t seqnum = rp.sequence_number;
    OutstandingTransaction& transaction = transactions[seqnum];

    if ( transaction.type != OutstandingType::persistenceCHKDPASS )
	throw BrokerError(ErrorType::genericImplementationError,"onPersistenceCHKDPASS: Expected transaction.type to be persistenceCHKDPASS.");

    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

    // Password/user incorrect?
    if ( rp.status == false )
    {
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];
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

void ProtocolDispatcher::onPersistenceULKDUP(const PersistenceLayerResponse& rp)
{
    // We have received an ULKDUP response. What do we do next?

    sequence_t seqnum = rp.sequence_number;

    // This (rp) is an answer. Someone did send a request; what transaction was it?
    OutstandingTransaction& transaction = transactions[seqnum];

    // We are in the process of sending a message and we just received information on the sender.
    // Is the sender authorized? If yes, ask for the receiver's information and return. If not,
    // respond with an error.
    if ( transaction.type == OutstandingType::persistenceSndmsgSenderULKDUP )
    {
	const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

	if ( rp.channel_name != original_webapp_request.channel_id )
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
		MessageForRelay msg(webapp_requests[transaction.original_sequence_number].message, rp.channel_name);

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
	    transaction.type = OutstandingType::persistenceMSGSVD;
	    const WebappRequest& original_webapp_request = webapp_requests[transaction.original_sequence_number];

	    PersistenceLayerCommand cmd(PersistenceLayerCommandCode::saveMessage, original_webapp_request.dest_user, original_webapp_request.message);

	    transactions[cmd.sequence_number] = transaction;
	    transactions.erase(seqnum);

	    communicator.send(cmd);
	}

    } else if ( transaction.type == OutstandingType::persistenceUonlqULKDUP )
    {
	// Respond to the "is user online?" request.
	WebappResponse resp(transaction.original_sequence_number,WebappResponseCode::isOnline,rp.online);

	// This transaction is finished and may be purged.
	transactions.erase(seqnum);

	communicator.send(resp);
    }

}
