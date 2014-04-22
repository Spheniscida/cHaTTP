# include "message-relay.hpp"
# include "error.hpp"

# include <sstream>

using std::istringstream;
using std::ostringstream;

MessageForRelay::MessageForRelay (const string& sender, const string& mesg, const string& chan_id )
    : seq_num(getNewSequenceNumber()),
    message(mesg),
    sender_user(sender),
    channel_id(chan_id)
{
}

string MessageForRelay::toString ( void ) const
{
    ostringstream message_to_send;

    message_to_send << seq_num << "\n" << "SNDMSG\n" << sender_user << '\n' << channel_id << '\n' << message;

    return message_to_send.str();
}

MessageRelayResponse::MessageRelayResponse ( const string& response )
{
    istringstream response_stream(response);
    string response_type, response_status;

    sender = MessageOrigin::fromMessageRelay;

    response_stream >> sequence_number;
    response_stream >> response_type;
    response_stream >> response_status;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid sequence number.");
    if ( response_type != "MSGSNT" )
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid response type.");
    if ( response_status == "OK" )
	status = true;
    else if ( response_status == "FAIL" )
	status = false;
    else
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid response status code.");
}
