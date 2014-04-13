# include "message-relay.hpp"

# include <sstream>

# include "error.hpp"

using std::istringstream;
using std::ostringstream;

MessageForRelay::MessageForRelay ( const string& mesg, const string& chan_id )
    : message(mesg),
    channel_id(chan_id)
{
}

string MessageForRelay::toString ( void )
{
    ostringstream message_to_send;

    message_to_send << getNewSequenceNumber() << "\n";
    message_to_send << "SNDMSG\n";
    message_to_send << channel_id << "\n";
    message_to_send << message;

    return message_to_send.str();
}

MessageRelayResponse::MessageRelayResponse ( const string& response )
{
    istringstream response_stream(response);
    string response_type, response_status;

    response_stream >> seq_num;
    response_stream >> response_type;
    response_stream >> response_status;

    if ( seq_num == 0 )
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
