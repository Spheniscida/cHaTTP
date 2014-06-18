# include "message-relay.hpp"
# include "error.hpp"

# include <sstream>

using std::istringstream;
using std::ostringstream;

MessageForRelay::MessageForRelay (const string& sender, const string& mesg, const string& chan_id )
    : seq_num(getNewSequenceNumber(SequenceCounter::MessageRelayCounter)),
    message(mesg),
    sender_user(sender),
    channel_id(chan_id),
    type(MessageForRelayType::sendMessage)
{
}

MessageForRelay::MessageForRelay(const string& chan_id, MessageForRelayType action_type)
    : seq_num(getNewSequenceNumber(SequenceCounter::MessageRelayCounter)),
    channel_id(chan_id),
    type(action_type)
{

}

string MessageForRelay::toString ( void ) const
{
    ostringstream message_to_send;

    switch ( type )
    {
	case MessageForRelayType::sendMessage:
	    message_to_send << seq_num << "\n" << "SNDMSG\n" << sender_user << '\n' << channel_id << '\n' << message;
	    break;
	case MessageForRelayType::deleteChannel:
	    message_to_send << seq_num << "\n" << "DELCHAN\n" << channel_id;
	    break;
	case MessageForRelayType::createChannel:
	    message_to_send << seq_num << "\n" << "NEWCHAN\n" << channel_id;
	    break;
    }

    return message_to_send.str();
}

MessageRelayResponse::MessageRelayResponse ( const string& response )
{
    parseMessage(response);
}

void MessageRelayResponse::parseMessage(const string& mesg)
{
    istringstream response_stream(mesg);
    string response_type_raw, response_status;

    response_stream >> sequence_number;
    response_stream >> response_type_raw;
    response_stream >> response_status;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid sequence number.");

    if ( response_type_raw == "MSGSNT" )
	response_type = MessageRelayResponseType::messageSent;
    else if ( response_type_raw == "DELTDCHAN" )
	response_type = MessageRelayResponseType::channelDeleted;
    else if ( response_type_raw == "CHANCREAT" )
	response_type = MessageRelayResponseType::channelCreated;
    else
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid response type.");

    if ( response_status == "OK" )
	status = true;
    else if ( response_status == "FAIL" )
	status = false;
    else
	throw BrokerError(ErrorType::protocolError,"MessageRelayResponse: Received invalid response status code.");

}
