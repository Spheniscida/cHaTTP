# include "broker2broker.hpp"
# include <sstream>

namespace {
    const char* ok_code = "OK";
    const char* fail_code = "FAIL";

    thread_local char current_message[max_message_size];
}

/**
 * @brief Initialize an outgoing message of SNDMSG type.
 */
MessageForB2B::MessageForB2B(const string& msg, const string& chan_id)
    : sequence_number(getNewSequenceNumber()),
    message(msg),
    channel_id(chan_id),
    type(B2BMessageType::B2BSNDMSG)
{
}

/**
 * @brief Initialize an outgoing message of MSGSNT type.
 */
MessageForB2B::MessageForB2B(sequence_t seq_num, bool stat)
    : sequence_number(seq_num),
    type(B2BMessageType::B2BMSGSNT),
    status(stat)
{
}

string MessageForB2B::toString(void) const
{
    std::ostringstream outgoing;

    outgoing << sequence_number << '\n';

    if ( type == B2BMessageType::B2BMSGSNT )
	outgoing << "MSGSNT\n" << (status ? ok_code : fail_code);
    else if ( type == B2BMessageType::B2BSNDMSG )
	outgoing << "SNDMSG\n" << channel_id << '\n' << message;

    return outgoing.str();
}

B2BIncoming::B2BIncoming(const string& message, const string& origin)
    : origin_broker(origin)
{
    sender = MessageOrigin::fromBroker;
    parseMessage(message);
}

void B2BIncoming::parseMessage(const string& msg)
{
    std::istringstream msg_stream(msg);

    msg_stream >> sequence_number;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"B2BIncoming::parseMessage(): Could not parse sequence number/Sequence number is 0.");

    string msg_type;

    msg_stream >> msg_type;

    if ( msg_type == "SNDMSG" )
    {
	type = B2BMessageType::B2BSNDMSG;

	msg_stream >> channel_id;

	msg_stream.getline(current_message,0); // Remove \n from stream
	msg_stream.getline(current_message,max_message_size);

	current_message[max_message_size-1] = 0;

	message = current_message;

	if ( channel_id.empty() || message.empty() )
	    throw BrokerError(ErrorType::protocolError,"B2BIncoming::parseMessage(): Channel ID or message are empty fields in SNDMSG request.");
    } else if ( msg_type == "MSGSNT" )
    {
	type = B2BMessageType::B2BMSGSNT;

	string str_status;
	msg_stream >> str_status;

	if ( str_status == ok_code )
	    status = true;
	else if ( str_status == fail_code )
	    status = false;
	else
	    throw BrokerError(ErrorType::protocolError,"B2BIncoming::parseMessage(): Unrecognized status code in message.");
    }
}
