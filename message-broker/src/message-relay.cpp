# include "message-relay.hpp"
# include "error.hpp"

# include <sstream>

using std::istringstream;
using std::ostringstream;

MessageForRelay::MessageForRelay (const string& channel_id, const ChattpMessage& mesg)
{
    request_buffer.set_channel_id(channel_id);
    request_buffer.set_type(MessageRelayRequest::SENDMESSAGE);
    request_buffer.set_sequence_number(message_relay_counter.get());
    *(request_buffer.mutable_mesg()) = mesg;
}

MessageForRelay::MessageForRelay(const string& chan_id, MessageRelayRequest::MessageRelayRequestType action_type)
{
    request_buffer.set_sequence_number(message_relay_counter.get());
    request_buffer.set_type(action_type);
    request_buffer.set_channel_id(chan_id);
}

MessageRelayResponse::MessageRelayResponse (const char* buffer, size_t length)
{
    response_buffer.ParseFromArray(buffer,length);
}
