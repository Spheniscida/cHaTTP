# include "broker2broker.hpp"
# include <sstream>
#include <sys/socket.h>

/**
 * @brief Initialize an outgoing message of SNDMSG type.
 */
MessageForB2B::MessageForB2B(const chattp::ChattpMessage& mesg, const string& channel_id)
{
    message_buffer.set_sequence_number(b2b_counter.get());
    *(message_buffer.mutable_mesg()) = mesg;
    message_buffer.set_channel_id(channel_id);
    message_buffer.set_type(B2BMessage::SENDMESSAGE);
}

/**
 * @brief Initialize an outgoing message of MSGSNT type.
 */
MessageForB2B::MessageForB2B(sequence_t seq_num, bool status)
{
    message_buffer.set_sequence_number(seq_num);
    message_buffer.set_type(B2BMessage::MESSAGESENT);
    message_buffer.set_status(status);
}

B2BIncoming::B2BIncoming(const char* buffer, size_t length, const string& origin_b)
    : origin_broker(origin_b)
{
    message_buffer.ParseFromArray(buffer,length);
}
