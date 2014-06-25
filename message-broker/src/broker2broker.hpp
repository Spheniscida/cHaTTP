# ifndef BROKER2BROKER_HPP
# define BROKER2BROKER_HPP

# include <string>

# include <broker2broker.pb.h>

using chattp::B2BMessage;

# include "error.hpp"
# include "conf.hpp"
# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;

/**
 * @brief Class for outgoing B2B messages.
 */
class MessageForB2B
{
public:
    // for SNDMSG
    MessageForB2B(const chattp::ChattpMessage& mesg, const string& channel_id);
    // for MSGSNT
    MessageForB2B(sequence_t seq_num, bool status);

    string toString(void) const { return message_buffer.SerializeAsString(); }
    const B2BMessage& get_protobuf(void) const { return message_buffer; }
    sequence_t sequence_number(void) const { return message_buffer.sequence_number(); }
private:

    B2BMessage message_buffer;
};

/**
 * @brief Class for incoming B2B messages.
 */
class B2BIncoming : public Receivable
{
public:
    B2BIncoming(const char* buffer, size_t length, const string& origin_b);

    const chattp::B2BMessage& get_protobuf(void) const { return message_buffer; }

    bool type(void) const { return message_buffer.type(); }
    bool status(void) const { return message_buffer.status(); }
    const string& channel_id(void) const { return message_buffer.channel_id(); }
    sequence_t sequence_number(void) const { return message_buffer.sequence_number(); }

    const string origin_broker;
private:

    B2BMessage message_buffer;
};

# endif
