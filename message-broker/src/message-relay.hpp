# ifndef MESSAGE_RELAY_HPP
# define MESSAGE_RELAY_HPP

# include <string>

# include <messagerelay.pb.h>
# include <message.pb.h>

using chattp::ChattpMessage;
using chattp::MessageRelayRequest;

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;

enum class MessageForRelayType {
    sendMessage,
    createChannel,
    deleteChannel
};

class MessageForRelay
{
public:
    MessageForRelay(const string& channel_id, const ChattpMessage& mesg);
    MessageForRelay(const string& chan_id, MessageRelayRequest::MessageRelayRequestType action_type);

    string toString(void) const { return request_buffer.SerializeAsString(); }
    sequence_t sequence_number(void) const { return request_buffer.sequence_number(); };

private:
    MessageRelayRequest request_buffer;
};

enum class MessageRelayResponseType {
    messageSent,
    channelCreated,
    channelDeleted
};

class MessageRelayResponse : public Receivable
{
public:
    MessageRelayResponse(const char* buffer, size_t length);

    sequence_t sequence_number(void) const { return response_buffer.sequence_number(); }
    chattp::MessageRelayResponse::MessageRelayResponseType type(void) const { return response_buffer.type(); }
    bool status(void) const { return response_buffer.status(); }
private:

    chattp::MessageRelayResponse response_buffer;
};

# endif
