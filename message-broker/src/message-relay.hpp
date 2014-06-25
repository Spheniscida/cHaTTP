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

class MessageForRelay
{
public:
    MessageForRelay(const string& channel_id, const ChattpMessage& mesg);
    MessageForRelay(const string& channel_id, MessageRelayRequest::MessageRelayRequestType action_type);

    string toString(void) const { return request_buffer.SerializeAsString(); }
    const MessageRelayRequest& get_protobuf(void) const { return request_buffer; }
    sequence_t sequence_number(void) const { return request_buffer.sequence_number(); };

private:
    MessageRelayRequest request_buffer;
};

class MessageRelayResponse : public Receivable
{
public:
    MessageRelayResponse(const char* buffer, size_t length);

    const chattp::MessageRelayResponse& get_protobuf(void) const { return response_buffer; }

    sequence_t sequence_number(void) const { return response_buffer.sequence_number(); }
    chattp::MessageRelayResponse::MessageRelayResponseType type(void) const { return response_buffer.type(); }
    bool status(void) const { return response_buffer.status(); }
private:

    chattp::MessageRelayResponse response_buffer;
};

# endif
