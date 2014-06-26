# ifndef WEBAPP_PROTO_HPP
# define WEBAPP_PROTO_HPP

# include <string>
# include <sstream>

# include "sequence-number.hpp"
# include "receivable.hpp"

# include <webapp.pb.h>

using chattp::WebappRequestMessage;
using chattp::WebappResponseMessage;

using std::string;

/**
 * @file webapp-proto.hpp
 *
 * Defining the protocol used to communicate with the FastCGI back-end web
 * application; it's described in /doc/protocols/webapp-message-broker.mkd.
 */

/******************************** Process incoming requests ********************************/

/**
 * @brief Class for web application requests to the message broker (that is, us).
 *
 */
class WebappRequest : public Receivable
{
public:
    WebappRequest(const char* buffer, size_t length);
    WebappRequest(void) = default;

    const chattp::WebappRequestMessage& get_protobuf(void) const { return request_buffer; }

	string channel_id_; // This field stores the channel id for LOGIN operations, i.e. when the WebappRequest object is in the webapp request cache.

    sequence_t sequence_number(void) const { return request_buffer.sequence_number(); }
    WebappRequestMessage::WebappRequestType type(void) const { return request_buffer.type(); }
    const string& user_name(void) const { return request_buffer.user_name(); }
    const string& password(void) const { return request_buffer.password(); }
    const string& channel_id(void) const { return request_buffer.channel_id(); }
    const string& message_sender(void) const { return request_buffer.mesg().sender(); }
    const string& message_receiver(void) const { return request_buffer.mesg().receiver(); }
    const string& message_body(void) const { return request_buffer.mesg().body(); }
    const string& settings(void) const { return request_buffer.settings(); }

private:
    WebappRequestMessage request_buffer;
};

/****************************** Create outgoing responses ********************************/

class WebappResponse
{
public:
    WebappResponse(sequence_t seq_num, WebappResponseMessage::WebappResponseType type, bool response_status, string error_desc); // No default value because ambiguity
    WebappResponse(sequence_t seq_num, WebappResponseMessage::WebappResponseType type, bool response_status, const string& channel_id_or_settings, string error_desc);
    WebappResponse(sequence_t seq_num, WebappResponseMessage::WebappResponseType type, bool response_status, bool online_authorized, string error_desc);
    WebappResponse(sequence_t seq_num, WebappResponseMessage::WebappResponseType type, bool response_status,
		   google::protobuf::RepeatedPtrField<const chattp::ChattpMessage>::iterator begin,
		   google::protobuf::RepeatedPtrField<const chattp::ChattpMessage>::iterator end,
		   string error_desc = "");

    string toString(void) const;
    const chattp::WebappResponseMessage& get_protobuf(void) const { return response_buffer; }
private:
    WebappResponseMessage response_buffer;
};

# endif
