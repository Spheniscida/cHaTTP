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
using std::istringstream;

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

	string channel_id_; // FIXME: Only temporary to silence the compiler (onPersistenceCHKDPASS())

    const sequence_t sequence_number(void) const { return request_buffer.sequence_number(); }
    const WebappRequestMessage::WebappRequestType type(void) const { return request_buffer.type(); }
    const string& user_name(void) const { return request_buffer.user_name(); }
    const string& password(void) const { return request_buffer.password(); }
    const string& channel_id(void) const { return request_buffer.channel_id(); }
    const string& message_sender(void) const { return request_buffer.mesg().sender(); }
    const string& message_receiver(void) const { return request_buffer.mesg().receiver(); }
    const string& message_body(void) const { return request_buffer.mesg().body(); }
    const bool is_group_message(void) const { return request_buffer.mesg().group_message(); }

private:
    WebappRequestMessage request_buffer;
};

/****************************** Create outgoing responses ********************************/

/**
 * @brief Response types sent by us.
 */
enum class WebappResponseCode {
    /// Registered user.
    registeredUser,
    /// Marked user as online.
    loggedIn,
    /// Marked user as offline.
    loggedOut,
    /// Accepted message for delivery (or didn't...)
    acceptedMessage,
    /// User status.
    isOnline,
    /// saved messages
    savedMessages,
    /// authorized y/n
    isAuthorized
};


class WebappResponse
{
public:
    WebappResponse(sequence_t seq_num, WebappResponseCode type, bool response_status = true, const string& error_message = "", const string& response_data = "");

    string toString(void) const;
private:
    /// e.g. LGDIN <channel id> or messages
    string payload;
    /// an error message appended after FAIL
    string error_message;
    sequence_t sequence_number;
    WebappResponseCode response_type;
    bool status;
};


# endif
