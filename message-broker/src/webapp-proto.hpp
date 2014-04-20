# ifndef WEBAPP_PROTO_HPP
# define WEBAPP_PROTO_HPP

# include <string>
# include <sstream>

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;
using std::istringstream;

/**
 * @file webapp-proto.hpp
 *
 * Defining the protocol used to communicate with the FastCGI back-end web
 * application; it's described in /doc/protocols/webapp-message-broker.mkd.
 */

/**
 * @brief Channel ID type
 *
 * The channel id is used to identify a user's session; it should be a randomly generated
 * token of roughly 20 to 60 characters.
 *
 * The same string is used as an identifier for a session in the nginx delivery back-end.
 */
typedef string channel_id_t;

/**
 * @brief Command types available for communication with us.
 */
enum class WebappRequestCode {
    /// UREG - register a user
    registerUser,
    /// LOGIN - mark user as online
    logIn,
    /// LOGOUT - mark user as offline
    logOut,
    /// SNDMSG - Send a message to another user.
    sendMessage,
    /// UONLQ - Ask if specified user is online.
    isOnline
};

/******************************** Process incoming requests ********************************/

/**
 * @brief Class for web application requests to the message broker (that is, us).
 *
 */
class WebappRequest : public Receivable
{
public:
    WebappRequest(const string&);
    WebappRequest(void) = default;

    /// for UREG, LOGIN, LOGOUT, UONLQ, SNDMSG (sender's user name)
    string user;
    /// for UREG, LOGIN
    string password;

    /// for SNDMSG, LOGOUT
    channel_id_t channel_id;
    /// for SNDMSG
    string dest_user;
    /// for SNDMSG
    string message;

    sequence_t sequence_number;
    WebappRequestCode request_type;
private:
    void parseWebappRequest(const string& rq);
};

extern istringstream& operator>>(istringstream&, WebappRequestCode&);

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
    isOnline
};


class WebappResponse
{
public:
    WebappResponse(sequence_t seq_num, WebappResponseCode type, bool response_status = true, const string& response_data = "");

    string toString(void) const;
private:
    /// e.g. LGDIN <channel id>
    string payload;
    sequence_t sequence_number;
    WebappResponseCode response_type;
    bool status;
};


# endif