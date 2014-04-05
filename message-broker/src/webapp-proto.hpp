# ifndef WEBAPP_PROTO_HPP
# define WEBAPP_PROTO_HPP

# include <string>
# include <sstream>

# include "sequence-number.hpp"

using std::string;
using std::istringstream;

/**
 * @file webapp-proto.hpp
 *
 * Defining the protocol used to communicate with the FastCGI back-end web
 * application; it's described in /doc/protocols/webapp-message-broker.mkd.
 */

extern void initWebapp(void);

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

/******************************** Process incoming requests ********************************/

/**
 * @brief Class for web application requests to the message broker (that is, us).
 *
 */
struct WebappRequest
{
    WebappRequest(const string&);

    WebappRequestCode request_type;
    sequence_t sequence_number;

    /// for UREG, LOGIN, LOGOUT, UONLQ, SNDMSG (sender's user name)
    string user;
    /// for UREG, LOGIN
    string password;

    /// for SNDMSG
    channel_id_t channel_id;
    /// for SNDMSG
    string dest_user;
    /// for SNDMSG
    string message;
};

extern istringstream& operator>>(istringstream&, WebappRequestCode&);

/****************************** Create outgoing responses ********************************/



# endif