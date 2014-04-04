# ifndef PARSE_PERSISTENT_MESSAGE_HPP
# define PARSE_PERSISTENT_MESSAGE_HPP

# include <string>
# include <vector>
# include <sstream>

# include "sequence-number.hpp"

using std::string;
using std::vector;
using std::istringstream;

/*********************** Parsing and representing protocol responses ***************************/

enum class PersistenceLayerResponseCode {
    /// User has been registered (or not...)
    userRegistered,
    /// Password was checked.
    passwordChecked,
    /// User has been marked as online
    loggedIn,
    /// User has been marked as offline
    loggedOut,
    /// User lookup results
    lookedUpUser,
    /// Message has been saved.
    savedMessage,
    /// Retained messages.
    messages
};

/**
 * @brief A generic response from the persistence layer
 *
 * An object of this class is used for the responses `userRegistered`, `passwordChecked`,
 * `loggedIn`, `savedMessage`.
 *
 * There are specialized classes for `lookedUpUser` and `messages`. The parser function returns
 * a pointer to the base class persistenceLayerResponse which may be casted to the special classes
 * if the `response_type` says so.
 *
 */
struct PersistenceLayerResponse
{
    PersistenceLayerResponseCode response_type;
    /// A unique sequence number of a transaction which may be used to find and restart an operation.
    sequence_t sequence_number;
    /// Success?
    bool status;
};

/**
 * @brief Response class for user look-up operations.
 *
 * Additional fields contain the name of the broker and the channel of the user on that broker.
 */
struct PersistenceLayerLookupResponse : public PersistenceLayerResponse
{
    PersistenceLayerLookupResponse(void);

    // status may be `true` if the user is offline.
    bool online;
    string broker_name;
    string channel_name;
};

/**
 * @brief Response class for message retrieval
 *
 * If the broker asked for old messages, this class is used, containing the messages sent by the persistence layer.
 */
struct PersistenceLayerMessagesResponse : public PersistenceLayerResponse
{
    PersistenceLayerMessagesResponse(void);

    vector<string> messages;
};

extern PersistenceLayerResponse* parsePersistenceResponse(const string&);

extern istringstream& operator>>(istringstream& stream, PersistenceLayerResponseCode& code);

/************************************* Creating protocol messages **************************************/

/**
 * @brief Possible command types.
 */
enum class PersistenceLayerCommandCode {
    /// Register a user
    registerUser,
    /// Check if a certain password is valid for a user
    checkPassword,
    /// Mark a user as online
    logIn,
    /// Mark a user as offline
    logOut,
    /// Get information on a user (broker location, channel id)
    lookUpUser,
    /// Save a message for a user who's offline
    saveMessage,
    /// Get saved messages for a user
    getMessages
};

class PersistenceLayerCommand
{
public:
    /// For ULKUP, LOGOUT, MSGGT
    PersistenceLayerCommand(PersistenceLayerCommandCode, string user_name);
    /// For UREG, CHKPASS, MSGSV
    PersistenceLayerCommand(PersistenceLayerCommandCode, string user_name, string password_or_message);
    /// For LOGIN
    PersistenceLayerCommand(PersistenceLayerCommandCode, string user_name, string broker_name, string channel_id);

    string toString(void);
private:
    sequence_t sequence_number;
    PersistenceLayerCommandCode type;

    string user_name;

    // Those are mutually exclusive. {
    string password_or_message;
    string broker_name;
    // }

    string channel_id;
};
# endif