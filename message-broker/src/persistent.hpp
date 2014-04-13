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
 * Most fields are only filled for specific response types:
 *
 * - sequence_number exists in any instance.
 * - status too.
 * - online, broker_name, channel_name: only for ULKDUP
 * - messages only for MSGS
 *
 */
class PersistenceLayerResponse
{
public:
    PersistenceLayerResponse(const string&);
    PersistenceLayerResponse(void);

    PersistenceLayerResponseCode response_type;
    /// A unique sequence number of a transaction which may be used to find and restart an operation.
    sequence_t sequence_number;
    /// Success?
    bool status;

    // Fields for Lookup Responses.
    bool online;
    string broker_name;
    string channel_name;

    // Field for message retrieval responses.
    vector<string> messages;

private:
    void parsePersistenceResponse(const string& message);

};

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
    /// This one is initialized automatically.
    sequence_t sequence_number;

    PersistenceLayerCommandCode request_type;

    string user_name;

    // Those are mutually exclusive. {
    string password_or_message;
    string broker_name;
    // }

    string channel_id;
};
# endif