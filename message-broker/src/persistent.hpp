# ifndef PARSE_PERSISTENT_MESSAGE_HPP
# define PARSE_PERSISTENT_MESSAGE_HPP

# include <string>
# include <vector>
# include <sstream>

# include <persistence.pb.h>

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;
using std::vector;
using std::istringstream;

using namespace chattp;

/*********************** Parsing and representing protocol responses ***************************/

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
class PersistenceLayerResponse : public Receivable
{
public:
    PersistenceLayerResponse(const char* buffer, size_t length);
    
    chattp::PersistenceResponse::PersistenceResponseType type(void) const { return response_buffer.type(); }
    
    
    
private:
    chattp::PersistenceResponse response_buffer;

};

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
    PersistenceLayerCommand(PersistenceLayerCommandCode, const string& user_name);
    /// For UREG, CHKPASS, MSGSV
    PersistenceLayerCommand(PersistenceLayerCommandCode code, const string& user, const string& broker, const string& channel);
    /// For LOGIN
    PersistenceLayerCommand(PersistenceLayerCommandCode code, const string& user, const string& data);

    string toString(void) const;
    /// This one is initialized automatically. We need to expose it so the function using this class may store the generated
    /// sequence number.
    sequence_t sequence_number;
private:
    string user_name, sender_name;

    // Those are mutually exclusive. {
    string password_or_message;
    string broker_name;
    // }

    string channel_id;
    PersistenceLayerCommandCode request_type;
};
# endif