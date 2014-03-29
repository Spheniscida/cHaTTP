# ifndef PARSE_PERSISTENT_MESSAGE_HPP
# define PARSE_PERSISTENT_MESSAGE_HPP

# include <string>
# include <vector>
# include <sstream>

using std::string;
using std::vector;
using std::istringstream;

/*
enum class persistentLayerCommands {
    registerUser,
    checkPassword,
    logIn,
    lookUpUser,
    saveMessage,
    getMessages
};
*/

enum class persistenceLayerResponseCode {
    userRegistered,
    passwordChecked,
    loggedIn,
    lookedUpUser,
    savedMessage,
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
struct persistenceLayerResponse
{
    persistenceLayerResponseCode response_type;
    /// A unique sequence number of a transaction which may be used to find and restart an operation.
    unsigned long long sequence_number;
    /// Success?
    bool status;
};

/**
 * @brief Response class for user look-up operations.
 *
 * Additional fields contain the name of the broker and the channel of the user on that broker.
 */
struct persistenceLayerLookupResponse : public persistenceLayerResponse
{
    persistenceLayerLookupResponse(void);

    string broker_name;
    string channel_name;
};

/**
 * @brief Response class for message retrieval
 *
 * If the broker asked for old messages, this class is used, containing the messages sent by the persistence layer.
 */
struct persistenceLayerMessagesResponse : public persistenceLayerResponse
{
    persistenceLayerMessagesResponse(void);

    vector<string> messages;
};

persistenceLayerResponse* parsePersistenceResponse(const string&);

istringstream& operator>>(istringstream& stream, persistenceLayerResponseCode& code);

# endif