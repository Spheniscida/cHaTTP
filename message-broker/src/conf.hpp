# ifndef CONF_HPP
# define CONF_HPP

# include <string>

using std::string;

/// Maximum message size accepted by this program (message: text sent from a user).
extern const unsigned int max_message_size;
/// Maximum size of incoming requests/responses (message: protocol message)
extern const unsigned int max_raw_message_size;

/**
 * @brief Enumeration for connection type: UNIX/UDP
 */
enum class connectionType { UNIX, INET };

/**
 * @brief connection information structure
 *
 * This `struct` holds the information needed to identify a remote or local
 * socket.
 */
struct connectionInformation
{
    /// The connection type
    connectionType type;
    /// The socket address
    string address;
    /// for inet sockets
    string port;
};

/**
 * @brief broker settings class.
 *
 * This class fetches various settings from environment variables and provides
 * them to other parts of the program.
 *
 * The normal *info variables provide information on the other services used by message broker,
 * the *bind_info variables store the binding address for the sockets communicating with mentioned services.
 */
class BrokerSettings
{
public:
    BrokerSettings(void);

    connectionInformation getMessageRelayAddress(void) { return message_relay_info; };
    connectionInformation getPersistenceLayerAddress(void) { return persistence_layer_info; };
    connectionInformation getWebappAddress(void) { return webapp_info; };

    connectionInformation getMessageRelayBindAddress(void) { return message_relay_bind_info; };
    connectionInformation getPersistenceLayerBindAddress(void) { return persistence_bind_info; };
    connectionInformation getWebappBindAddress(void) { return webapp_bind_info; };
private:
    connectionInformation message_relay_info;
    connectionInformation persistence_layer_info;
    connectionInformation webapp_info;
    connectionInformation message_relay_bind_info;
    connectionInformation persistence_bind_info;
    connectionInformation webapp_bind_info;

    connectionInformation extractConnInfo(const char* addr_var, const char* family_var, const char* port_var);
    bool checkSanity(void);
};

# endif
