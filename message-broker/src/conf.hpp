# ifndef CONF_HPP
# define CONF_HPP

# include <string>

using std::string;

/// Maximum message size accepted by this program.
extern const unsigned int max_message_size;

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
 */
class BrokerSettings
{
public:
    BrokerSettings(void);

    connectionInformation getMessageRelayAddress(void);
    connectionInformation getPersistenceLayerAddress(void);
private:
    connectionInformation message_relay_info;
    connectionInformation persistence_layer_info;
};

# endif
