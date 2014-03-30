# ifndef CONF_HPP
# define CONF_HPP

# include <string>

using std::string;

/**
 * @brief Enumeration for connection type: UNIX/UDP
 */
enum class connectionType { UNIX, UDP };

/**
 * @brief connection information structure
 *
 * This `struct` holds the information needed to identify a remote or local
 * socket.
 */
struct connectionInformation
{
    /// The socket address
    string address;
    /// The connection type
    connectionType type;
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
    string message_relay_address;
    connectionType message_relay_connection_type;

    string persistence_layer_address;
    connectionType persistence_layer_connection_type;

};

# endif
