# ifndef CONF_HPP
# define CONF_HPP

# include <string>
# include <chrono>
# include <atomic>

using std::string;
using std::chrono::time_point;
using std::chrono::steady_clock;

class BrokerSettings;

/****** Those are the important compile-time settings ******/
/// UDP port for broker2broker communication.
const string message_broker_port = "27533";
/// Enables extensive logging
const bool debugging_mode = true;

/// At least for how long should a transaction live in the cache before it is marked as "timed-out"? (in seconds) (Note: A transaction may live at most twice this interval)
const unsigned int transaction_min_timeout = 10;

/// Maximum message size accepted by this program (message: text sent from a user).
const unsigned int max_message_size = 16384;
/// Maximum size of incoming requests/responses (message: protocol message)
const unsigned int max_raw_message_size = 12288;

/******* Just declarations ******/
extern const BrokerSettings global_broker_settings;

/// Thread #. 0 is the main process.
extern thread_local unsigned int thread_id;
/// Time of start-up
extern time_point<steady_clock> start_time;
/// Counter of packets/messages processed (messages = actual messages)
extern std::atomic<unsigned int> packets_processed, messages_processed;

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
    /// The socket address
    string address;
    /// for inet sockets
    string port;
    /// The connection type
    connectionType type;
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

    connectionInformation getMessageRelayAddress(void) const { return message_relay_info; };
    connectionInformation getPersistenceLayerAddress(void) const { return persistence_layer_info; };
    connectionInformation getWebappAddress(void) const { return webapp_info; };

    connectionInformation getMessageRelayBindAddress(void) const { return message_relay_bind_info; };
    connectionInformation getPersistenceLayerBindAddress(void) const { return persistence_bind_info; };
    connectionInformation getWebappBindAddress(void) const { return webapp_bind_info; };
    connectionInformation getB2BBindAddress(void) const { return b2b_bind_info; };

    bool getClusteredMode(void) const { return clustered_mode; }; // inline, is cheap.

    const string& getMessageBrokerName(void) const { return message_broker_name; };

    unsigned int getNumberOfThreads(void) const { return n_threads; };
private:
    void extractB2BInfo(void);

    connectionInformation message_relay_info;
    connectionInformation persistence_layer_info;
    connectionInformation webapp_info;
    connectionInformation message_relay_bind_info;
    connectionInformation persistence_bind_info;
    connectionInformation webapp_bind_info;

    connectionInformation b2b_bind_info;

    connectionInformation extractConnInfo(const char* addr_var, const char* family_var, const char* port_var);

    string message_broker_name;
    unsigned int n_threads;
    bool clustered_mode;
};

# endif
