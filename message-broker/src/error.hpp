# ifndef ERROR_HPP
# define ERROR_HPP

# include "conf.hpp"
# include "broker-util.hpp"
# include "synchronization.hpp"

# include <string>
# include <chrono>

using namespace std::chrono;
using std::string;

/**
 * @brief enum class containing possible errors.
 */
enum class ErrorType {
    /// There has been some sort of configuration error. Detailed information is in `error_message`.
    configurationError,
    /// An error in a response the broker received from another daemon
    protocolError,
    /// An error caused by wrong parameters to a function which is specialized depending on an argument (i.e. PersistenceLayerCommand's ctor)
    argumentError,
    /// Hit an unimplemented area
    unimplemented,
    /// Network/socket errors
    ipcError,
    /// Any implementation error -- something that really shouldn't happen.
    genericImplementationError
};

/**
 * @brief class representing any error somewhere in the broker. Intended to be thrown.
 */
struct BrokerError
{
    BrokerError(ErrorType t, const string& message);
    string toString(void);

    string error_message;
    ErrorType type;
};

/**
 * @brief Synchronized debugging output.
 *
 * debug_log() uses a mutex to control access to stdout.
 */
template<typename ... Ts>
void debug_log(Ts... args)
{
    if ( debugging_mode )
    {
	time_point<steady_clock> now(steady_clock::now());
	microseconds diff = duration_cast<microseconds>(now - start_time);

	lock_guard<mutex> output_lock(output_mutex);

	std::cerr << diff.count() % 1000000 << " - DBG : ";
	stderrWrite(args...);
    }
}

template<typename ... Ts>
void error_log(Ts... args)
{
    lock_guard<mutex> output_lock(output_mutex);

    std::cerr << "ERR : ";
    stderrWrite(args...);
}
# endif
