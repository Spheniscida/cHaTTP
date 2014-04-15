# ifndef ERROR_HPP
# define ERROR_HPP

# include "conf.hpp"
# include <string>
# include <iostream>

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

    ErrorType type;
    string error_message;
};

inline void debug_log(const string& msg)
{
    if ( debugging_mode )
	std::cerr << "DBG <> " << msg << std::endl;
}

# endif
