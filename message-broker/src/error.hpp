# ifndef ERROR_HPP
# define ERROR_HPP

# include <string>

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
    argumentError
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


# endif