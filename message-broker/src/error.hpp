# ifndef ERROR_HPP
# define ERROR_HPP

# include <string>

using std::string;

/**
 * @brief enum class containing possible errors.
 */
enum class errorType {
    configurationError, /// There has been some sort of configuration error. Detailed information is in `error_message`.
    protocolError, /// An error in a response the broker received from another daemon
    persistenceLayerError /// Some error in the persistence layer
};

/**
 * @brief class representing any error somewhere in the broker. Intended to be thrown.
 */
struct brokerError
{
    brokerError(errorType t, const string& message);
    string toString(void);

    errorType type;
    string error_message;
};


# endif
