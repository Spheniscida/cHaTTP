# include "error.hpp"

/**
 * @brief Constructor
 *
 * This is the brokerError constructor.
 *
 * @param t Takes the kind of error which occurred.
 * @param message An arbitrary, human-readable message with details about the error.
 */
BrokerError::BrokerError(ErrorType t, const string& message)
    : type(t), error_message(message)
{
}

/**
 * @brief Convert error object to printable string
 *
 * @returns A string describing the error, terminated by '\n'. It can be immediately sent to `cerr`.
 */
string BrokerError::toString(void)
{
    string error_type_str;
    string full_message;

    switch ( type )
    {
	case ErrorType::configurationError
	    : error_type_str = "configurationError: ";
	    break;
	case ErrorType::argumentError
	    : error_type_str = "argumentError: ";
	    break;
	case ErrorType::protocolError
	    : error_type_str = "protocolError: ";
    }

    full_message = error_type_str + error_message + "\n";

    return full_message;
}
