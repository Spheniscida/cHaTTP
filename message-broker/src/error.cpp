# include "error.hpp"

/**
 * @brief Constructor
 *
 * This is the brokerError constructor.
 *
 * @param t Takes the kind of error which occurred.
 * @param message An arbitrary, human-readable message with details about the error.
 */
brokerError::brokerError(errorType t, const string& message)
    : type(t), error_message(message)
{
}

/**
 * @brief Convert error object to printable string
 *
 * @returns A string describing the error, terminated by '\n'. It can be immediately sent to `cerr`.
 */
string brokerError::toString(void)
{
    string error_type_str;
    string full_message;

    switch ( type )
    {
	case errorType::configurationError
	    : error_type_str = "configurationError: ";
	    break;
	case errorType::persistenceLayerError
	    : error_type_str = "persistentLayerError: ";
	    break;
	case errorType::protocolError
	    : error_type_str = "protocolError: ";
    }

    full_message = error_type_str + error_message + "\n";

    return full_message;
}
