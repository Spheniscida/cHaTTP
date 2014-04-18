# include "error.hpp"
# include <algorithm>
# include <iostream>

using std::cerr;
using std::for_each;

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
	case ErrorType::configurationError:
	    error_type_str = "configurationError: ";
	    break;
	case ErrorType::argumentError:
	    error_type_str = "argumentError: ";
	    break;
	case ErrorType::protocolError:
	    error_type_str = "protocolError: ";
	    break;
	case ErrorType::unimplemented:
	    error_type_str = "UNIMPLEMENTED: ";
	    break;
	case ErrorType::genericImplementationError:
	    error_type_str = "genericImplementationError: ";
	    break;
	case ErrorType::ipcError:
	    error_type_str = "IPC error: ";
	    break;
	default: error_type_str = "Unknown error: ";
    }

    full_message = error_type_str + error_message + "\n";

    return full_message;
}

void debug_log(const string& msg)
{
    if ( debugging_mode )
    {
	string without_newlines = msg;
	for_each(without_newlines.begin(),without_newlines.end(),[](char& c) { c == '\n' ? c = ' ' : 0; });
	cerr << "DBG : " << without_newlines << std::endl;
    }
}