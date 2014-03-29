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
