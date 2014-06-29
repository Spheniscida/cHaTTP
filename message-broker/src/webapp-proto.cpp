# include "webapp-proto.hpp"
# include "error.hpp"
# include "conf.hpp"
#include <error.h>

using std::ostringstream;

/*********************************** Webapp requests ****************************/

namespace
{
    const string ok_code = "OK";
    const string fail_code = "FAIL";
}

/**
 * @brief Parse a request from the web application and create an object with that information.
 */
WebappRequest::WebappRequest(const char* buffer, size_t length)
{
    request_buffer.ParseFromArray(buffer,length);
}

/****************************** Responses *******************************/

/**
 * @brief Create a response to webapp for all message types only requiring a boolean status field.
 *
 * Those types are: LOGGEDOUT REGISTERED SENTMESSAGE
 *
 * @param seq_num The sequence number to use in the response.
 * @param type
 * @param response_status
 * @param error_desc This error description is only used and transmitted if (! response_status). It should be containing
 * a error code (number) followed by a comma and a human-readable message, as described in error-codes.mkd.
 *
 * @throws BrokerError (type argumentError) if a wrong `type` has been given.
 */
WebappResponse::WebappResponse(sequence_t seq_num, WebappResponseMessage::WebappResponseType type, bool response_status, string error_desc)
{
    if ( type != WebappResponseMessage::LOGGEDOUT && type != WebappResponseMessage::REGISTERED
      && type != WebappResponseMessage::SENTMESSAGE && type != WebappResponseMessage::SAVEDSETTINGS
      && type != WebappResponseMessage::HEARTBEAT_RECEIVED && type != WebappResponseMessage::CHANGEDPASS )
    {
	throw BrokerError(ErrorType::argumentError,"WebappResponse: Expected LOGGEDOUT/REGISTERED/SENTMESSAGE/SAVEDSETTINGS/HEARTBEAT_RECEIVED/CHANGEDPASS, but got other type.");
    }

    response_buffer.set_type(type);
    response_buffer.set_sequence_number(seq_num);
    response_buffer.set_status(response_status);

    if ( ! response_status )
    {
	unsigned int error_code = removeErrorCode(error_desc);

	response_buffer.set_error_code(error_code);
	response_buffer.set_error_message(error_desc);
    }
}

/**
 * @brief Create a GOTMESSAGES response.
 *
 * @param seq_num The sequence number to use in the response.
 * @param type
 * @param response_status
 * @param begin An iterator pointing to the beginning of a RepeatedPtrField containing several messages.
 * @param end The iterator pointing to the end of the same container.
 * @param error_desc An error description used if (! response_status).
 *
 * @throws BrokerError (type argumentError) if a wrong `type` has been given.
 */
WebappResponse::WebappResponse(sequence_t seq_num,
			       WebappResponseMessage::WebappResponseType type,
			       bool response_status,
			       google::protobuf::RepeatedPtrField< const chattp::ChattpMessage >::iterator begin,
			       google::protobuf::RepeatedPtrField< const chattp::ChattpMessage >::iterator end,
			       string error_desc)
{
    if ( type != WebappResponseMessage::GOTMESSAGES )
    {
	throw BrokerError(ErrorType::argumentError, "WebappResponse: Expected GOTMESSAGES, but got other command type.");
    }

    response_buffer.set_status(response_status);
    response_buffer.set_sequence_number(seq_num);
    response_buffer.set_type(type);

    if ( response_status )
    {
	// Ugly copy. Almost a hack (note the decltype... *sigh*)
	for ( google::protobuf::RepeatedPtrField<const chattp::ChattpMessage>::iterator it = begin; it != end; it++ )
	{
	    *(response_buffer.add_mesgs()) = *it;
	}
    } else
    {
	unsigned int error_code = removeErrorCode(error_desc);

	response_buffer.set_error_code(error_code);
	response_buffer.set_error_message(error_desc);
    }
}

/**
 * @brief Create a USERSTATUS or AUTHORIZED response.
 *
 * @param seq_num The sequence number to use in the response.
 * @param type
 * @param response_status
 * @param online_authorized If the user is (respectively) online or authorized.
 * @param error_desc An error description used if (! response_status).
 *
 * @throws BrokerError (type argumentError) if a wrong `type` has been given.
 */
WebappResponse::WebappResponse(sequence_t seq_num,
			       WebappResponseMessage::WebappResponseType type,
			       bool response_status,
			       bool online_authorized,
			       string error_desc)
{
    if ( type != WebappResponseMessage::USERSTATUS && type != WebappResponseMessage::AUTHORIZED )
    {
	throw BrokerError(ErrorType::argumentError,"WebappResponse: Expected USERSTATUS or AUTHORIZED, but got other type.");
    }

    response_buffer.set_sequence_number(seq_num);
    response_buffer.set_type(type);
    response_buffer.set_status(response_status);

    if ( ! response_status )
    {
	unsigned int error_code = removeErrorCode(error_desc);

	response_buffer.set_error_code(error_code);
	response_buffer.set_error_message(error_desc);
    } else
    { // Only if the status is OK.
	if ( type == WebappResponseMessage::AUTHORIZED )
	    response_buffer.set_authorized(online_authorized);
	else
	    response_buffer.set_online(online_authorized);
    }
}

/**
 * @brief Create a LOGGEDIN response.
 *
 * @param seq_num The sequence number to use in the response.
 * @param type
 * @param response_status
 * @param channel_id The new channel id.
 * @param error_desc An error description used if (! response_status)
 *
 * @throws BrokerError (type argumentError) if a wrong `type` has been given.
 */
WebappResponse::WebappResponse(sequence_t seq_num,
			       WebappResponseMessage::WebappResponseType type,
			       bool response_status,
			       const string& channel_id_or_settings,
			       string error_desc)
{
    if ( type != WebappResponseMessage::LOGGEDIN && type != WebappResponseMessage::GOTSETTINGS )
    {
	throw BrokerError(ErrorType::argumentError,"WebappResponse: Expected LOGGEDIN/GOTSETTINGS, but got other.");
    }

    response_buffer.set_sequence_number(seq_num);
    response_buffer.set_type(type);
    response_buffer.set_status(response_status);

    if ( ! response_status )
    {
	unsigned int error_code = removeErrorCode(error_desc);

	response_buffer.set_error_code(error_code);
	response_buffer.set_error_message(error_desc);

	return;
    }

    if ( type == WebappResponseMessage::LOGGEDIN )
    {
	response_buffer.set_channel_id(channel_id_or_settings);
    } else if ( type == WebappResponseMessage::GOTSETTINGS )
    {
	response_buffer.set_settings(channel_id_or_settings);
    }

}

string WebappResponse::toString(void) const
{
    return response_buffer.SerializeAsString();
}
