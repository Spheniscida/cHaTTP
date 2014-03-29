# include "persistent.hpp"
# include "error.hpp"
# include <sstream>
# include <string>
# include <iostream>

using std::istringstream;
using std::string;

/**
 * @brief Construct a response object for a user look-up response from the persistence layer.
 */
persistenceLayerLookupResponse::persistenceLayerLookupResponse(void)
{
    response_type = persistenceLayerResponseCode::lookedUpUser;
}

/**
 * @brief Construct a response containing multiple messages
 */
persistenceLayerMessagesResponse::persistenceLayerMessagesResponse(void)
{
    response_type = persistenceLayerResponseCode::messages;
}


/**
 * @brief Stream operator to parse persistence layer response codes.
 *
 * @todo Re-order the `if`s for better performance?
 */
istringstream& operator>>(istringstream& stream, persistenceLayerResponseCode& code)
{
    string code_string;

    stream >> code_string;

    if ( code_string.empty() )
	throw brokerError(errorType::protocolError,string("The response code could not be parsed: ") + stream.str());

    if ( code_string == "UREGD" )
	code = persistenceLayerResponseCode::userRegistered;
    else if ( code_string == "CHKDPASS" )
	code = persistenceLayerResponseCode::passwordChecked;
    else if ( code_string == "ULKDUP" )
	code = persistenceLayerResponseCode::lookedUpUser;
    else if ( code_string == "LGDIN" )
	code = persistenceLayerResponseCode::loggedIn;
    else if ( code_string == "MSGSVD" )
	code = persistenceLayerResponseCode::savedMessage;
    else if ( code_string == "MSGS" )
	code = persistenceLayerResponseCode::messages;

    return stream;
}

/**
 * @brief Parse a message from the persistence layer.
 *
 * @param r The response string.
 *
 * @returns A pointer to a generic response object. Depending on the value of response_type, that pointer may have to
 * 	be casted to represent a specialized class.
 *
 * **ATTENTION: The pointer returned by this function must be freed (using `delete`) to avoid memory leaks. In this application,
 * it probably wouldn't even make sense to use `unique_ptr`s.**
 */
persistenceLayerResponse* parsePersistenceResponse(const string& r)
{
    unsigned long long seq_num;

    persistenceLayerResponseCode response_type;
    istringstream response(r);

    response >> seq_num;

    if ( seq_num == 0 )
	throw brokerError(errorType::protocolError,"The sequence number could not be read or it was 0, violating proto-specs.");

    response >> response_type;

    if ( response_type == persistenceLayerResponseCode::lookedUpUser )
    {
	string ok;
	response >> ok;

	if ( ok != "OK" )
	    throw brokerError(errorType::persistenceLayerError,string("Persistence layer returned FAIL for user lookup: ") + r);

	persistenceLayerLookupResponse* response_obj = new persistenceLayerLookupResponse;

	response_obj->sequence_number = seq_num;

	response >> response_obj->broker_name;
	response >> response_obj->channel_name;

	if ( response_obj->broker_name.empty() || response_obj->channel_name.empty() )
	{
	    throw brokerError(errorType::protocolError,"Response type was ULKDUP; however, broker and/or channel could not be retrieved");
	    delete response_obj;
	}

	response_obj->status = true;
	return response_obj;
    } else if ( response_type == persistenceLayerResponseCode::messages )
    {
	persistenceLayerMessagesResponse* response_obj = new persistenceLayerMessagesResponse;
	response_obj->sequence_number = seq_num;

	string single_message;

	while ( response.good() )
	{
	    response >> single_message;
	    if ( ! single_message.empty() )
		break;
	}

	response_obj->status = true;
	return response_obj;
    } else
    {
	persistenceLayerResponse* response_obj = new persistenceLayerResponse;
	response_obj->response_type = response_type;
	response_obj->sequence_number = seq_num;

	string okfail;

	response >> okfail;

	if ( okfail == "OK" )
	    response_obj->status = true;
	else if ( okfail == "FAIL" )
	    response_obj->status = false;
	else
	    throw brokerError(errorType::protocolError,"The status returned from the persistence layer was neither OK nor FAIL.");

	response_obj->status = true;

	return response_obj;
    }


    return nullptr;
}
