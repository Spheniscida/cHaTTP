# include "webapp-proto.hpp"
# include "error.hpp"
# include "conf.hpp"

using std::ostringstream;

/*********************************** Webapp requests ****************************/

namespace
{
    thread_local char* current_message;
    const string ok_code = "OK";
    const string fail_code = "FAIL";
}

/**
 * @brief initialize webapp subsystem for current thread.
 *
 * this function's main purpose is allocating a larger block of memory to
 * store incoming messages.
 */
void initWebappProtocolParser(void)
{
    current_message = new char[max_message_size];
}

/**
 * @brief Stream input operator overloaded for WebappRequestCode objects. Used for parsing requests.
 */
istringstream& operator>>(istringstream& stream, WebappRequestCode& code)
{
    string code_string;

    stream >> code_string;

    if ( code_string.empty() )
	throw BrokerError(ErrorType::protocolError,string("The web-app request code could not be parsed: ") + stream.str());
    else if ( code_string == "SNDMSG" )
	code = WebappRequestCode::sendMessage;
    else if ( code_string == "LOGIN" )
	code = WebappRequestCode::logIn;
    else if ( code_string == "LOGOUT" )
	code = WebappRequestCode::logOut;
    else if ( code_string == "UONLQ" )
	code = WebappRequestCode::isOnline;
    else if ( code_string == "UREG" )
	code = WebappRequestCode::registerUser;
    else
	throw BrokerError(ErrorType::protocolError,"Received unknown request code from web app: " + code_string);

    return stream;
}

/**
 * @brief Parse a request from the web application and create an object with that information.
 */
WebappRequest::WebappRequest(const string& request)
{
    parseWebappRequest(request);
}

void WebappRequest::parseWebappRequest(const string& request)
{
    istringstream rqstream(request);

    rqstream >> sequence_number;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"WebappRequest: sequence number could not be read or it was 0, violating proto-specs.");

    rqstream >> request_type;

    if ( request_type == WebappRequestCode::sendMessage )
    {
	rqstream >> user;
	rqstream >> channel_id;
	rqstream >> dest_user;

	// Message
	rqstream.getline(current_message,0);
	rqstream.getline(current_message,max_message_size);

	message = current_message;

	return;
    } else if ( request_type == WebappRequestCode::registerUser || request_type == WebappRequestCode::logIn )
    {
	rqstream >> user;
	rqstream >> password;

	return;
    } else if ( request_type == WebappRequestCode::isOnline || request_type == WebappRequestCode::logOut )
    {
	rqstream >> user;

	return;
    }

    throw BrokerError(ErrorType::unimplemented,"WebappRequest");
}


/****************************** Responses *******************************/

WebappResponse::WebappResponse(WebappResponseCode type, sequence_t seqnum, bool response_status, const string& response_data)
    : response_type(type),
    sequence_number(seqnum),
    status(response_status)
{
    if ( type == WebappResponseCode::loggedIn )
	payload = response_data;
}

string WebappResponse::toString(void)
{
    ostringstream ostr;

    ostr << sequence_number << '\n';

    switch ( response_type )
    {
	case WebappResponseCode::acceptedMessage: ostr << "ACCMSG\n" << (status ? ok_code : "FAIL"); break;
	case WebappResponseCode::isOnline: ostr << "UONL\n" << (status ? "Y" : "N"); break;
	case WebappResponseCode::loggedIn: ostr << "LGDIN\n" << (status ? ok_code + "\n" + payload : fail_code); break;
	case WebappResponseCode::loggedOut: ostr << "LGDOUT"; break;
	case WebappResponseCode::registeredUser: ostr << "UREGD\n" << (status ? ok_code : fail_code); break;
    }

    return ostr.str();
}
