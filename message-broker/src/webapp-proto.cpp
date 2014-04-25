# include "webapp-proto.hpp"
# include "error.hpp"
# include "conf.hpp"

using std::ostringstream;

/*********************************** Webapp requests ****************************/

namespace
{
    thread_local char current_message[max_message_size];
    const string ok_code = "OK";
    const string fail_code = "FAIL";
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
    sender = MessageOrigin::fromWebApp;
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

	// Chop of the remaining '\n' character before reading the message.
	rqstream.getline(current_message,0);
	rqstream.getline(current_message,max_message_size);

	current_message[max_message_size-1] = 0; // Terminate it in either case.

	message = current_message;

	return;
    } else if ( request_type == WebappRequestCode::registerUser || request_type == WebappRequestCode::logIn )
    {
	rqstream >> user;
	rqstream >> password;

	if ( user.empty() || password.empty() )
	    throw BrokerError(ErrorType::protocolError,"WebappRequest: Missing user or password field.");

	return;
    } else if ( request_type == WebappRequestCode::isOnline )
    {
	rqstream >> user;

	if ( user.empty() )
	    throw BrokerError(ErrorType::protocolError,"WebappRequest: Missing user field.");

	return;
    } else if ( request_type == WebappRequestCode::logOut )
    {
	rqstream >> user;
	rqstream >> channel_id;

	if ( user.empty() || channel_id.empty() )
	    throw BrokerError(ErrorType::protocolError,"WebappRequest: Missing user or channel_id field.");
	return;
    }

    throw BrokerError(ErrorType::unimplemented,"WebappRequest");
}


/****************************** Responses *******************************/

WebappResponse::WebappResponse(sequence_t seq_num, WebappResponseCode type, bool response_status, const string& error_desc, const string& response_data)
    : error_message(error_desc),
      sequence_number(seq_num),
      response_type(type),
      status(response_status)
{
    if ( type == WebappResponseCode::loggedIn )
	payload = response_data;
}

string WebappResponse::toString(void) const
{
    ostringstream ostr;

    ostr << sequence_number << '\n';

    switch ( response_type )
    {
	case WebappResponseCode::acceptedMessage: ostr << "ACCMSG\n" << (status ? ok_code : "FAIL"); break;
	case WebappResponseCode::isOnline: ostr << "UONL\n" << (status ? "Y" : "N"); break;
	case WebappResponseCode::loggedIn: ostr << "LGDIN\n" << (status ? ok_code + "\n" + payload : fail_code); break;
	case WebappResponseCode::loggedOut: ostr << "LGDOUT\n" << (status ? ok_code  : fail_code); break;
	case WebappResponseCode::registeredUser: ostr << "UREGD\n" << (status ? ok_code : fail_code); break;
    }

    if ( ! status )
	ostr << "\n" << error_message;

    return ostr.str();
}
