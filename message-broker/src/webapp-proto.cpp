# include "webapp-proto.hpp"
# include "error.hpp"

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

WebappRequest::WebappRequest(const string& request)
{
    istringstream rqstream(request);

    rqstream >> sequence_number;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"WebappRequest: sequence number could not be read or it was 0, violating proto-specs.");

    rqstream >> request_type;

    throw BrokerError(ErrorType::unimplemented,"WebappRequest");
}

