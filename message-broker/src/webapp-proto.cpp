# include "webapp-proto.hpp"
# include "error.hpp"

istringstream& operator>>(istringstream& stream, WebappRequestCode& code)
{
    string code_string;

    stream >> code_string;

    if ( code_string.empty() )
	throw BrokerError(ErrorType::protocolError,string("The response code could not be parsed: ") + stream.str());
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