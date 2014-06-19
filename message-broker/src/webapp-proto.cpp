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

WebappResponse::WebappResponse(sequence_t seq_num, WebappResponseCode type, bool response_status, const string& error_desc, const string& response_data)
    : payload(response_data),
      error_message(error_desc),
      sequence_number(seq_num),
      response_type(type),
      status(response_status)
{
}

string WebappResponse::toString(void) const
{
    ostringstream ostr;

    ostr << sequence_number << '\n';

    bool want_error_message = !status && !error_message.empty();

    switch ( response_type )
    {
	case WebappResponseCode::acceptedMessage:
		ostr << "ACCMSG\n" << (status ? ok_code : "FAIL");
		break;
	case WebappResponseCode::isOnline:
		ostr << "UONL\n" << (status ? "Y" : "N");
		want_error_message = false;
		break;
	case WebappResponseCode::loggedIn:
		ostr << "LGDIN\n" << (status ? ok_code + "\n" + payload : fail_code);
		break;
	case WebappResponseCode::loggedOut:
		ostr << "LGDOUT\n" << (status ? ok_code  : fail_code);
		break;
	case WebappResponseCode::registeredUser:
		ostr << "UREGD\n" << (status ? ok_code : fail_code);
		break;
	case WebappResponseCode::savedMessages:
		ostr << "MSGS\n" << (status ? ok_code + "\n" + payload : fail_code);
		break;
	case WebappResponseCode::isAuthorized:
		ostr << "AUTHD\n" << (status ? "Y" : "N");
                want_error_message = false;
		break;
    }

    if ( want_error_message )
	ostr << "\n" << error_message;

    return ostr.str();
}
