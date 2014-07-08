# include "protocol.hpp"
# include "error.hpp"

# include <sstream>

# include <message.pb.h>

# include "utils.hpp"

std::atomic<sequence_t> sequence_number;

namespace
{
    const unsigned int body_buffer_length = 256;
    thread_local char body_buffer[body_buffer_length];
}

WebappRequestMessage makeREGISTERRequest(const Url& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in register request");

    WebappRequestMessage msg;
    msg.set_type(WebappRequestMessage::REGISTER);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGINRequest(const Url& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in login request");

    WebappRequestMessage msg;

    msg.set_type(WebappRequestMessage::LOGIN);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGOUTRequest(const Url& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in login request");

    WebappRequestMessage msg;
    msg.set_type(WebappRequestMessage::LOGOUT);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage makeSENDMESSAGERequest(const Url& u, FCGX_Request* request)
{
    if ( u.getParameter("user_name").empty()
      || u.getParameter("channel_id").empty()
      || u.getParameter("dest_user").empty() )
	throw WebappError("Missing parameter in send request");

    WebappRequestMessage request_message;
    string message;
    int read = 0;

    if ( ! u.getParameter("message").empty() )
	message = u.getParameter("message");
    else
    {
	while ( 0 < (read = FCGX_GetStr(body_buffer,body_buffer_length,request->in)) )
	    message.append(body_buffer,read);
    }

    request_message.set_sequence_number(sequence_number++);
    request_message.set_type(WebappRequestMessage::SENDMESSAGE);
    request_message.set_user_name(u.getParameter("user_name"));
    request_message.set_channel_id(u.getParameter("channel_id"));

    ChattpMessage msg;
    msg.set_sender(u.getParameter("user_name"));
    msg.set_receiver(u.getParameter("dest_user"));
    msg.set_body(message);
    msg.set_timestamp(get2822TimeStamp());

    (*request_message.mutable_mesg()) = msg;

    return request_message;
}

WebappRequestMessage makeISONLINERequest(const Url& u)
{
    if ( u.getParameter("user_name").empty() )
	throw WebappError("Missing parameter in isonline request");

    WebappRequestMessage msg;
    msg.set_type(WebappRequestMessage::QUERYSTATUS);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_sequence_number(sequence_number++);

    return msg;
}

WebappRequestMessage createRequest(const Url& u, FCGX_Request* request)
{
    switch ( u.type )
    {
	case Url::REGISTER:
	    return makeREGISTERRequest(u);
	case Url::LOGIN:
	    return makeLOGINRequest(u);
	case Url::LOGOUT:
	    return makeLOGOUTRequest(u);
	case Url::SENDMESSAGE:
	    return makeSENDMESSAGERequest(u,request);
	case Url::ISONLINE:
	    return makeISONLINERequest(u);
    }

    throw WebappError("Sorry, but you called an unimplemented request");
}
