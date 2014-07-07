# include "protocol.hpp"
# include "error.hpp"

std::atomic<sequence_t> sequence_number;

WebappRequestMessage makeREGISTERRequest(const Url& u)
{
    WebappRequestMessage msg;

    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in register request");

    msg.set_type(WebappRequestMessage::REGISTER);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGINRequest(const Url& u)
{
    WebappRequestMessage msg;

    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in login request");

    msg.set_type(WebappRequestMessage::LOGIN);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGOUTRequest(const Url& u)
{
    WebappRequestMessage msg;

    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in login request");

    msg.set_type(WebappRequestMessage::LOGOUT);
    msg.set_sequence_number(sequence_number++);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage createRequest(const Url& u)
{
    switch ( u.type )
    {
	case Url::REGISTER:
	    return makeREGISTERRequest(u);
	case Url::LOGIN:
	    return makeLOGINRequest(u);
	case Url::LOGOUT:
	    return makeLOGOUTRequest(u);
    }
}
