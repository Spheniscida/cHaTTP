# include "protocol.hpp"
# include "error.hpp"

# include <sstream>

# include <message.pb.h>

# include "utils.hpp"
# include "json.hpp"

namespace
{
    const unsigned int body_buffer_length = 256;
    thread_local char body_buffer[body_buffer_length];
}

WebappRequestMessageFactory request_factory;

WebappRequestMessage makeREGISTERRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in register request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();
    msg.set_type(WebappRequestMessage::REGISTER);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGINRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("password").empty() )
	throw WebappError("Missing parameter in login request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::LOGIN);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));

    return msg;
}

WebappRequestMessage makeLOGOUTRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in login request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::LOGOUT);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage makeSENDMESSAGERequest(const RequestURI& u, FCGX_Request* request)
{
    if ( u.getParameter("user_name").empty()
      || u.getParameter("channel_id").empty()
      || u.getParameter("dest_user").empty() )
	throw WebappError("Missing parameter in send request");

    WebappRequestMessage request_message = request_factory.getWebappRequestMessage();
    string message;
    int read = 0;

    if ( ! u.getParameter("message").empty() )
	message = u.getParameter("message");
    else
    {
	while ( 0 < (read = FCGX_GetStr(body_buffer,body_buffer_length,request->in)) )
	    message.append(body_buffer,read);
    }

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

WebappRequestMessage makeISONLINERequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() )
	throw WebappError("Missing parameter in isonline request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();
    msg.set_type(WebappRequestMessage::QUERYSTATUS);
    msg.set_user_name(u.getParameter("user_name"));

    return msg;
}

WebappRequestMessage makeSETCONFRequest(const RequestURI& u, FCGX_Request* request)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in setconf request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::SAVESETTINGS);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    string settings;
    unsigned int read = 0;

    if ( ! u.getParameter("settings").empty() )
	settings = u.getParameter("settings");
    else
    {
	while ( 0 < (read = FCGX_GetStr(body_buffer,body_buffer_length,request->in)) )
	    settings.append(body_buffer,read);
    }

    msg.set_settings(settings);

    return msg;
}

WebappRequestMessage makeGETCONFRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in getconf request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::GETSETTINGS);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage makeCHANGEPASSRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty()
      || u.getParameter("password").empty()
      || u.getParameter("new_password").empty() )
	throw WebappError("Missing parameter in change_password request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::CHANGEPASS);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_password(u.getParameter("password"));
    msg.set_new_password(u.getParameter("new_password"));

    return msg;
}

WebappRequestMessage makeGETMESSAGESRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in savedmessages request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::GETMESSAGES);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage makeHEARTBEATRequest(const RequestURI& u)
{
    if ( u.getParameter("user_name").empty() || u.getParameter("channel_id").empty() )
	throw WebappError("Missing parameter in heartbeat request");

    WebappRequestMessage msg = request_factory.getWebappRequestMessage();

    msg.set_type(WebappRequestMessage::CHANNEL_HEARTBEAT);
    msg.set_user_name(u.getParameter("user_name"));
    msg.set_channel_id(u.getParameter("channel_id"));

    return msg;
}

WebappRequestMessage createRequest(const RequestURI& u, FCGX_Request* request)
{
    switch ( u.type )
    {
	case RequestURI::REGISTER:
	    return makeREGISTERRequest(u);
	case RequestURI::LOGIN:
	    return makeLOGINRequest(u);
	case RequestURI::LOGOUT:
	    return makeLOGOUTRequest(u);
	case RequestURI::SENDMESSAGE:
	    return makeSENDMESSAGERequest(u,request);
	case RequestURI::ISONLINE:
	    return makeISONLINERequest(u);
	case RequestURI::SETCONF:
	    return makeSETCONFRequest(u,request);
	case RequestURI::GETCONF:
	    return makeGETCONFRequest(u);
	case RequestURI::CHANGEPASS:
	    return makeCHANGEPASSRequest(u);
	case RequestURI::GETMESSAGES:
	    return makeGETMESSAGESRequest(u);
	case RequestURI::HEARTBEAT:
	    return makeHEARTBEATRequest(u);
    }

    throw WebappError("Sorry, but you called an unimplemented request",true);
}

// RESPONSE HANDLING

JSONObject messageToJSONObject(const ChattpMessage& msg)
{
    JSONObject message;

    JSONString sender("sender",msg.sender()), receiver("receiver",msg.receiver()),
	body("body",msg.body()), timestamp("timestamp",msg.timestamp());

    message.addPair(sender);
    message.addPair(receiver);
    message.addPair(body);
    message.addPair(timestamp);

    return message;
}

string responseToJSON(const WebappResponseMessage& response)
{
    JSONObject response_object;

    JSONBoolean status("status",response.status());
    JSONString error("error",response.error_message());
    JSONNumber error_code("error-code",response.error_code());

    response_object.addPair(status);
    response_object.addPair(error);
    response_object.addPair(error_code);

    string type_string;

    switch ( response.type() )
    {
	case WebappResponseMessage::LOGGEDIN:
	{
	    type_string = "logged-in";

	    JSONString channel_id("channel_id",response.channel_id());
	    response_object.addPair(channel_id);
	    break;
	}
	case WebappResponseMessage::LOGGEDOUT:
	{
	    type_string = "logged-out";
	    break;
	}
	case WebappResponseMessage::SENTMESSAGE:
	{
	    type_string = "message-accepted";
	    break;
	}
	case WebappResponseMessage::REGISTERED:
	{
	    type_string = "registered";
	    break;
	}
	case WebappResponseMessage::USERSTATUS:
	{
	    type_string = "online";

	    JSONBoolean online("online",response.online());
	    response_object.addPair(online);
	    break;
	}
	case WebappResponseMessage::GOTMESSAGES:
	{
	    type_string = "saved-messages";

	    JSONObjectList messages("messages");

	    for ( auto it = response.mesgs().begin(); it != response.mesgs().end(); it++ )
	    {
		JSONObject message;

		JSONString sender("sender",it->sender()), receiver("receiver",it->receiver()),
		    body("body",it->body()), timestamp("timestamp",it->timestamp());

		message.addPair(sender);
		message.addPair(receiver);
		message.addPair(body);
		message.addPair(timestamp);

		messages.addValue(message);
	    }

	    response_object.addPair(messages);

	    break;
	}
	case WebappResponseMessage::GOTSETTINGS:
	{
	    type_string = "settings";

	    JSONString settings("settings",response.settings());
	    response_object.addPair(settings);
	    break;
	}
	case WebappResponseMessage::SAVEDSETTINGS:
	{
	    type_string = "saved-settings";
	    break;
	}
	case WebappResponseMessage::HEARTBEAT_RECEIVED:
	{
	    type_string = "heartbeated";
	    break;
	}
	case WebappResponseMessage::CHANGEDPASS:
	{
	    type_string = "changed-password";
	    break;
	}
	default:
	    type_string = "UNIMPLEMENTED";
    }

    JSONString type("type",type_string);

    response_object.addPair(type);

    return response_object.toString();
}
