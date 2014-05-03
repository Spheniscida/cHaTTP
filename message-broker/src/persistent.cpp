# include "persistent.hpp"
# include "error.hpp"
# include <string>
# include <iostream>
# include <memory>

using std::istringstream;
using std::ostringstream;
using std::string;

namespace
{
    thread_local char temp_messages[65536];
}

/**
 * @brief Initialize class with the values extracted from `response`.
 *
 * @param response A string containing a response sent from the persistence layer.
 */
PersistenceLayerResponse::PersistenceLayerResponse(const string& response)
{
    parsePersistenceResponse(response);
}

/**
 * @brief Stream operator to parse persistence layer response codes.
 */
istringstream& operator>>(istringstream& stream, PersistenceLayerResponseCode& code)
{
    string code_string;

    stream >> code_string;

    if ( code_string.empty() )
	throw BrokerError(ErrorType::protocolError,string("The persistence response code could not be parsed: ") + stream.str());
    else if ( code_string == "ULKDUP" )
	code = PersistenceLayerResponseCode::lookedUpUser;
    else if ( code_string == "MSGSVD" )
	code = PersistenceLayerResponseCode::savedMessage;
    else if ( code_string == "CHKDPASS" )
	code = PersistenceLayerResponseCode::passwordChecked;
    else if ( code_string == "UREGD" )
	code = PersistenceLayerResponseCode::userRegistered;
    else if ( code_string == "MSGS" )
	code = PersistenceLayerResponseCode::messages;
    else if ( code_string == "LGDIN" )
	code = PersistenceLayerResponseCode::loggedIn;
    else if ( code_string == "LGDOUT" )
	code = PersistenceLayerResponseCode::loggedOut;
    else
	throw BrokerError(ErrorType::protocolError,"Received unknown response code from persistence: " + code_string);

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
 */
void PersistenceLayerResponse::parsePersistenceResponse(const string& r)
{
    istringstream response(r);

    response >> sequence_number;

    if ( sequence_number == 0 )
	throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: sequence number could not be read or it was 0, violating proto-specs.");

    response >> response_type;

    if ( response_type == PersistenceLayerResponseCode::lookedUpUser )
    {
	string ok;
	response >> ok;

	if ( ok == "OK" )
	{
	    status = true;
	    online = true;
	}
	else if ( ok == "FAIL" )
	{
	    status = false;
	    online = false;
	}
	else if ( ok == "OFFLINE" )
	{
	    status = true;
	    online = false;
	}
	else
	    throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: response status (expected OK|FAIL|OFFLINE): " + ok);

	if ( status && online ) // Additional information is only present when status is "OK"
	{
	    response >> broker_name;
	    response >> channel_id;

	    if ( broker_name.empty() || channel_id.empty() )
	    {
		throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: type was ULKDUP; however, broker and/or channel could not be retrieved");
	    }

	}
    } else if ( response_type == PersistenceLayerResponseCode::messages )
    {
	string ok;
	response >> ok;

	if ( ok == "FAIL" )
	{
	    status = false;
	    return;
	} else if ( ok == "OK" )
	    status = true;
	else
	    throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: response status (expected OK|FAIL): " + ok);

	response.getline(temp_messages,65535); // remove first newline.
	response.getline(temp_messages,65535);

	messages = std::move(string(temp_messages));
    } else
    {
	string ok;

	response >> ok;

	if ( ok == "OK" )
	    status = true;
	else if ( ok == "FAIL" )
	    status = false;
	else
	    throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: response status (expected OK|FAIL): " + ok);
    }
}

/*********************************** Persistence layer commands *************************************/

/**
 *
 * @param code The actual type of command.
 * @param user_name The user name wanted by the specified command
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, const string& user_name)
    : sequence_number(getNewSequenceNumber()),
    user_name(user_name),
    request_type(code)
{
    switch ( code )
    {
	case PersistenceLayerCommandCode::lookUpUser: // falls through
	case PersistenceLayerCommandCode::getMessages: // falls through
	case PersistenceLayerCommandCode::logOut: break;
	default: throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: ULKUP, MSGGT or LOGOUT, but got other command type.");
    }
}

/**
 *
 * @param code The command type
 * @param user The user name
 * @param data Either a password (UREG, CHKPASS)
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, const string& user, const string& data)
    : sequence_number(getNewSequenceNumber()),
    user_name(user),
    request_type(code)
{
    switch ( code )
    {
	case PersistenceLayerCommandCode::registerUser: // falls through
	case PersistenceLayerCommandCode::checkPassword: password_or_message = data; break;
	case PersistenceLayerCommandCode::saveMessage: password_or_message = data; break;
	default: throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: UREG, CHKPASS or MSGSV, but got other command type.");
    }
}

/**
 *
 * @param code The command type.
 * @param user The user name.
 * @param broker_or_message The broker handling that user. If MSGSV, the message.
 * @param channel_or_sender The channel of the user. If MSGSV, then it's the user name of the sender.
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, const string& user, const string& broker_or_message, const string& channel_or_sender)
    : sequence_number(getNewSequenceNumber()),
    user_name(user),
    broker_name(broker_or_message),
    request_type(code)
{
    if ( ! (code == PersistenceLayerCommandCode::logIn || code == PersistenceLayerCommandCode::saveMessage) )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: LOGIN/MSGSV, but got other command type.");
    if ( debugging_mode && channel_or_sender == "" && code == PersistenceLayerCommandCode::saveMessage )
	throw BrokerError(ErrorType::genericImplementationError,"PersistenceLayerCommand: No destination user supplied.");

    if ( code == PersistenceLayerCommandCode::logIn )
	channel_id = channel_or_sender;
    else if ( code == PersistenceLayerCommandCode::saveMessage )
    {
	sender_name = channel_or_sender;
	password_or_message = broker_or_message;
    }
}

/**
 * @brief Convert a command object to a string according to the persistence protocol.
 *
 * @returns A string ready to be sent to the persistence layer.
 */
string PersistenceLayerCommand::toString(void) const
{
    ostringstream out;

    if ( 0 == sequence_number )
	throw BrokerError(ErrorType::protocolError,"PersistenceLayerCommand::toString: Invalid sequence number");

    out << sequence_number << '\n';

    switch ( request_type )
    {
	case PersistenceLayerCommandCode::lookUpUser : out << "ULKUP\n" << user_name; break;
	case PersistenceLayerCommandCode::getMessages : out << "MSGGT\n" << user_name; break;
	case PersistenceLayerCommandCode::logOut : out << "LOGOUT\n" << user_name; break;
	case PersistenceLayerCommandCode::registerUser : out << "UREG\n" << user_name << '\n' << password_or_message; break;
	case PersistenceLayerCommandCode::checkPassword: out << "CHKPASS\n" << user_name << '\n' << password_or_message; break;
	case PersistenceLayerCommandCode::saveMessage : out << "MSGSV\n" << user_name << '\n' << sender_name << '\n' << password_or_message; break;
	case PersistenceLayerCommandCode::logIn : out << "LOGIN\n" << user_name << '\n' << broker_name << '\n' << channel_id; break;
    }

    return out.str();
}
