# include "persistent.hpp"
# include "error.hpp"
# include <string>
# include <iostream>

using std::istringstream;
using std::ostringstream;
using std::string;

/**
 * @brief Initialize class with the values extracted from `response`.
 *
 * @param response A string containing a response sent from the persistence layer.
 */
PersistenceLayerResponse::PersistenceLayerResponse(const string& response)
{
    message_type = ReceivedMessageType::fromPersistence;
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
 * @brief [DEPRECATED] Parse a message from the persistence layer.
 *
 * @param r The response string.
 *
 * @returns A pointer to a generic response object. Depending on the value of response_type, that pointer may have to
 * 	be casted to represent a specialized class.
 *
 * [DEPRECATED] in favor of PersistenceLayerResponse's constructor.
 *
 * **ATTENTION: The pointer returned by this function must be freed (using `delete`) to avoid memory leaks. In this application,
 * it probably wouldn't even make sense to use `unique_ptr`s.**
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
	    response >> channel_name;

	    if ( broker_name.empty() || channel_name.empty() )
	    {
		throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: type was ULKDUP; however, broker and/or channel could not be retrieved");
	    }

	}
    } else if ( response_type == PersistenceLayerResponseCode::messages )
    {
	string ok;
	response >> ok;

	if ( ok == "FAIL" )
	    status = false;
	else if ( ok == "OK" )
	    status = true;
	else
	    throw BrokerError(ErrorType::protocolError,"parsePersistenceResponse: response status (expected OK|FAIL): " + ok);

	char* single_message = new char[32768];

	response.getline(single_message,0); // Remove the remaining newline character from the previous stream read.

	while ( response.good() )
	{
	    response.getline(single_message,32767);
	    if ( response.fail() )
		break;
	    messages.push_back(string(single_message));
	}

	status = true;
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
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, string user)
    : sequence_number(getNewSequenceNumber()),
    request_type(code),
    user_name(user)
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
 * @param data Either a password (UREG, CHKPASS) or a message (MSGSV).
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, string user, string data)
    : sequence_number(getNewSequenceNumber()),
    request_type(code),
    user_name(user)
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
 * @param broker The broker handling that user.
 * @param channel The channel of the user.
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceLayerCommandCode code, string user, string broker, string channel)
    : sequence_number(getNewSequenceNumber()),
    request_type(code),
    user_name(user),
    broker_name(broker),
    channel_id(channel)
{
    if ( code != PersistenceLayerCommandCode::logIn )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: LOGIN, but got other command type.");
}

/**
 * @brief Convert a command object to a string according to the persistence protocol.
 *
 * @returns A string ready to be sent to the persistence layer.
 */
string PersistenceLayerCommand::toString(void)
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
	case PersistenceLayerCommandCode::saveMessage : out << "MSGSV\n" << user_name << '\n' << password_or_message; break;
	case PersistenceLayerCommandCode::logIn : out << "LOGIN\n" << user_name << '\n' << broker_name << '\n' << channel_id; break;
    }

    return out.str();
}
