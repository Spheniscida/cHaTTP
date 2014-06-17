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
    // file-local data
}

/**
 * @brief Initialize class with the values extracted from `response`.
 *
 * @param response A string containing a response sent from the persistence layer.
 */
PersistenceLayerResponse::PersistenceLayerResponse(const char* buffer, size_t length)
{
    response_buffer.ParseFromArray(static_cast<const void*>(buffer),length);
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
