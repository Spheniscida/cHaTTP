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
 * Construct messages for one-argument commands: LOOKUP, LOGOUT, GETMESSAGES.
 *
 * @param code The command type.
 * @param user_name The user name to supply to that command.
 *
 * @throws BrokerError for non-matching commands.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user_name)
{
    if ( code != PersistenceRequest::LOOKUP && code != PersistenceRequest::LOGOUT
      && code != PersistenceRequest::GETMESSAGES && code != PersistenceRequest::GETSETTINGS )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected LOOKUP, LOGOUT, GETSETTINGS or GETMESSAGES, but got other command type");

    request_buffer.set_sequence_number(persistence_counter.get());
    request_buffer.set_type(code);

    if ( code == PersistenceRequest::LOOKUP )
    {
	*(request_buffer.add_lookup_users()) = user_name; // Persistence expects user names to be looked up in this field.
    } else
	request_buffer.set_user_name(user_name);
}

/**
 * Construct messages for LOOKUP.
 *
 * @param code The actual type of command.
 * @param user_names The user names to look up.
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const vector<string>& user_names)
{
    if ( code != PersistenceRequest::LOOKUP )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected LOOKUP, but got other command type.");

    request_buffer.set_sequence_number(persistence_counter.get());
    request_buffer.set_type(code);

    for ( const string& u : user_names )
    {
	*(request_buffer.add_lookup_users()) = u;
    }
}

/**
 *
 * @param code The command type
 * @param user The user name
 * @param password The password (this documentation is very useful, isn't it?)
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user, const string& password_or_settings)
{
    if ( code != PersistenceRequest::REGISTER && code != PersistenceRequest::CHECKPASS && code != PersistenceRequest::SAVESETTINGS )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected UREG or CHKPASS, but got other command type.");

    request_buffer.set_sequence_number(persistence_counter.get());
    request_buffer.set_type(code);
    request_buffer.set_user_name(user);
    if ( code != PersistenceRequest::SAVESETTINGS )
	request_buffer.set_password(password_or_settings);
    else
	request_buffer.set_settings(password_or_settings);
}

/**
 * Doing LOGIN messages.
 *
 * @param code The command type.
 * @param user The user name.
 * @param broker The broker handling that user.
 * @param channel The channel of the user.
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user, const string& broker, const string& channel)
{
    request_buffer.set_sequence_number(persistence_counter.get());
    request_buffer.set_type(code);
    request_buffer.set_user_name(user);
    request_buffer.set_broker_name(broker); // Actually unnecessary for logout
    request_buffer.set_channel_id(channel);

    if ( code != PersistenceRequest::LOGIN && code != PersistenceRequest::LOGOUT )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected LOGIN, but got other command type.");

}

/**
 * Creating SAVEMESSAGE requests.
 *
 * @param code The type -- it has to be SAVEMESSAGE.
 * @param message The message as protobuf object.
 *
 * @throws BrokerError If a command type different from SAVEMESSAGE has been supplied.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const chattp::ChattpMessage& message)
{
    if ( code != PersistenceRequest::SAVEMESSAGE )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected SAVEMESSAGE, but got other command type.");

    request_buffer.set_type(code);
    request_buffer.set_sequence_number(persistence_counter.get());
    *request_buffer.mutable_mesg() = message;
}

/**
 * @brief Convert a command object to a string according to the persistence protocol.
 *
 * @returns A string ready to be sent to the persistence layer.
 */
string PersistenceLayerCommand::toString(void) const
{
    return request_buffer.SerializeAsString();
}
