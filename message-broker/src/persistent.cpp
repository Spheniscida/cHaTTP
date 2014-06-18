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
    if ( code != PersistenceRequest::LOOKUP && code != PersistenceRequest::LOGOUT && code != PersistenceRequest::GETMESSAGES )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: Expected LOOKUP, LOGOUT or GETMESSAGES, but got other command type");

    request.set_sequence_number(sequence_number = getNewSequenceNumber(SequenceCounter::PersistenceCounter));
    request.set_type(code);

    if ( code == PersistenceRequest::LOOKUP )
    {
	*(request.add_lookup_users()) = user_name; // Persistence expects user names to be looked up in this field.
    } else
	request.set_user_name(user_name);
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

    request.set_sequence_number(sequence_number = getNewSequenceNumber(SequenceCounter::PersistenceCounter));
    request.set_type(code);

    for ( const string& u : user_names )
    {
	*(request.add_lookup_users()) = u;
    }
}

/**
 *
 * @param code The command type
 * @param user The user name
 * @param data Either a password (UREG, CHKPASS) or a protobuf-encoded message (MSGSV)
 *
 * @throws BrokerError If a command type has been supplied which has the wrong number of parameters.
 */
PersistenceLayerCommand::PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user, const string& data)
{
    if ( code != PersistenceRequest::REGISTER && code != PersistenceRequest::CHECKPASS && code != PersistenceRequest::SAVEMESSAGE )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: UREG, CHKPASS or MSGSV, but got other command type.");

    request.set_sequence_number(sequence_number = getNewSequenceNumber(SequenceCounter::PersistenceCounter));
    request.set_type(code);
    request.set_user_name(user);

    if ( code != PersistenceRequest::SAVEMESSAGE )
    {
	request.set_password(data);
    }
    else
    {
	chattp::ChattpMessage* mesg = request.mutable_mesg();
	mesg->ParseFromString(data);
    }
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
    request.set_sequence_number(sequence_number = getNewSequenceNumber(SequenceCounter::PersistenceCounter));
    request.set_type(code);
    request.set_user_name(user);
    request.set_broker_name(broker);
    request.set_channel_id(channel);

    if ( code != PersistenceRequest::LOGIN )
	throw BrokerError(ErrorType::argumentError,"PersistenceLayerCommand: LOGIN, but got other command type.");

}

/**
 * @brief Convert a command object to a string according to the persistence protocol.
 *
 * @returns A string ready to be sent to the persistence layer.
 */
string PersistenceLayerCommand::toString(void) const
{
    return request.SerializeAsString();
}
