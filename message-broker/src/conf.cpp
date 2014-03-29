# include "conf.hpp"
# include "error.hpp"
# include <cstdlib>

using std::getenv;

const char* mesg_relay_addr_env_var  = "CHATTP_MESG_RELAY_ADDR";
const char* mesg_relay_conn_env_var  = "CHATTP_MESG_RELAY_CONN_TYPE";
const char* persistence_addr_env_var = "CHATTP_PERSISTENCE_LAYER_ADDRESS";
const char* persistence_conn_env_var = "CHATTP_PERSISTENCE_LAYER_CONN_TYPE";

/**
 * @brief Fetch configuration from environment.
 *
 * This class configures the message broker. It uses the following environment variables:
 *
 * - CHATTP_MESG_RELAY_ADDR - The socket address of the message relay daemon
 * - CHATTP_MESG_RELAY_CONN_TYPE - ("UNIX"/"UDP") How to connect to that daemon.
 * - CHATTP_PERSISTENCE_LAYER_ADDRESS - The socket address of the persistence layer daemon
 * - CHATTP_PERSISTENCE_LAYER_CONN_TYPE - ("UNIX"/"UDP") How to connect to that daemon.
 *
 */
brokerSettings::brokerSettings(void)
{
    // Message relay configuration
    if ( getenv(mesg_relay_addr_env_var) )
	message_relay_address = getenv(mesg_relay_addr_env_var);
    else
	throw brokerError(errorType::configurationError,"There is no CHATTP_MESG_RELAY_ADDR environment variable");


    if ( getenv(mesg_relay_conn_env_var) )
    {
	string conn_type(getenv(mesg_relay_conn_env_var));
	if (conn_type == "UNIX")
	    message_relay_connection_type = connectionType::UNIX;
	else if (conn_type == "UDP")
	    message_relay_connection_type = connectionType::UDP;
	else
	    throw brokerError(errorType::configurationError,"There's something wrong with the CHATTP_MESG_RELAY_CONN_TYPE environment variable");
    }

    // Persistence layer (data source) configuration
    if ( getenv(persistence_addr_env_var) )
	persistence_layer_address = getenv(persistence_addr_env_var);
    else
	throw brokerError(errorType::configurationError,"There is no CHATTP_PERSISTENCE_LAYER_ADDRESS environment variable");


    if ( getenv(persistence_conn_env_var) )
    {
	string conn_type(getenv(persistence_conn_env_var));

	if ( conn_type == "UDP" )
	    persistence_layer_connection_type = connectionType::UDP;
	else if ( conn_type == "UNIX" )
	    persistence_layer_connection_type = connectionType::UNIX;
	else
	    throw brokerError(errorType::configurationError,"There's something wrong with the CHATTP_PERSISTENCE_LAYER_CONN_TYPE environment variable");
    }
    else
	throw brokerError(errorType::configurationError,"There is no CHATTP_PERSISTENCE_LAYER_CONN_TYPE environment variable");
}

connectionInformation brokerSettings::getMessageRelayAddress(void)
{
    connectionInformation info;

    info.type = message_relay_connection_type;
    info.address = message_relay_address;

    return info;
}

connectionInformation brokerSettings::getPersistenceLayerAddress(void)
{
    connectionInformation info;

    info.type = persistence_layer_connection_type;
    info.address = persistence_layer_address;

    return info;
}
