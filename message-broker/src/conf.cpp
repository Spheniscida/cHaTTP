# include "conf.hpp"
# include "error.hpp"
# include <cstdlib>

using std::getenv;

/**
 * @brief Fetch configuration from environment.
 *
 * This class configures the message broker. It uses the following environment variables:
 *
 * - MESG_RELAY_ADDR - The socket address of the message relay daemon
 * - MESG_RELAY_CONN_TYPE - ("UNIX"/"UDP") How to connect to that daemon.
 * - PERSISTENCE_LAYER_ADDRESS - The socket address of the persistence layer daemon
 * - PERSISTENCE_LAYER_CONN_TYPE - ("UNIX"/"UDP") How to connect to that daemon.
 *
 */
brokerSettings::brokerSettings(void)
{
    // Message relay configuration
    if ( getenv("MESG_RELAY_ADDR") )
	message_relay_address = getenv("MESG_RELAY_ADDR");
    else
	throw brokerError(errorType::configurationError,"There is no MESG_RELAY_ADDR environment variable");


    if ( getenv("MESG_RELAY_CONN_TYPE") )
    {
	string conn_type(getenv("MESG_RELAY_CONN_TYPE"));
	if (conn_type == "UNIX")
	    message_relay_connection_type = connectionType::UNIX;
	else if (conn_type == "UDP")
	    message_relay_connection_type = connectionType::UDP;
	else
	    throw brokerError(errorType::configurationError,"There's something wrong with the MESG_RELAY_CONN_TYPE environment variable");
    }

    // Persistence layer (data source) configuration
    if ( getenv("PERSISTENCE_LAYER_ADDRESS") )
	persistence_layer_address = getenv("PERSISTENCE_LAYER_ADDRESS");
    else
	throw brokerError(errorType::configurationError,"There is no PERSISTENCE_LAYER_ADDRESS environment variable");


    if ( getenv("PERSISTENCE_LAYER_CONN_TYPE") )
    {
	string conn_type(getenv("PERSISTENCE_LAYER_CONN_TYPE"));

	if ( conn_type == "UDP" )
	    persistence_layer_connection_type = connectionType::UDP;
	else if ( conn_type == "UNIX" )
	    persistence_layer_connection_type = connectionType::UNIX;
	else
	    throw brokerError(errorType::configurationError,"There's something wrong with the PERSISTENCE_LAYER_CONN_TYPE environment variable");
    }
    else
	throw brokerError(errorType::configurationError,"There is no PERSISTENCE_LAYER_CONN_TYPE environment variable");
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
