# include "conf.hpp"
# include "error.hpp"
# include <cstdlib>

using std::getenv;

/**
 * @brief Fetch configuration from environment.
 */
brokerSettings::brokerSettings(void)
{
    // Message relay configuration
    if ( getenv("MESG_RELAY_ADDR") )
	message_relay_address = getenv("MESG_RELAY_ADDR");
    else
	throw brokerError(errorType::configurationError,"There is no MESG_RELAY_ADDR environment variable");

    if (getenv("MESG_RELAY_CONN_TYPE"))
    {
	string conn_type(getenv("MESG_RELAY_CONN_TYPE"));
	if (conn_type == "UNIX")
	    message_relay_connection_type = connectionType::UNIX;
	else if (conn_type == "UDP")
	    message_relay_connection_type = connectionType::UDP;
	else
	    throw brokerError(errorType::configurationError,"There's something wrong with the MESG_RELAY_CONN_TYPE environment variable");
    }
}

connectionInformation brokerSettings::getMessageRelayAddress(void)
{

}

connectionInformation brokerSettings::getPersistenceLayerAddress(void)
{

}
