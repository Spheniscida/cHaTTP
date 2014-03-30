# include "conf.hpp"
# include "error.hpp"
# include "broker-util.hpp"
# include <cstdlib>

using std::getenv;

const char* unix_family = "UNIX";
const char* inet_family = "INET";

const char* mesg_relay_addr_env_var   = "CHATTP_MESG_RELAY_ADDR";
const char* mesg_relay_family_env_var = "CHATTP_MESG_RELAY_FAMILY";
const char* mesg_relay_port_env_var   = "CHATTP_MESG_RELAY_PORT";
const char* persistence_addr_env_var  = "CHATTP_PERSISTENCE_LAYER_ADDR";
const char* persistence_family_env_var= "CHATTP_PERSISTENCE_LAYER_FAMILY";
const char* persistence_port_env_var  = "CHATTP_PERSISTENCE_LAYER_PORT";

/**
 * @brief Fetch configuration from environment.
 *
 * This class configures the message broker from environment variables.
 *
 */
BrokerSettings::BrokerSettings(void)
{
    // Message relay configuration
    if ( getenv(mesg_relay_addr_env_var) )
	message_relay_info.address = getenv(mesg_relay_addr_env_var);
    else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + mesg_relay_addr_env_var + " environment variable");


    if ( getenv(mesg_relay_family_env_var) )
    {
	string conn_type(getenv(mesg_relay_family_env_var));

	if (conn_type == unix_family)
	    message_relay_info.type = connectionType::UNIX;
	else if (conn_type == inet_family)
	{
	    message_relay_info.type = connectionType::INET;

	    if ( getenv(mesg_relay_port_env_var) )
		message_relay_info.port = getenv(mesg_relay_port_env_var);
	    else throw BrokerError(ErrorType::configurationError,string("The ") + mesg_relay_port_env_var + " environment variable could not be found.");
	}
	else
	    throw BrokerError(ErrorType::configurationError,string("There's something wrong with the ") + mesg_relay_family_env_var + " environment variable");
    } else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + mesg_relay_family_env_var + " environment variable");

    // Persistence layer (data source) configuration
    if ( getenv(persistence_addr_env_var) )
	persistence_layer_info.address = getenv(persistence_addr_env_var);
    else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + persistence_addr_env_var + " environment variable");


    if ( getenv(persistence_family_env_var) )
    {
	string conn_type(getenv(persistence_family_env_var));

	if ( conn_type == inet_family )
	{
	    persistence_layer_info.type = connectionType::INET;

	    if ( getenv(persistence_port_env_var) )
		persistence_layer_info.port = getenv(persistence_port_env_var);
	    else
		throw BrokerError(ErrorType::configurationError,string("There is no ") + persistence_port_env_var + " environment variable.");
	}
	else if ( conn_type == unix_family )
	    persistence_layer_info.type = connectionType::UNIX;
	else
	    throw BrokerError(ErrorType::configurationError,string("There's something wrong with the ") + persistence_family_env_var + " environment variable");
    }
    else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + persistence_family_env_var + " environment variable");
}

connectionInformation BrokerSettings::getMessageRelayAddress(void)
{
    return message_relay_info;
}

connectionInformation BrokerSettings::getPersistenceLayerAddress(void)
{
    return persistence_layer_info;
}
