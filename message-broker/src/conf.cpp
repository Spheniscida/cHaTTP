# include "conf.hpp"
# include "error.hpp"
# include "broker-util.hpp"
# include <cstdlib>
# include <sstream>

using std::getenv;

namespace
{
    const char* unix_family = "UNIX";
    const char* inet_family = "INET";
    const string yes_value = "Y";
    const string no_value = "N";

    const char* webapp_addr_env_var	  = "CHATTP_WEBAPP_ADDR";
    const char* webapp_family_env_var	  = "CHATTP_WEBAPP_FAMILY";
    const char* webapp_port_env_var	  = "CHATTP_WEBAPP_PORT";
    const char* mesg_relay_addr_env_var   = "CHATTP_MESG_RELAY_ADDR";
    const char* mesg_relay_family_env_var = "CHATTP_MESG_RELAY_FAMILY";
    const char* mesg_relay_port_env_var   = "CHATTP_MESG_RELAY_PORT";
    const char* persistence_addr_env_var  = "CHATTP_PERSISTENCE_LAYER_ADDR";
    const char* persistence_family_env_var= "CHATTP_PERSISTENCE_LAYER_FAMILY";
    const char* persistence_port_env_var  = "CHATTP_PERSISTENCE_LAYER_PORT";

    const char* broker_webapp_bind_addr_var	= "CHATTP_MSGBROKER_WEBAPP_BIND_ADDR";
    const char* broker_webapp_bind_port_var	= "CHATTP_MSGBROKER_WEBAPP_BIND_PORT";
    const char* broker_msgrelay_bind_addr_var	= "CHATTP_MSGBROKER_MSGRELAY_BIND_ADDR";
    const char* broker_msgrelay_bind_port_var	= "CHATTP_MSGBROKER_MSGRELAY_BIND_PORT";
    const char* broker_persistence_bind_addr_var= "CHATTP_MSGBROKER_PERSISTENCE_BIND_ADDR";
    const char* broker_persistence_bind_port_var= "CHATTP_MSGBROKER_PERSISTENCE_BIND_PORT";

    const char* broker_b2b_bind_address_var	= "CHATTP_MSGBROKER_BROKER_NAME";

    const char* broker_thread_number_var	= "CHATTP_MSGBROKER_NUMBER_THREADS";
    const char* broker_clustered_var		= "CHATTP_MSGBROKER_RUN_CLUSTERED";
}

time_point<steady_clock> start_time;
std::atomic<unsigned int> packets_processed;

thread_local unsigned int thread_id;

BrokerSettings global_broker_settings;

/**
 * @brief Fetch configuration from environment.
 *
 * This class configures the message broker from environment variables.
 *
 */
BrokerSettings::BrokerSettings(void)
{
    try {
	// Fetch services.
	message_relay_info = extractConnInfo(mesg_relay_addr_env_var,mesg_relay_family_env_var,mesg_relay_port_env_var);
	persistence_layer_info = extractConnInfo(persistence_addr_env_var,persistence_family_env_var,persistence_port_env_var);
	webapp_info = extractConnInfo(webapp_addr_env_var,webapp_family_env_var,webapp_port_env_var);

	// Fetch bind info.
	message_relay_bind_info = extractConnInfo(broker_msgrelay_bind_addr_var, mesg_relay_family_env_var, broker_msgrelay_bind_port_var);
	persistence_bind_info = extractConnInfo(broker_persistence_bind_addr_var, persistence_family_env_var, broker_persistence_bind_port_var);
	webapp_bind_info = extractConnInfo(broker_webapp_bind_addr_var, webapp_family_env_var, broker_webapp_bind_port_var);

	extractB2BInfo();

	// Fetch number of threads
	if ( getenv(broker_thread_number_var) )
	{
	    std::istringstream var(getenv(broker_thread_number_var));
	    var >> n_threads;
	} else
	{
	    n_threads = 1;
	}

	if ( getenv(broker_clustered_var) )
	{
	    if ( getenv(broker_clustered_var) == yes_value )
		clustered_mode = true;
	    else
		clustered_mode = false;
	} else
	{
	    clustered_mode = false;
	}

    } catch (BrokerError e)
    {
	error_log(e.toString());
	throw e;
    }
}

void BrokerSettings::extractB2BInfo(void)
{
    if ( getenv(broker_b2b_bind_address_var) )
	b2b_bind_info.address = getenv(broker_b2b_bind_address_var);
    else
	throw BrokerError(ErrorType::configurationError,string("No ") + broker_b2b_bind_address_var + " environment variable available.");

    b2b_bind_info.port = message_broker_port;
    b2b_bind_info.type = connectionType::INET;

    if ( getenv(broker_b2b_bind_address_var) )
	message_broker_name = getenv(broker_b2b_bind_address_var);
    else
	throw BrokerError(ErrorType::configurationError,string("No ") + broker_b2b_bind_address_var + " environment variable available.");

}

connectionInformation BrokerSettings::extractConnInfo (const char* addr_var, const char* family_var, const char* port_var)
{
    connectionInformation conninfo;

    if ( getenv(addr_var) )
	conninfo.address = getenv(addr_var);
    else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + addr_var + " environment variable.");

    if ( getenv(family_var) )
    {
	string conn_type(getenv(family_var));

	if (conn_type == unix_family)
	{
	    conninfo.type = connectionType::UNIX;

	} else if (conn_type == inet_family)
	{
	    conninfo.type = connectionType::INET;

	    if ( getenv(port_var) )
	    {
		conninfo.port = getenv(port_var);
	    } else
		throw BrokerError(ErrorType::configurationError,string("The ") + port_var + " environment variable could not be found.");
	} else
	    throw BrokerError(ErrorType::configurationError,string("There's something wrong with the ") + family_var + " environment variable");
    } else
	throw BrokerError(ErrorType::configurationError,string("There is no ") + family_var + " environment variable");

    return conninfo;
}
