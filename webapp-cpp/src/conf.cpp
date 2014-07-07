# include "conf.hpp"
# include "error.hpp"

# include <sys/stat.h>
# include <fcgiapp.h>

namespace {
    const std::string UNIX = "UNIX";
    const std::string INET = "INET";
}

bool broker_socket_unix;
std::string broker_inet_addr, broker_inet_port;

int createFastCGISocket(void)
{
    if ( ! getenv("CHATTP_WEBAPP_FASTCGI_PATH") )
	throw WebappError("Missing CHATTP_WEBAPP_FASTCGI_PATH environment variable.");

    unsigned int fd = FCGX_OpenSocket(getenv("CHATTP_WEBAPP_FASTCGI_PATH"),128);

    // You may want to change/remove this.
    chmod(getenv("CHATTP_WEBAPP_FASTCGI_PATH"),0777);
    return fd;
}

ConnInfo getBrokerAddress(void)
{
    if ( ! getenv("CHATTP_MSGBROKER_WEBAPP_BIND_ADDR") || ! getenv("CHATTP_WEBAPP_FAMILY") )
	throw WebappError("Missing CHATTP_MSGBROKER_WEBAPP_BIND_ADDR or CHATTP_WEBAPP_FAMILY variable");

    if ( UNIX == getenv("CHATTP_WEBAPP_FAMILY") )
    {
	ConnInfo info;
	info.isInet = false;
	info.address = string(getenv("CHATTP_MSGBROKER_WEBAPP_BIND_ADDR"));

	return info;
    } else if ( INET == getenv("CHATTP_WEBAPP_FAMILY") )
    {
	if ( ! getenv("CHATTP_MSGBROKER_WEBAPP_BIND_PORT") )
	    throw WebappError("Missing CHATTP_MSGBROKER_WEBAPP_BIND_PORT variable.");

	ConnInfo info;
	info.isInet = true;
	info.address = string(getenv("CHATTP_MSGBROKER_WEBAPP_BIND_ADDR"));
	info.port = string(getenv("CHATTP_MSGBROKER_WEBAPP_BIND_PORT"));

	return info;
    } else
	throw WebappError("CHATTP_WEBAPP_FAMILY is set to an invalid value (expecting UNIX/INET)");
}

ConnInfo getBindAddress(void)
{
    if ( ! getenv("CHATTP_WEBAPP_FAMILY") || ! getenv("CHATTP_WEBAPP_ADDR") )
	throw WebappError("Missing CHATTP_WEBAPP_FAMILY or CHATTP_WEBAPP_ADDR variable.");

    if ( UNIX == getenv("CHATTP_WEBAPP_FAMILY") )
    {
	ConnInfo info;
	info.isInet = false;
	info.address = string(getenv("CHATTP_WEBAPP_ADDR"));

	return info;
    } else if ( INET == getenv("CHATTP_WEBAPP_FAMILY") )
    {
	if ( ! getenv("CHATTP_WEBAPP_PORT") )
	    throw WebappError("Missing CHATTP_WEBAPP_PORT variable.");

	ConnInfo info;
	info.isInet = true;
	info.address = string(getenv("CHATTP_WEBAPP_ADDR"));
	info.port = string(getenv("CHATTP_WEBAPP_PORT"));

	return info;

    } else
	throw WebappError("CHATTP_WEBAPP_FAMILY is set to an invalid value (expecting UNIX/INET)");
}

unsigned int getNThreads(void)
{
    if ( ! getenv("CHATTP_WEBAPP_N_THREADS") )
	return 1;
    else
	return std::stoi(string(getenv("CHATTP_WEBAPP_N_THREADS")));

}
