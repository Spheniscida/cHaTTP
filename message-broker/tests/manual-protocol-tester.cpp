# include <iostream>
# include <algorithm>
# include <libsocket/unixclientdgram.hpp>
# include <unistd.h>

# include <conf.hpp>

using libsocket::unix_dgram_client;

BrokerSettings config;

void fail(const char* msg)
{
    std::cout << "Terminating: " << msg << std::endl;
    exit(1);
}

void persistenceDummy(void)
{
    if ( config.getPersistenceLayerAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(config.getPersistenceLayerAddress().address);

    pid_t pid = fork();

    if ( pid == 0 ) // child -- reading from stdin, sending
    {
	std::string input;
	char* s = new char[128];
	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;

	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : 0; });

	    sock.sndto(input,config.getPersistenceLayerBindAddress().address);
	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    std::for_each(mesg.begin(),mesg.end(),[](char& c) { c == '\n' ? c = ' ' : 0; });

	    std::cout << mesg << std::endl;
	}
    }
}

void webappDummy(void)
{
    if ( config.getWebappAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(config.getWebappAddress().address);

    pid_t pid = fork();

    if ( pid == 0 ) // child -- reading from stdin, sending
    {
	std::string input;
	char* s = new char[128];
	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;

	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : 0; });

	    sock.sndto(input,config.getWebappBindAddress().address);
	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    std::for_each(mesg.begin(),mesg.end(),[](char& c) { c == '\n' ? c = ' ' : 0; });

	    std::cout << mesg << std::endl;
	}
    }
}

void messagerelayDummy(void)
{
    if ( config.getMessageRelayAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(config.getMessageRelayAddress().address);

    pid_t pid = fork();

    if ( pid == 0 ) // child -- reading from stdin, sending
    {
	std::string input;
	char* s = new char[128];
	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;

	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : c; });

	    sock.sndto(input,config.getMessageRelayBindAddress().address);
	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    std::for_each(mesg.begin(),mesg.end(),[](char& c) { c == '\n' ? c = ' ' : c; });

	    std::cout << mesg << std::endl;
	}
    }
}

int main(int argc, char** argv)
{
    if ( argc == 0 )
	fail("Usage: manual-tester <p|w|m>");

    // argv[1] is the important argument.
    if ( argv[1][0] == 'p' ) // Persistence
	persistenceDummy();
    else if ( argv[1][0] == 'w' ) // Webapp
	webappDummy();
    else if ( argv[1][0] == 'm' ) // Message relay
	messagerelayDummy();

}