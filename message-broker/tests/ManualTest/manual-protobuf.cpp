# include <iostream>
# include <algorithm>
# include <libsocket/unixclientdgram.hpp>
# include <unistd.h>
# include <sstream>

# include "persistence.pb.h"

# include <conf.hpp>

using std::getline;
using libsocket::unix_dgram_client;

void fail(const char* msg)
{
    std::cout << "Terminating: " << msg << std::endl;
    exit(1);
}

void persistenceDummy(void)
{
    if ( global_broker_settings.getPersistenceLayerAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(global_broker_settings.getPersistenceLayerAddress().address);

    pid_t pid = fork();

    if ( pid == 0 ) // child -- reading from stdin, sending
    {
        chattp::PersistenceResponse response;

	std::string input;
	char* s = new char[128];
	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;

	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : 0; });

            std::istringstream raw_msg(input);

            unsigned int seq;
            raw_msg >> seq;

            std::string cmd;
            getline(raw_msg,cmd);
            getline(raw_msg,cmd);

            if ( cmd == "UREGD" )
            {
                std::string status;
                getline(raw_msg,status);

                response.Clear();

                response.set_sequence_number(seq);
                response.set_type(chattp::PersistenceResponse::REGISTERED);
                response.set_status(status == "OK");

            } else if ( cmd == "LGDIN" )
            {
                std::string status;
                getline(raw_msg,status);

                response.Clear();

                response.set_sequence_number(seq);
                response.set_type(chattp::PersistenceResponse::LOGGEDIN);
                response.set_status(status == "OK");
            } else if ( cmd == "LGDOUT" )
            {
                std::string status;
                getline(raw_msg,status);

                response.Clear();

                response.set_sequence_number(seq);
                response.set_type(chattp::PersistenceResponse::LOGGEDOUT);
                response.set_status(status == "OK");
            } else if ( cmd == "CHKDPASS" )
            {
                std::string status;
                getline(raw_msg, status);

                response.Clear();

                response.set_sequence_number(seq);
                response.set_type(chattp::PersistenceResponse::CHECKEDPASS);
                response.set_status(status == "OK");
            } else if ( cmd == "ULKDUP" )
            {
                std::string status;
                getline(raw_msg, status);

                response.Clear();
                response.set_sequence_number(seq);
                response.set_type(chattp::PersistenceResponse::LOOKEDUP);

                if ( status == "OFFLINE" )
                {
                    response.set_status(true);
                    chattp::PersistenceResponse_UserLocation* loc = response.add_user_locations();
                    loc->set_online(false);
                } else if ( status == "FAIL" )
                {
                    response.set_status(false);
                } else if ( status == "OK" )
                {
                    response.set_status(true);
                    chattp::PersistenceResponse_UserLocation* loc = response.add_user_locations();
                    loc->set_online(true);

                    std::string broker, chan;

                    getline(raw_msg,broker);
                    getline(raw_msg,chan);

                    loc->set_broker_name(broker);
                    loc->set_channel_id(chan);
                }

            }

            std::cout << response.DebugString() << response.SerializeAsString() << std::endl;
            sock.sndto(response.SerializeAsString(),global_broker_settings.getPersistenceLayerBindAddress().address);

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
    if ( global_broker_settings.getWebappAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(global_broker_settings.getWebappAddress().address);

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

	    sock.sndto(input,global_broker_settings.getWebappBindAddress().address);
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
    if ( global_broker_settings.getMessageRelayAddress().type != connectionType::UNIX )
	fail("wrong address family");

    unix_dgram_client sock(global_broker_settings.getMessageRelayAddress().address);

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

	    sock.sndto(input,global_broker_settings.getMessageRelayBindAddress().address);
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
