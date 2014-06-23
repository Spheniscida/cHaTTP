# include <iostream>
# include <algorithm>
# include <libsocket/unixclientdgram.hpp>
# include <unistd.h>
# include <sstream>

# include "persistence.pb.h"
# include "webapp.pb.h"
# include "message.pb.h"
# include "messagerelay.pb.h"

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

            std::string cmd, status;
            getline(raw_msg,cmd);
            getline(raw_msg,cmd);

	    response.Clear();

	    response.set_sequence_number(seq);

            if ( cmd == "UREGD" )
            {
                getline(raw_msg,status);

                response.set_type(chattp::PersistenceResponse::REGISTERED);
                response.set_status(status == "OK");

            } else if ( cmd == "LGDIN" )
            {
                getline(raw_msg,status);

                response.set_type(chattp::PersistenceResponse::LOGGEDIN);
                response.set_status(status == "OK");
            } else if ( cmd == "LGDOUT" )
            {
                getline(raw_msg,status);

                response.set_type(chattp::PersistenceResponse::LOGGEDOUT);
                response.set_status(status == "OK");
            } else if ( cmd == "CHKDPASS" )
            {
                getline(raw_msg, status);

                response.set_type(chattp::PersistenceResponse::CHECKEDPASS);
                response.set_status(status == "OK");

            } else if ( cmd == "ULKDUP" )
            {
                getline(raw_msg, status);

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

		    loc->set_user_name("<dummy>");
                    loc->set_broker_name(broker);
                    loc->set_channel_id(chan);
                }

            } else if ( cmd == "MSGS" )
	    {
		getline(raw_msg,status);

		response.set_type(chattp::PersistenceResponse::GOTMESSAGES);
		response.set_status(status == "OK");

		std::string msgdata[4]; // Sender, receiver, body, timestamp

		for ( unsigned int i = 0; i < 4; i++ )
		    getline(raw_msg,msgdata[i]);

		chattp::ChattpMessage* cmsg = response.add_mesgs();
		cmsg->set_sender(msgdata[0]);
		cmsg->set_receiver(msgdata[1]);
		cmsg->set_body(msgdata[2]);
		cmsg->set_timestamp(msgdata[3]);
	    } else if ( cmd == "MSGSVD" )
	    {
		getline(raw_msg, status);

		response.set_status(status == "OK");
		response.set_type(chattp::PersistenceResponse::SAVEDMESSAGE);
	    } else if ( cmd == "GOTSETTINGS" )
	    {
		getline(raw_msg, status);
		getline(raw_msg, *(response.mutable_settings()));

		response.set_status(status == "OK");
		response.set_type(chattp::PersistenceResponse::GOTSETTINGS);
	    } else if ( cmd == "SAVEDSETTINGS" )
	    {
		getline(raw_msg, status);

		response.set_status(status == "OK");
		response.set_type(chattp::PersistenceResponse::SAVEDSETTINGS);
	    } else
		continue;

            std::cout << response.DebugString() << std::endl;
            sock.sndto(response.SerializeAsString(),global_broker_settings.getPersistenceLayerBindAddress().address);

	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	chattp::PersistenceRequest req;
	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    req.Clear();
	    req.ParseFromString(mesg);

	    std::cout << req.DebugString() << std::endl;
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
	chattp::WebappRequestMessage req;

	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;


	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : 0; });

	    std::istringstream raw_msg(input);

	    unsigned int seq;

	    raw_msg >> seq;

	    std::string cmd, status;

	    getline(raw_msg,cmd);
	    getline(raw_msg,cmd);

	    req.Clear();
	    req.set_sequence_number(seq);

	    if ( cmd == "UREG" )
	    {
		std::string user, pwd;

		getline(raw_msg,user);
		getline(raw_msg,pwd);

		req.set_type(chattp::WebappRequestMessage::REGISTER);
		req.set_user_name(user);
		req.set_password(pwd);
	    } else if ( cmd == "LOGIN" )
	    {
		std::string user, pwd;

		getline(raw_msg,user);
		getline(raw_msg,pwd);

		req.set_type(chattp::WebappRequestMessage::LOGIN);

		req.set_user_name(user);
		req.set_password(pwd);

	    } else if ( cmd == "LOGOUT" )
	    {
		std::string user, chanid;

		getline(raw_msg,user);
		getline(raw_msg,chanid);

		req.set_type(chattp::WebappRequestMessage::LOGOUT);
		req.set_user_name(user);
		req.set_channel_id(chanid);

	    } else if ( cmd == "SNDMSG" )
	    {
		req.set_type(chattp::WebappRequestMessage::SENDMESSAGE);

		chattp::ChattpMessage* mesg = req.mutable_mesg();

		getline(raw_msg,*(mesg->mutable_sender()));
		getline(raw_msg,*(req.mutable_channel_id()));
		getline(raw_msg,*(mesg->mutable_receiver()));
		getline(raw_msg,*(mesg->mutable_body()));
		mesg->set_timestamp("<dummy ts>");

	    } else if ( cmd == "TYPING" )
	    {
		req.set_type(chattp::WebappRequestMessage::SENDMESSAGE);

		chattp::ChattpMessage* mesg = req.mutable_mesg();

		getline(raw_msg,*(mesg->mutable_sender()));
		getline(raw_msg,*(req.mutable_channel_id()));
		getline(raw_msg,*(mesg->mutable_receiver()));
		mesg->set_timestamp("<dummy ts>");
		mesg->set_is_typing(true);

	    } else if ( cmd == "UONLQ" )
	    {
		getline(raw_msg,*(req.mutable_user_name()));

		req.set_type(chattp::WebappRequestMessage::QUERYSTATUS);
	    } else if ( cmd == "ISAUTH" )
	    {
		req.set_type(chattp::WebappRequestMessage::AUTHORIZED);

		getline(raw_msg,*(req.mutable_user_name()));
		getline(raw_msg,*(req.mutable_channel_id()));

	    } else if ( cmd == "MSGGT" )
	    {
		req.set_type(chattp::WebappRequestMessage::GETMESSAGES);

		getline(raw_msg,*(req.mutable_user_name()));
		getline(raw_msg,*(req.mutable_channel_id()));
	    } else if ( cmd == "GETSETTINGS" )
	    {
		req.set_type(chattp::WebappRequestMessage::GETSETTINGS);

		getline(raw_msg,*(req.mutable_user_name()));
		getline(raw_msg,*(req.mutable_channel_id()));
	    } else if ( cmd == "SAVESETTINGS" )
	    {
		req.set_type(chattp::WebappRequestMessage::SAVESETTINGS);

		getline(raw_msg,*(req.mutable_user_name()));
		getline(raw_msg,*(req.mutable_channel_id()));
		getline(raw_msg,*(req.mutable_settings()));
	    } else
		continue;

	    std::cout << req.DebugString() << std::endl;
	    sock.sndto(req.SerializeAsString(),global_broker_settings.getWebappBindAddress().address);
	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	chattp::WebappResponseMessage req;

	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    req.Clear();
	    req.ParseFromString(mesg);

	    std::cout << req.DebugString() << std::endl;
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

	chattp::MessageRelayResponse resp;

	while ( std::cin.good() )
	{
	    std::cin.getline(s,127);
	    input = s;

	    std::for_each(input.begin(),input.end(),[](char& c) { c == ' ' ? c = '\n' : c; });

	    std::istringstream raw_msg(input);

	    unsigned int seq;
	    std::string cmd, status;

	    raw_msg >> seq;

	    resp.Clear();
	    resp.set_sequence_number(seq);

	    getline(raw_msg,cmd);
	    getline(raw_msg,cmd);

	    if ( cmd == "MSGSNT" )
	    {
		getline(raw_msg,status);

		resp.set_type(chattp::MessageRelayResponse::SENTMESSAGE);
		resp.set_status(status == "OK");
	    } else if ( cmd == "DELTDCHAN" )
	    {
		getline(raw_msg,status);

		resp.set_type(chattp::MessageRelayResponse::DELETEDCHANNEL);
		resp.set_status(status == "OK");
	    } else if ( cmd == "CHANCREAT" )
	    {
		getline(raw_msg,status);

		resp.set_type(chattp::MessageRelayResponse::CREATEDCHANNEL);
		resp.set_status(status == "OK");
	    } else
		continue;

	    std::cout << resp.DebugString() << std::endl;

	    sock.sndto(resp.SerializeAsString(),global_broker_settings.getMessageRelayBindAddress().address);
	}
    } else // parent -- receiving, writing to stdout
    {
	std::string mesg, src;
	src.resize(0);

	chattp::MessageRelayResponse resp;

	while ( true )
	{
	    mesg.resize(2048);
	    sock.rcvfrom(mesg,src);

	    resp.ParseFromString(mesg);

	    std::cout << resp.DebugString() << std::endl;
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
