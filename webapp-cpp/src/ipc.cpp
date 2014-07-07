# include "ipc.hpp"

# include <memory>

namespace
{
    thread_local char receiver_buffer[max_raw_message_size];
}

IPC::IPC(void)
{
    remote_info = getBrokerAddress();
    bind_info = getBindAddress();

    if ( ! remote_info.isInet )
    {
	webapp_socket = new unix_dgram_client(bind_info.address);
	webapp_socket->connect(remote_info.address);

	webapp_inet_socket = nullptr;
	isInet = false;
    } else
    {
	webapp_inet_socket = new inet_dgram_server(bind_info.address,bind_info.port,LIBSOCKET_BOTH);

	webapp_socket = nullptr;
	isInet = true;
    }
}

void IPC::sendRequest(const WebappRequestMessage& msg)
{
    string serialized(std::move(msg.SerializeAsString()));

    if ( ! isInet )
    {
	webapp_socket->snd(serialized.data(),serialized.length());
    } else
    {
	webapp_inet_socket->sndto(serialized,remote_info.address,remote_info.port);
    }
}

WebappResponseMessage IPC::receiveResponse(void)
{
    WebappResponseMessage msg;

    if ( ! isInet )
    {
	ssize_t n = webapp_socket->rcv(receiver_buffer,max_raw_message_size);
	msg.ParseFromArray(receiver_buffer,n);
    } else
    {
	ssize_t n = webapp_inet_socket->rcvfrom(receiver_buffer,max_raw_message_size,nullptr,0,nullptr,0,0,true);
	msg.ParseFromArray(receiver_buffer,n);
    }

    return msg;
}
