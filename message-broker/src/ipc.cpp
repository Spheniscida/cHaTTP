# include "ipc.hpp"
# include <cstring>

using std::vector;

namespace
{
    thread_local char* message_receiver_buffer;

    /// This specifies the size of the last protocol message so we only have to reset last_message_size bytes instead of 16k.
    thread_local unsigned int last_message_size;
}

/**
 * @brief Initialize IPC stuff, e.g. buffers.
 *
 * Must be called once per thread.
 */
void initIPC ( void )
{
    message_receiver_buffer = new char[max_raw_message_size];
    last_message_size = max_raw_message_size;
}


Communicator::Communicator (void)
    : e_set(3)
{
    BrokerSettings settings;

    persistence_connection_info.type = settings.getPersistenceLayerBindAddress().type;
    msgrelay_connection_info.type = settings.getMessageRelayBindAddress().type;
    webapp_connection_info.type = settings.getWebappBindAddress().type;

    try {
	if ( persistence_connection_info.type == connectionType::UNIX )
	{
	    inet_persistence_sock = nullptr;
	    unix_persistence_sock = new unix_dgram_server(settings.getPersistenceLayerBindAddress().address);

	    e_set.add_fd(unix_persistence_sock,LIBSOCKET_READ);
	} else if ( persistence_connection_info.type == connectionType::INET )
	{
	    unix_persistence_sock = nullptr;
	    inet_persistence_sock = new inet_dgram_server(settings.getPersistenceLayerBindAddress().address,settings.getPersistenceLayerBindAddress().port,LIBSOCKET_BOTH);

	    e_set.add_fd(inet_persistence_sock,LIBSOCKET_READ);
	}

	if ( msgrelay_connection_info.type == connectionType::UNIX )
	{
	    inet_msgrelay_sock = nullptr;
	    unix_msgrelay_sock = new unix_dgram_server(settings.getMessageRelayBindAddress().address);
	    e_set.add_fd(unix_msgrelay_sock,LIBSOCKET_READ);
	} else if ( msgrelay_connection_info.type == connectionType::INET )
	{
	    unix_msgrelay_sock = nullptr;
	    inet_msgrelay_sock = new inet_dgram_server(settings.getMessageRelayBindAddress().address,settings.getMessageRelayBindAddress().port,LIBSOCKET_BOTH);

	    e_set.add_fd(inet_msgrelay_sock,LIBSOCKET_READ);
	}

	if ( webapp_connection_info.type == connectionType::UNIX )
	{
	    inet_webapp_sock = nullptr;
	    unix_webapp_sock = new unix_dgram_server(settings.getWebappBindAddress().address);

	    e_set.add_fd(unix_webapp_sock,LIBSOCKET_READ);

	} else if ( webapp_connection_info.type == connectionType::INET )
	{
	    unix_webapp_sock = nullptr;
	    inet_webapp_sock = new inet_dgram_server(settings.getWebappBindAddress().address,settings.getWebappBindAddress().port,LIBSOCKET_BOTH);

	    e_set.add_fd(inet_webapp_sock,LIBSOCKET_READ);
	}

    } catch (libsocket::socket_exception e)
    {
	std::cerr << "Caught socket exception: " << e.mesg;
	throw BrokerError(ErrorType::ipcError,"");
    }
}

Communicator::~Communicator (void)
{
    if ( inet_msgrelay_sock )
	delete inet_msgrelay_sock;
    if ( unix_msgrelay_sock )
	delete unix_msgrelay_sock;
    if ( inet_persistence_sock )
	delete inet_persistence_sock;
    if ( unix_persistence_sock )
	delete unix_persistence_sock;
    if ( inet_webapp_sock )
	delete inet_webapp_sock;
    if ( unix_webapp_sock )
	delete unix_webapp_sock;
}

vector<Receivable*> Communicator::receiveMessage (void)
{
    epollset<libsocket::socket>::ready_socks ready_for_recv = e_set.wait();

    unsigned short size = ready_for_recv.size();

    if ( size > 0 && size < 3 ) // Should be the most likely case
    {
	vector<Receivable*> return_vec(ready_for_recv.size(),nullptr);

	for ( unsigned short i = 0; i < ready_for_recv.size(); i++ )
	{
	    if ( getSocketType(ready_for_recv[0]) == connectionType::UNIX )
		return_vec[i] = receiveFromUNIX(dynamic_cast<unix_dgram_server*>(ready_for_recv[i]));
	    else if (getSocketType(ready_for_recv[1]) == connectionType::INET )
		return_vec[i] = receiveFromINET(dynamic_cast<inet_dgram_server*>(ready_for_recv[i]));
	}

	return return_vec;
    } else if ( size == 0 )
    {
	return receiveMessage(); // This is probably some spurious wake-up, so do another call...
    } else
    {
	std::ostringstream errmsg;

	errmsg << "Communicator::receiveMessage: Unprepared for " << size << " ready sockets.";
	throw BrokerError(ErrorType::ipcError,errmsg.str());
    }
}

connectionType Communicator::getSocketType ( libsocket::socket* sock )
{
    if ( sock == unix_persistence_sock || sock == unix_webapp_sock || sock == unix_msgrelay_sock )
	return connectionType::UNIX;
    else return connectionType::INET;
}

Receivable* Communicator::receiveFromUNIX ( unix_dgram_server* sock )
{
    memset(message_receiver_buffer,0,last_message_size);
    last_message_size = sock->rcvfrom(message_receiver_buffer, max_raw_message_size, nullptr, 0);

    if ( sock == unix_webapp_sock )
	return (static_cast<Receivable*>(new WebappRequest(message_receiver_buffer)));
    else if ( sock == unix_persistence_sock )
	return (static_cast<Receivable*>(new PersistenceLayerResponse(message_receiver_buffer)));
    else if ( sock == unix_msgrelay_sock )
	return (static_cast<Receivable*>(new MessageRelayResponse(message_receiver_buffer)));
    else
	throw BrokerError(ErrorType::ipcError,"Communicator::receiveFromUNIX: Unknown socket encountered!");
}

Receivable* Communicator::receiveFromINET ( inet_dgram_server* sock )
{
    memset(message_receiver_buffer,0,last_message_size);
    last_message_size = sock->rcvfrom(message_receiver_buffer, max_raw_message_size, nullptr, 0, nullptr, 0, 0, true);

    if ( sock == inet_webapp_sock )
	return (static_cast<Receivable*>(new WebappRequest(message_receiver_buffer)));
    else if ( sock == inet_persistence_sock )
	return (static_cast<Receivable*>(new PersistenceLayerResponse(message_receiver_buffer)));
    else if ( sock == inet_msgrelay_sock )
	return (static_cast<Receivable*>(new MessageRelayResponse(message_receiver_buffer)));
    else
	throw BrokerError(ErrorType::ipcError,"Communicator::receiveFromINET: Unknown socket encountered!");
}

void Communicator::send ( const PersistenceLayerCommand& cmd )
{
    if ( inet_persistence_sock )
	inet_persistence_sock->sndto(cmd.toString(),persistence_connection_info.address, persistence_connection_info.port);
    else if ( unix_persistence_sock )
	unix_persistence_sock->sndto(cmd.toString(),persistence_connection_info.address);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const PersistenceLayerCommand&): No working socket for persistence.");
}

void Communicator::send ( const WebappResponse& cmd )
{
    if ( unix_webapp_sock )
	unix_webapp_sock->sndto(cmd.toString(),webapp_connection_info.address);
    else if ( inet_webapp_sock )
	inet_webapp_sock->sndto(cmd.toString(),webapp_connection_info.address, webapp_connection_info.port);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const WebappResponse&): No working socket for webapp.");
}

void Communicator::send ( const MessageForRelay& cmd )
{
    if ( unix_msgrelay_sock )
	unix_msgrelay_sock->sndto(cmd.toString(),msgrelay_connection_info.address);
    else if ( inet_msgrelay_sock )
	inet_msgrelay_sock->sndto(cmd.toString(),msgrelay_connection_info.address,msgrelay_connection_info.port);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const MessageForRelay&): No working socket for message relay.");

}

