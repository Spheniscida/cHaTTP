# include "ipc.hpp"
# include <cstring>

using std::vector;

namespace
{
    thread_local char message_receiver_buffer[max_raw_message_size];

    /// This specifies the size of the last protocol message so we only have to reset last_message_size bytes instead of 16k.
    thread_local unsigned int last_message_size;

    thread_local char inet_sender[256];
    thread_local unsigned int last_inet_sender_size;
}

/**
 * @brief Initialize IPC stuff, e.g. buffers.
 *
 * Must be called once per thread.
 */
void initIPC ( void )
{
    last_message_size = max_raw_message_size;
    last_inet_sender_size = 256;
}


Communicator::Communicator (void)
    : e_set(3)
{

    persistence_connection_info = global_broker_settings.getPersistenceLayerAddress();
    msgrelay_connection_info = global_broker_settings.getMessageRelayAddress();
    webapp_connection_info = global_broker_settings.getWebappAddress();
    b2b_connection_info = global_broker_settings.getB2BBindAddress();

    try {

	if ( persistence_connection_info.type == connectionType::UNIX )
	{
	    inet_persistence_sock = nullptr;
	    unix_persistence_sock = new unix_dgram_server(global_broker_settings.getPersistenceLayerBindAddress().address,SOCK_NONBLOCK);

	    e_set.add_fd(unix_persistence_sock,LIBSOCKET_READ);
	} else if ( persistence_connection_info.type == connectionType::INET )
	{
	    unix_persistence_sock = nullptr;
	    inet_persistence_sock = new inet_dgram_server(global_broker_settings.getPersistenceLayerBindAddress().address,global_broker_settings.getPersistenceLayerBindAddress().port,LIBSOCKET_BOTH,SOCK_NONBLOCK);

	    e_set.add_fd(inet_persistence_sock,LIBSOCKET_READ);
	}


	if ( msgrelay_connection_info.type == connectionType::UNIX )
	{
	    inet_msgrelay_sock = nullptr;
	    unix_msgrelay_sock = new unix_dgram_server(global_broker_settings.getMessageRelayBindAddress().address,SOCK_NONBLOCK);
	    e_set.add_fd(unix_msgrelay_sock,LIBSOCKET_READ);
	} else if ( msgrelay_connection_info.type == connectionType::INET )
	{
	    unix_msgrelay_sock = nullptr;
	    inet_msgrelay_sock = new inet_dgram_server(global_broker_settings.getMessageRelayBindAddress().address,global_broker_settings.getMessageRelayBindAddress().port,LIBSOCKET_BOTH,SOCK_NONBLOCK);

	    e_set.add_fd(inet_msgrelay_sock,LIBSOCKET_READ);
	}


	if ( webapp_connection_info.type == connectionType::UNIX )
	{
	    inet_webapp_sock = nullptr;
	    unix_webapp_sock = new unix_dgram_server(global_broker_settings.getWebappBindAddress().address,SOCK_NONBLOCK);

	    e_set.add_fd(unix_webapp_sock,LIBSOCKET_READ);

	} else if ( webapp_connection_info.type == connectionType::INET )
	{
	    unix_webapp_sock = nullptr;
	    inet_webapp_sock = new inet_dgram_server(global_broker_settings.getWebappBindAddress().address,global_broker_settings.getWebappBindAddress().port,LIBSOCKET_BOTH,SOCK_NONBLOCK);

	    e_set.add_fd(inet_webapp_sock,LIBSOCKET_READ);
	}


	if ( b2b_connection_info.type == connectionType::INET && global_broker_settings.getClusteredMode() )
	{
	    inet_b2b_sock = new inet_dgram_server(global_broker_settings.getB2BBindAddress().address,global_broker_settings.getB2BBindAddress().port,LIBSOCKET_BOTH,SOCK_NONBLOCK);

	    e_set.add_fd(inet_b2b_sock,LIBSOCKET_READ);
	} else if ( b2b_connection_info.type == connectionType::UNIX )
	{
	    throw BrokerError(ErrorType::configurationError,"Communicator: The B2B bind address may not be a UNIX-domain address.");
	}

    } catch (libsocket::socket_exception e)
    {
	error_log("Caught socket exception in IPC setup: ", e.mesg);
	throw BrokerError(ErrorType::ipcError,"Caught socket exception.");
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
    if ( inet_b2b_sock )
	delete inet_b2b_sock;
}

unsigned int Communicator::receiveMessages(std::vector<Receivable*>& return_vec)
{
    Receivable* return_value;
    unsigned int received_messages = 0;

    epollset<libsocket::socket>::ready_socks ready_for_recv;

    // Output argument!
    int n_ready = e_set.wait(ready_for_recv);

    for ( unsigned short i = 0; i < n_ready; i++ ) // Most likely: n_ready == 1
    {
	// We receive only one message. epoll works level-triggered here, so we receive the
	// second one immediately on the next call to receiveMessages()
	if ( getSocketType(ready_for_recv[i]) == connectionType::UNIX )
	{
	    // Returns nullptr if there are no messages anymore.
	    // This "algorithm" has to be used because we use edge-triggered epoll.
	    while ( (return_value = receiveFromUNIX(dynamic_cast<unix_dgram_server*>(ready_for_recv[i]))) )
	    {
		// Only resize if the vector is already full -- this is quite unlikely, so we save us some memory operations.
		if ( received_messages >= return_vec.size() )
		    return_vec.resize(1+return_vec.size());

		return_vec[received_messages] = return_value;
		received_messages++;
	    }
	}
	else
	{
	    while ( nullptr != (return_value = receiveFromINET(dynamic_cast<inet_dgram_server*>(ready_for_recv[i]))) )
	    {
		if ( received_messages >= return_vec.size() )
		    return_vec.resize(1+return_vec.size());

		return_vec[received_messages] = return_value;
		received_messages++;
	    }
	}
    }

    return received_messages;
}

connectionType Communicator::getSocketType(libsocket::socket* sock)
{
    if ( sock == unix_persistence_sock || sock == unix_webapp_sock || sock == unix_msgrelay_sock )
	return connectionType::UNIX;
    else return connectionType::INET;
}

Receivable* Communicator::receiveFromUNIX(unix_dgram_server* sock)
{
    int received_size;
    memset(message_receiver_buffer,0,last_message_size);

    received_size = sock->rcvfrom(message_receiver_buffer, max_raw_message_size, nullptr, 0);

    if ( received_size < 0 )
	return nullptr;

    last_message_size = received_size;
    message_receiver_buffer[received_size] = 0;

    if ( debugging_mode ) // string() is expensive
	debug_log("tid ", thread_id," Received message (unix): " + string(message_receiver_buffer));

    packets_processed++;
    if ( sock == unix_webapp_sock )
	return (static_cast<Receivable*>(new WebappRequest(message_receiver_buffer)));
    else if ( sock == unix_persistence_sock )
	return (static_cast<Receivable*>(new PersistenceLayerResponse(message_receiver_buffer,received_size)));
    else if ( sock == unix_msgrelay_sock )
	return (static_cast<Receivable*>(new MessageRelayResponse(message_receiver_buffer)));
    else
	throw BrokerError(ErrorType::ipcError,"Communicator::receiveFromUNIX: Unknown socket encountered!");
}

Receivable* Communicator::receiveFromINET(inet_dgram_server* sock)
{
    int received_size;
    memset(message_receiver_buffer,0,last_message_size+1);
    memset(inet_sender,0,last_inet_sender_size+1);

    received_size = sock->rcvfrom(message_receiver_buffer, max_raw_message_size, inet_sender, 255, nullptr, 0, 0, true);

    if ( received_size < 0 )
	return nullptr;

    last_message_size = received_size;
    last_inet_sender_size = strlen(inet_sender);

    if ( debugging_mode ) // string() is expensive
	debug_log("tid ", thread_id, " Received message (inet): ", string(message_receiver_buffer));

    packets_processed++;
    if ( sock == inet_webapp_sock )
	return (static_cast<Receivable*>(new WebappRequest(message_receiver_buffer)));
    else if ( sock == inet_persistence_sock )
	return (static_cast<Receivable*>(new PersistenceLayerResponse(message_receiver_buffer,received_size)));
    else if ( sock == inet_msgrelay_sock )
	return (static_cast<Receivable*>(new MessageRelayResponse(message_receiver_buffer)));
    else if ( sock == inet_b2b_sock )
	return (static_cast<Receivable*>(new B2BIncoming(message_receiver_buffer,string(inet_sender,last_inet_sender_size))));
    else
	throw BrokerError(ErrorType::ipcError,"Communicator::receiveFromINET: Unknown socket encountered!");
}

void Communicator::send(const PersistenceLayerCommand& cmd)
{
    if ( debugging_mode ) // toString() is expensive
	debug_log("Sent to Persistence Layer: ", cmd.toString());

    if ( inet_persistence_sock )
	inet_persistence_sock->sndto(cmd.toString(),persistence_connection_info.address, persistence_connection_info.port);
    else if ( unix_persistence_sock )
	unix_persistence_sock->sndto(cmd.toString(),persistence_connection_info.address);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const PersistenceLayerCommand&): No working socket for persistence.");

    packets_processed++;
}

void Communicator::send(const WebappResponse& cmd)
{
    if ( debugging_mode ) // toString() is expensive
	debug_log("Sent to WebApp (response): ", cmd.toString());

    if ( unix_webapp_sock )
	unix_webapp_sock->sndto(cmd.toString(),webapp_connection_info.address);
    else if ( inet_webapp_sock )
	inet_webapp_sock->sndto(cmd.toString(),webapp_connection_info.address, webapp_connection_info.port);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const WebappResponse&): No working socket for webapp.");

    packets_processed++;
}

void Communicator::send(const MessageForRelay& cmd)
{
    if ( debugging_mode ) // toString() is expensive
	debug_log("Sent to Message relay: ", cmd.toString());

    if ( unix_msgrelay_sock )
	unix_msgrelay_sock->sndto(cmd.toString(),msgrelay_connection_info.address);
    else if ( inet_msgrelay_sock )
	inet_msgrelay_sock->sndto(cmd.toString(),msgrelay_connection_info.address,msgrelay_connection_info.port);
    else
	throw BrokerError(ErrorType::genericImplementationError,"Communicator::send(const MessageForRelay&): No working socket for message relay.");

    packets_processed++;
}

void Communicator::send(const MessageForB2B& cmd, const string& broker)
{
    if ( debugging_mode ) // toString() is expensive
	debug_log("Sent to broker ", broker, " command ", cmd.toString());

    inet_b2b_sock->sndto(cmd.toString(),broker,message_broker_port);

    packets_processed++;
}
