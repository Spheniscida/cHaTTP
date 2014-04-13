# include "ipc.hpp"

Communicator::Communicator ( void )
{
    BrokerSettings settings;

    persistence_type = settings.getPersistenceLayerBindAddress().type;
    msgrelay_type = settings.getMessageRelayBindAddress().type;
    webapp_type = settings.getWebappBindAddress().type;

    if ( persistence_type == connectionType::UNIX )
    {
	inet_persistence = nullptr;
	unix_persistence = new unix_dgram_server(settings.getPersistenceLayerAddress().address);
    } else if ( persistence_type == connectionType::INET )
    {
	unix_persistence = nullptr;
	inet_persistence = new inet_dgram_server(settings.getPersistenceLayerBindAddress().address,settings.getPersistenceLayerBindAddress().port,LIBSOCKET_BOTH);
    }

    if ( msgrelay_type == connectionType::UNIX )
    {
	inet_msgrelay = nullptr;
	unix_msgrelay = new unix_dgram_server(settings.getMessageRelayAddress().address);
    } else if ( msgrelay_type == connectionType::INET )
    {
	unix_msgrelay = nullptr;
	inet_msgrelay = new inet_dgram_server(settings.getMessageRelayBindAddress().address,settings.getMessageRelayBindAddress().port,LIBSOCKET_BOTH);
    }

    if ( webapp_type == connectionType::UNIX )
    {
	inet_webapp = nullptr;
	unix_webapp = new unix_dgram_server(settings.getWebappBindAddress().address);
    } else if ( webapp_type == connectionType::INET )
    {
	unix_webapp = nullptr;
	inet_webapp = new inet_dgram_server(settings.getWebappBindAddress().address,settings.getWebappBindAddress().port,LIBSOCKET_BOTH);
    }
}

Communicator::~Communicator ( void )
{
    if ( inet_msgrelay )
	delete inet_msgrelay;
    if ( unix_msgrelay )
	delete unix_msgrelay;
    if ( inet_persistence )
	delete inet_persistence;
    if ( unix_persistence )
	delete unix_persistence;
    if ( inet_webapp )
	delete inet_webapp;
    if ( unix_webapp )
	delete unix_webapp;
}
