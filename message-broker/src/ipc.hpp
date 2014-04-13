# ifndef IPC_HPP
# define IPC_HPP

# include <libsocket/unixserverdgram.hpp>
# include <libsocket/inetserverdgram.hpp>
# include <libsocket/epoll.hpp>

# include "conf.hpp"
# include "persistent.hpp"
# include "webapp-proto.hpp"

using libsocket::inet_dgram_server;
using libsocket::unix_dgram_server;

/**
 * @brief Communicator is an additional layer between the message broker and the outside world.
 *
 * It encapsulates socket objects from libsocket and tracks (for example) the address family for each
 * connection. Other parts of the message broker only have to use the generic interface of Communicator.
 */
class Communicator
{
public:
    Communicator(void);
    ~Communicator(void);

    void sendToPersistence(const PersistenceLayerCommand& cmd);
    PersistenceLayerResponse recvFromPersistence(void);

    void sendToWebapp(const WebappResponse& cmd);
    WebappRequest recvFromWebapp(void);

    // To be implemented (especially the classes).
    //void sendToRelay(const MessageForRelay&);
    //MessageRelayResponse recvFromRelay(void);
private:
    connectionType persistence_type;
    connectionType webapp_type;
    connectionType msgrelay_type;

    inet_dgram_server *inet_persistence, *inet_webapp, *inet_msgrelay;
    unix_dgram_server *unix_persistence, *unix_webapp, *unix_msgrelay;
};

# endif