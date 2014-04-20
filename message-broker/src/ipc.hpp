# ifndef IPC_HPP
# define IPC_HPP

# include <memory>

# include <libsocket/exception.hpp>
# include <libsocket/unixserverdgram.hpp>
# include <libsocket/inetserverdgram.hpp>
# include "epoll.hpp"

# include "conf.hpp"
# include "error.hpp"
# include "persistent.hpp"
# include "webapp-proto.hpp"
# include "message-relay.hpp"

using libsocket::inet_dgram_server;
using libsocket::unix_dgram_server;
using libsocket::epollset;

using std::unique_ptr;

extern void initIPC(void);

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

    void send(const PersistenceLayerCommand& cmd);
    void send(const WebappResponse& cmd);
    void send(const MessageForRelay& cmd);

    unsigned int receiveMessages(std::vector< Receivable* >& return_vec);
private:
    connectionInformation persistence_connection_info, webapp_connection_info, msgrelay_connection_info;
    epollset<libsocket::socket> e_set;

    inet_dgram_server *inet_persistence_sock, *inet_webapp_sock, *inet_msgrelay_sock;
    unix_dgram_server *unix_persistence_sock, *unix_webapp_sock, *unix_msgrelay_sock;

    connectionType getSocketType(libsocket::socket* sock);
    Receivable* receiveFromUNIX(unix_dgram_server* sock);
    Receivable* receiveFromINET(inet_dgram_server* sock);
};

# endif