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
    void sendToWebapp(const WebappResponse& cmd);
    void sendToRelay(const MessageForRelay&);

    unique_ptr<Receivable> receiveMessage(void);
private:
    connectionInformation persistence_info, webapp_info, msgrelay_info;
    epollset<libsocket::socket> e_set;

    inet_dgram_server *inet_persistence, *inet_webapp, *inet_msgrelay;
    unix_dgram_server *unix_persistence, *unix_webapp, *unix_msgrelay;

    static void sendTo(const connectionInformation& dst, const string& msg);
};

# endif