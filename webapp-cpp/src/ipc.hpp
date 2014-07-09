# ifndef IPC_HPP
# define IPC_HPP

# include <mutex>

# include <libsocket/unixclientdgram.hpp>
# include <libsocket/inetserverdgram.hpp>

using libsocket::unix_dgram_client;
using libsocket::inet_dgram_server;

# include <webapp.pb.h>

using namespace chattp;

# include "conf.hpp"

class IPC
{
public:
    IPC(void);

    void receiveResponse(WebappResponseMessage& msg);
    void sendRequest(const WebappRequestMessage& msg);

private:

    unix_dgram_client* webapp_socket;
    inet_dgram_server* webapp_inet_socket;

    std::mutex socket_write_mutex;
    ConnInfo bind_info;
    ConnInfo remote_info;
    bool isInet;
};

extern IPC* main_ipc;

# endif
