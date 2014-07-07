# ifndef IPC_HPP
# define IPC_HPP

# include <libsocket/unixclientdgram.hpp>
# include <libsocket/inetserverdgram.hpp>

using libsocket::unix_dgram_client;
using libsocket::inet_dgram_server;

# include <webapp.pb.h>

using namespace chattp;

# include "conf.hpp"

class IPC
{
    IPC(void);

    WebappResponseMessage receiveResponse(void);
    void sendRequest(const WebappRequestMessage& msg);

private:

    unix_dgram_client* webapp_socket;
    inet_dgram_server* webapp_inet_socket;

    bool isInet;
    ConnInfo bind_info;
    ConnInfo remote_info;
};

# endif
