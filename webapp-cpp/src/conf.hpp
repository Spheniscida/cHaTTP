# ifndef CONF_HPP
# define CONF_HPP

# include <libsocket/unixclientdgram.hpp>
# include <libsocket/inetserverdgram.hpp>

# include <string>

struct ConnInfo
{
    std::string address;
    std::string port;
    bool isInet;
};

extern int createFastCGISocket(void);

extern ConnInfo getBindAddress(void);
extern ConnInfo getBrokerAddress(void);


# endif
