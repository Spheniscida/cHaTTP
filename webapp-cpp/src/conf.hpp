# ifndef CONF_HPP
# define CONF_HPP

# include <libsocket/unixclientdgram.hpp>
# include <libsocket/inetserverdgram.hpp>

# include <string>

/// Maximum message size accepted by this program (message: text sent from a user).
const unsigned int max_message_size = 16384;
/// Maximum size of incoming requests/responses (message: protocol message)
const unsigned int max_raw_message_size = 12288;

struct ConnInfo
{
    std::string address;
    std::string port;
    bool isInet;
};

extern int createFastCGISocket(void);

extern ConnInfo getBindAddress(void);
extern ConnInfo getBrokerAddress(void);
extern unsigned int getNThreads(void);

# endif
