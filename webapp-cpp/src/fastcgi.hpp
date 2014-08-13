# ifndef FASTCGI_HPP
# define FASTCGI_HPP

# include <string>
// C-linkage is declared in the header file itself.
# include <fcgiapp.h>
# include <webapp.pb.h>

# include "ipc.hpp"
# include "transactions.hpp"

struct FCGInfo
{
    int fastcgi_sock;
};

/**
 * @brief Handles incoming FastCGI (web) requests as well as responses from the
 * message broker via two different event loops.
 *
 */
class MessageEventHandler
{
public:
    MessageEventHandler(FCGInfo info) : fastcgi_socket(info.fastcgi_sock) {};

    void handleResponses(void);
    void fastCGIWorker(void);

private:
    TransactionMap transaction_map;
    int fastcgi_socket;
    IPC main_ipc;
};

extern void replyFastCGI(FCGX_Request* request, unsigned int status, const std::string& body);

# endif
