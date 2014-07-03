# ifndef FASTCGI_HPP
# define FASTCGI_HPP

# include <string>

# include <fcgiapp.h>
# include <webapp.pb.h>

struct FCGInfo
{
    int fastcgi_sock;
};

extern void fastCGIWorker(FCGInfo info);
extern void replyFastCGI(FCGX_Request* request, unsigned int status, const std::string& body);

# endif
