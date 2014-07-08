# ifndef PROTOCOL_HPP
# define PROTOCOL_HPP
# include <fcgiapp.h>

# include "url.hpp"

# include <webapp.pb.h>

using namespace chattp;

# include <iostream>
# include <atomic>

typedef unsigned long long sequence_t;

extern std::atomic<sequence_t> sequence_number;

extern WebappRequestMessage makeREGISTERRequest(const Url& u);
extern WebappRequestMessage createRequest(const Url& u, FCGX_Request* request);

# endif
