# ifndef PROTOCOL_HPP
# define PROTOCOL_HPP

/*
 * The protocol module handles generation of requests and responses, that is
 * conversion between Request URI → Protobuf object and Protobuf object → JSON.
 */

# include <fcgiapp.h>

# include "requesturi.hpp"

# include <webapp.pb.h>

using namespace chattp;

# include <iostream>
# include <atomic>
# include <string>

typedef unsigned long long sequence_t;

extern WebappRequestMessage createRequest(const RequestURI& u, FCGX_Request* request);
extern std::string responseToJSON(const WebappResponseMessage& response);

// :P
// Ensures that the sequence number is handled appropriately.
class WebappRequestMessageFactory
{
public:
    WebappRequestMessageFactory(void) : sequence_number(1) {}
    WebappRequestMessage getWebappRequestMessage(void) { WebappRequestMessage msg; msg.set_sequence_number(sequence_number++); return msg; }

private:
    std::atomic<sequence_t> sequence_number;
};

# endif
