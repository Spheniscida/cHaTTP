# include "fcgi-reply.hpp"

# include "ipc.hpp"
# include "transactions.hpp"
# include "protocol.hpp"

# include <webapp.pb.h>

using namespace chattp;

# include <string>

using std::string;


void handleResponses(void)
{
    WebappResponseMessage msg;
    string json_response;

    while ( true )
    {
	main_ipc->receiveResponse(msg);
	SavedTransaction ta = transaction_map.get(msg.sequence_number());

	if ( ! ta.lookup_success )
	    continue;

	json_response = responseToJSON(msg);

	FCGX_PutS("Status: 200 OK\r\n",ta.request->out);
	FCGX_PutS("Content-type: application/json\r\n",ta.request->out);
	FCGX_FPrintF(ta.request->out,"Content-length: %d\r\n",json_response.length());

	FCGX_PutS("\r\n",ta.request->out);

	FCGX_PutS(json_response.c_str(),ta.request->out);
	FCGX_PutS("\n",ta.request->out);

	FCGX_Finish_r(ta.request);

	delete ta.request;
	transaction_map.erase(msg.sequence_number());
    }

}

