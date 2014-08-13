
# include "fastcgi.hpp"
# include "requesturi.hpp"
# include "error.hpp"
# include "transactions.hpp"
# include "ipc.hpp"
# include "protocol.hpp"

# include <webapp.pb.h>

using namespace chattp;

# include <string>

using std::string;

void MessageEventHandler::fastCGIWorker(void)
{
    FCGX_Init();
    RequestURI u;

    while ( 1 )
    {
	FCGX_Request* request = new FCGX_Request;
	FCGX_InitRequest(request,fastcgi_socket,0);

	FCGX_Accept_r(request);

	try
	{
	    u.parseUrl(string(FCGX_GetParam("REQUEST_URI",request->envp)));

	    WebappRequestMessage msg(createRequest(u,request));

	    SavedTransaction ta;
	    ta.request = request;

	    transaction_map.insert(msg.sequence_number(),ta); // ta.lookup_success will be set to true

	    main_ipc.sendRequest(msg);
	} catch (WebappError e)
	{
	    if ( ! e.server_error )
		FCGX_PutS((string("Status: 400 ") + e.error_message).c_str(),request->out);
	    else
		FCGX_PutS((string("Status: 500 ") + e.error_message).c_str(),request->out);

	    FCGX_PutS("\r\n",request->out);
	    FCGX_PutS("Content-type: text/plain\r\n\r\n",request->out);
	    FCGX_PutS(e.error_message.c_str(),request->out);
	    FCGX_PutChar('\n',request->out);

	    FCGX_Finish_r(request);
	    delete request;
	    continue;
	} catch (libsocket::socket_exception e)
	{
	    FCGX_PutS("Status: 500 Couldn't reach broker\r\n\r\n",request->out);
	    FCGX_Finish_r(request);
	    delete request;
	    continue;
	}
    }
}

void MessageEventHandler::handleResponses(void)
{
    WebappResponseMessage msg;
    string json_response;

    while ( true )
    {
	main_ipc.receiveResponse(msg);
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

