
# include "fastcgi.hpp"
# include "requesturi.hpp"
# include "error.hpp"
# include "transactions.hpp"

void fastCGIWorker(FCGInfo info)
{
    FCGX_Init();
    RequestURI u;

    while ( 1 )
    {
	FCGX_Request* request = new FCGX_Request;
	FCGX_InitRequest(request,info.fastcgi_sock,0);

	FCGX_Accept_r(request);

	try
	{
	    u.parseUrl(string(FCGX_GetParam("REQUEST_URI",request->envp)));

	    WebappRequestMessage msg(createRequest(u,request));

	    SavedTransaction ta;
	    ta.request = request;

	    transaction_map.insert(msg.sequence_number(),ta);

	    main_ipc->sendRequest(msg);
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
