
# include "fastcgi.hpp"
# include "url.hpp"
# include "error.hpp"
# include "transactions.hpp"

IPC* main_ipc;

void fastCGIWorker(FCGInfo info)
{
    FCGX_Init();

    while ( 1 )
    {
	FCGX_Request* request = new FCGX_Request;
	FCGX_InitRequest(request,info.fastcgi_sock,0);

	FCGX_Accept_r(request);

	std::cout << FCGX_GetParam("REQUEST_URI",request->envp) << std::endl;

	Url u;

	try
	{
	    u.parseUrl(string(FCGX_GetParam("REQUEST_URI",request->envp)));
	    WebappRequestMessage msg(createRequest(u));
	    main_ipc->sendRequest(msg);

	    SavedTransaction ta;
	    ta.request = *request;

	    transaction_map.insert(msg.sequence_number(),ta);
	} catch (WebappError e)
	{
	    FCGX_PutS((string("Status: 400 ") + e.error_message).c_str(),request->out);
	    FCGX_PutS("Content-type: text/plain\r\n\r\n",request->out);
	    FCGX_PutS(e.error_message.c_str(),request->out);

	    FCGX_Finish_r(request);
	    delete request;
	    continue;
	}

	FCGX_PutS("Content-type: text/plain\r\n\r\n",request->out);
	FCGX_PutS("",request->out);

        delete request;
    }

}
