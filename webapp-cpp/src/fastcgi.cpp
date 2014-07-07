
# include "fastcgi.hpp"
# include "url.hpp"
# include "error.hpp"
# include "transactions.hpp"

# include <iostream>

void fastCGIWorker(FCGInfo info)
{
    FCGX_Init();

    while ( 1 )
    {
        FCGX_Request* request = new FCGX_Request;
        FCGX_InitRequest(request,info.fastcgi_sock,0);

	FCGX_Accept_r(request);

	FCGX_PutS("Content-type: text/plain\r\n\r\n",request->out);

// 	std::cout << FCGX_GetParam("REQUEST_URI",request->envp) << std::endl;

	Url u;

	try
	{
	    u.parseUrl(string(FCGX_GetParam("REQUEST_URI",request->envp)));
	} catch (WebappError e)
	{
	    FCGX_PutS(e.error_message.c_str(),request->out);
	}

        /*
	for ( auto e : u.url_parameters )
	{
	    std::cout << e.first << " <> " << e.second << std::endl;
	}
        */

	FCGX_PutS("xyz :))",request->out);

        FCGX_Finish_r(request);
        delete request;
    }

}
