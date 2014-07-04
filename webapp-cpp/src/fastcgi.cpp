
# include "fastcgi.hpp"
# include "url.hpp"

# include <iostream>

void fastCGIWorker(FCGInfo info)
{
    FCGX_Init();

    while ( 1 )
    {
        FCGX_Request* request = new FCGX_Request;
        FCGX_InitRequest(request,info.fastcgi_sock,0);

	FCGX_Accept_r(request);

	//std::cout << FCGX_GetParam("REQUEST_URI",request->envp) << std::endl;

	Url u;
        u.parseUrl(string(FCGX_GetParam("REQUEST_URI",request->envp)));

        /*
	for ( auto e : u.url_parameters )
	{
	    std::cout << e.first << " <> " << e.second << std::endl;
	}
        */

	FCGX_PutS("Content-type: text/plain\r\n\r\n",request->out);
	FCGX_PutS("xyz :))",request->out);

        FCGX_Finish_r(request);
        delete request;
    }

}
