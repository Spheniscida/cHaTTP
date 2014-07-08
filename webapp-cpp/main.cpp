# include <iostream>
# include <thread>

# include "src/conf.hpp"
# include "src/error.hpp"
# include "src/fastcgi.hpp"
# include "src/protocol.hpp"

# include "src/url.hpp"
# include "src/ipc.hpp"

# include <stdlib.h>
# include <fcgiapp.h>

int main(void)
{
    sequence_number = 1;

    try
    {
	main_ipc = new IPC;
	int fd = createFastCGISocket();

	FCGInfo info { .fastcgi_sock = fd };

	unsigned int threads = getNThreads();

	/*for ( unsigned int i = 0; i < threads - 1; i++ )
	{
	    std::thread t(fastCGIWorker,info);
	    t.detach();
	}*/

	fastCGIWorker(info);

    } catch (WebappError e)
    {
	std::cerr << "Error was thrown: " << e.error_message << std::endl;
    } catch (libsocket::socket_exception e)
    {
	std::cerr << "Socket error: " << e.mesg;
    }

    return 0;
}
