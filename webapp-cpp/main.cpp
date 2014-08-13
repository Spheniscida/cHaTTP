# include <iostream>
# include <thread>

# include "src/conf.hpp"
# include "src/error.hpp"
# include "src/fastcgi.hpp"
# include "src/protocol.hpp"
# include "src/ipc.hpp"

# include <stdlib.h>
# include <fcgiapp.h>

int main(void)
{
    try
    {
	int fd = createFastCGISocket();

	FCGInfo info { .fastcgi_sock = fd };

	unsigned int threads = getNThreads();

	MessageEventHandler msg_handler(info);

	auto fastcgi_handler = [&msg_handler]() -> void { msg_handler.fastCGIWorker(); };
	auto broker_handler = [&msg_handler]() -> void { msg_handler.handleResponses(); };

	for ( unsigned int i = 0; i < threads - 1; i++ )
	{
	    std::thread t(fastcgi_handler);
	    std::thread r(broker_handler);
	    t.detach();
	    r.detach();
	}

	std::thread t(fastcgi_handler);
	broker_handler();

    } catch (WebappError e)
    {
	std::cerr << "Error was thrown: " << e.error_message << std::endl;
    } catch (libsocket::socket_exception e)
    {
	std::cerr << "Socket error: " << e.mesg;
    }

    return 0;
}
