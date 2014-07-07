# include <iostream>
# include <thread>

# include "src/conf.hpp"
# include "src/error.hpp"
# include "src/fastcgi.hpp"

# include "src/url.hpp"

# include <stdlib.h>
# include <fcgiapp.h>

int main(void)
{
    try
    {
	int fd = createFastCGISocket();

	FCGInfo info { .fastcgi_sock = fd };

	std::thread t1(fastCGIWorker,info);
	std::thread t2(fastCGIWorker,info);
	std::thread t3(fastCGIWorker,info);

	t1.detach();
	t2.detach();
	t3.detach();

	fastCGIWorker(info);

    } catch (WebappError e)
    {
	std::cerr << "Error was thrown: " << e.error_message << std::endl;
    }

    return 0;
}
