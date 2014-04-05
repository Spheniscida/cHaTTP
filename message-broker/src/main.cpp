# include "conf.hpp"
# include "error.hpp"
# include "webapp-proto.hpp"
# include <iostream>

/**
 * @brief Initialize Message Broker thread.
 *
 * Use this function for each thread because it allocates thread_local data.
 */
void initMessageBrokerThread(void)
{
    initWebapp();
}

int main(int argc, char** argv)
{
    // Only one thread yet.
    initMessageBrokerThread();
    // This is only testing yet.
    try {
	BrokerSettings b;
    } catch (BrokerError e)
    {
	std::cerr << e.toString();
    }
    return 0;
}
