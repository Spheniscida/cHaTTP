# include <iostream>

# include "conf.hpp"
# include "error.hpp"
# include "webapp-proto.hpp"
# include "ipc.hpp"

/**
 * @brief Initialize Message Broker thread.
 *
 * Use this function for each thread because it allocates thread_local data.
 */
void initMessageBrokerThread(void)
{
    initWebappProtocolParser();
    initIPC();
}

/**
 * @brief Initialize global state of Message broker.
 *
 * This function should to be called only once for a single instance of
 * this program.
 */
void initMessageBroker(void)
{
    initializeGlobalSequenceNumber();
}

int main(int argc, char** argv)
{
    // Only one thread yet.
    initMessageBrokerThread();
    initMessageBroker();
    // This is only testing yet.
    try {
	BrokerSettings b;
	// The communicator creates several sockets in /tmp.
	//Communicator c;
    } catch (BrokerError e)
    {
	std::cerr << e.toString();
    }

    return 0;
}
