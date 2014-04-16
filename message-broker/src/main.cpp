# include <iostream>

# include "conf.hpp"
# include "error.hpp"
# include "webapp-proto.hpp"
# include "ipc.hpp"
# include "broker-util.hpp"
# include "broker.hpp"

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
    initializeUrandomSource();
    packets_processed = 0;
}

/**
 * @brief Calculate processed messages per second since start.
 * 
 * "Processed messages" means all protocol requests and responses received and sent by us.
 */
double messagesPerSec(void)
{
    using namespace std::chrono;
    unsigned long long duration = duration_cast<milliseconds>(steady_clock::now() - start_time).count();

    return (static_cast<double>(packets_processed) / duration);
}


int main(int argc, char** argv)
{
    start_time = steady_clock::now();
    // Only one thread yet.
    initMessageBrokerThread();
    initMessageBroker();

    // This is only testing yet.
    try {
	//BrokerSettings b;
    } catch (BrokerError e)
    {
	std::cerr << e.toString();
    }

    return 0;
}
