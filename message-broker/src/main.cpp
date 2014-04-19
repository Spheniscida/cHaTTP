# include <iostream>
# include <thread>

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

void startThread(ProtocolDispatcher& dispatch, unsigned int tid)
{
    thread_id = tid;
    initMessageBrokerThread();

    while ( true )
    {
	try {
	    dispatch.dispatch();
	} catch (BrokerError e)
	{
	    std::cerr << e.toString();
	} catch (libsocket::socket_exception exc)
	{
	    std::cerr << exc.mesg;
	    // This is most probably only some error from sndto or rcvfrom -- log it and ignore it.
// 	    throw exc;
	}
    }


}

int main(int argc, char** argv)
{
    start_time = steady_clock::now();
    // Only one thread yet.
    initMessageBroker();
    initMessageBrokerThread();

    // This is only testing yet.
    try {
	BrokerSettings b;

	ProtocolDispatcher dispatcher;

	for ( unsigned int i = 0; i < number_of_threads; i++ )
	{
	    // Thread id starts with 1.
	    std::thread dispatcher_thread([&dispatcher,i]() -> void { startThread(dispatcher,i+1); });
	    dispatcher_thread.detach();
	}

	// We are thread #0
	startThread(dispatcher,0);

    } catch (BrokerError e)
    {
	std::cerr << e.toString();
    } catch (libsocket::socket_exception exc)
    {
	std::cerr << exc.mesg;
    }

    return 0;
}
