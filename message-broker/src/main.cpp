# include <iostream>

# include "conf.hpp"
# include "error.hpp"
# include "webapp-proto.hpp"
# include "ipc.hpp"
# include "broker-util.hpp"
# include "broker.hpp"

# include "synchronization.hpp"

/**
 * @brief Initialize Message Broker thread.
 *
 * Use this function for each thread because it allocates thread_local data.
 */
void initMessageBrokerThread(void)
{
    initIPC();
}

/**
 * @brief Initialize global state of Message broker.
 *
 * This function should to be called only once for a single instance of
 * this program.
 */
void globalInit(void)
{
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
	    error_log(e.toString());
	} catch (libsocket::socket_exception exc)
	{
	    error_log(exc.mesg);
	    // This is most probably only some error from sndto or rcvfrom -- log it and ignore it.
	}
    }


}

void cleanup_transactions(ProtocolDispatcher& dispatcher)
{
    while ( true )
    {
	std::this_thread::sleep_for(std::chrono::milliseconds(transaction_min_timeout * 1000));
	dispatcher.runTimeoutFinder();
    }

}

int main(int argc, char** argv)
{
    start_time = steady_clock::now();
    // Only one thread yet.
    globalInit();
    initMessageBrokerThread();

    try {
	ProtocolDispatcher dispatcher;

	for ( unsigned int i = 0; i < global_broker_settings.getNumberOfThreads(); i++ )
	{
	    // Thread id starts with 1.
	    std::thread dispatcher_thread([&dispatcher,i]() -> void { startThread(dispatcher,i+1); });
	    dispatcher_thread.detach();
	}

	std::thread timeout_cleanup_thread([&dispatcher]() -> void { cleanup_transactions(dispatcher); });
	// We are thread #0
	startThread(dispatcher,0);

    } catch (BrokerError e)
    {
	error_log(e.toString());
    } catch (libsocket::socket_exception exc)
    {
	error_log(exc.mesg);
    }

    return 0;
}
