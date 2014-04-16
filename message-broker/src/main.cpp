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
}

void communicator_example(void);

int main(int argc, char** argv)
{
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

void communicator_example(void)
{
    Communicator c;

    vector<Receivable*> v = c.receiveMessage();

    if ( v.size() > 0 )
    {
	if ( v[0]->sender == MessageOrigin::fromWebApp )
	{
	    WebappRequest* x = dynamic_cast<WebappRequest*>(v[0]);
	    std::cout << x->sequence_number;
	    std::cout << x->user << x->password;
	    std::cout << "\n";
	}
    }
}
