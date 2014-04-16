# ifndef BROKER_HPP
# define BROKER_HPP

# include <unordered_map>

using std::unordered_map;

# include "webapp-proto.hpp"
# include "persistent.hpp"
# include "message-relay.hpp"
# include "ipc.hpp"

class OutstandingTransaction;

// NOTE: Introduce locks (mutexes) for multithreaded operation!
extern unordered_map<sequence_t,OutstandingTransaction> transactions;
extern unordered_map<sequence_t,WebappRequest> webapp_requests;

/**
 * @brief Broker "main" class
 *
 * This class contains methods and data allowing it to handle incoming requests and responses.
 * The dispatch() function will do most of the work.
 */
class ProtocolDispatcher
{
public:

    void dispatch(void);

    void onWebAppLOGIN(const WebappRequest& rq);
    void onPersistenceCHKDPASS(const PersistenceLayerResponse& rp);
    void onPersistenceLGDIN(const PersistenceLayerResponse& rp);
    void onPersistenceULKDUP(const PersistenceLayerResponse& rp);

private:
    Communicator communicator;
};

enum class OutstandingType {
    // for logins
    persistenceCHKDPASS,
    persistenceLGDIN,

    // for ULKDUP
    persistenceSndmsgSenderULKDUP,
    persistenceSndmsgReceiverULKDUP,
    persistenceUonlqULKDUP,

    // for MSGSV
    persistenceMSGSVD,

    // for MSGSNT
    messagerelayMSGSNT
};

/**
 * An awaited request.
 */
class OutstandingTransaction
{
public:
    OutstandingType type;
    /// References another transaction, e.g. a SNDMSG request from the web application.
    sequence_t original_sequence_number;
};

# endif