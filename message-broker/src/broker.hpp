# ifndef BROKER_HPP
# define BROKER_HPP

# include <unordered_map>
# include <memory>

using std::shared_ptr;
using std::unordered_map;
using std::dynamic_pointer_cast;

# include "webapp-proto.hpp"
# include "persistent.hpp"
# include "message-relay.hpp"
# include "ipc.hpp"
# include "error.hpp"

class OutstandingTransaction;

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

    void onWebAppUREG(const WebappRequest& rq);
    void onWebAppLOGIN(const WebappRequest& rq);
    void onWebAppLOGOUT(const WebappRequest& rq);
    void onWebAppSNDMSG(const WebappRequest& rq);
    void onWebAppUONLQ(const WebappRequest& rq);
    void onPersistenceUREGD(const PersistenceLayerResponse& rp);
    void onPersistenceCHKDPASS(const PersistenceLayerResponse& rp);
    void onPersistenceLGDIN(const PersistenceLayerResponse& rp);
    void onPersistenceLGDOUT(const PersistenceLayerResponse& rp);
    void onPersistenceULKDUP(const PersistenceLayerResponse& rp);
    void onPersistenceMSGSVD(const PersistenceLayerResponse& rp);
    void onPersistenceMSGS(const PersistenceLayerResponse& rp);
    void onMessagerelayMSGSNT(const MessageRelayResponse& rp);

private:
    Communicator communicator;

    void handlePersistenceMessage(std::shared_ptr< Receivable > msg);
    void handleWebappMessage(std::shared_ptr<Receivable> msg);
    void handleMessagerelayMessage(std::shared_ptr< Receivable > msg);
};

enum class OutstandingType {
    // for logins
    persistenceCHKDPASS,
    persistenceLGDIN,

    // for logouts
    persistenceLGDOUT,

    // for ULKDUP
    persistenceSndmsgSenderULKDUP,
    persistenceSndmsgReceiverULKDUP,
    persistenceUonlqULKDUP,
    persistenceLogoutULKDUP,
    persistenceUREGD,

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
    /// References another transaction, usually the sequence number of a request from the web application.
    sequence_t original_sequence_number;
};

# endif