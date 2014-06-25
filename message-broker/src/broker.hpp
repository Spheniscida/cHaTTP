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

struct OutstandingTransaction;

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
    void onWebAppMSGGT(const WebappRequest& rq);
    void onWebAppISAUTH(const WebappRequest& rq);
    void onWebAppSettingsRequest(const WebappRequest& rq);
    void onPersistenceUREGD(const PersistenceLayerResponse& rp);
    void onPersistenceCHKDPASS(const PersistenceLayerResponse& rp);
    void onPersistenceLGDIN(const PersistenceLayerResponse& rp);
    void onPersistenceLGDOUT(const PersistenceLayerResponse& rp);
    void onPersistenceULKDUP(const PersistenceLayerResponse& rp);
    void onPersistenceMSGSVD(const PersistenceLayerResponse& rp);
    void onPersistenceMSGS(const PersistenceLayerResponse& rp);
    void onPersistenceSettingsResponse(const PersistenceLayerResponse& rp);
    void onMessagerelayMSGSNT(const MessageRelayResponse& rp);
    void onMessagerelayCHANCREAT(const MessageRelayResponse& rp);
    void onMessagerelayDELTDCHAN(const MessageRelayResponse& rp);
    void onB2BSNDMSG(const B2BIncoming& msg);
    void onB2BMSGSNT(const B2BIncoming& msg);

private:
    Communicator communicator;

    void handlePersistenceMessage(shared_ptr<PersistenceLayerResponse> msg);
    void handleWebappMessage(shared_ptr<WebappRequest> msg);
    void handleMessagerelayMessage(shared_ptr<MessageRelayResponse> msg);
    void handleBrokerMessage(shared_ptr<B2BIncoming> msg);
};

enum class OutstandingType {
    // for logins
    persistenceCHKDPASS,
    persistenceLGDIN,

    // for logouts
    persistenceLGDOUT,
    persistenceAfterFailedChancreatLogout,

    // for ULKDUP
    persistenceSndmsgULKDUP,
    persistenceUonlqULKDUP,
    persistenceLoginULKDUP,
    persistenceLogoutULKDUP,
    persistenceMessageGetULKDUP,
    persistenceIsauthULKDUP,
    persistenceGetSettingsULKDUP,
    persistenceSaveSettingsULKDUP,
    persistenceUREGD,
    persistenceMSGS,

    // for MSGSV
    persistenceMSGSVD,
    persistenceB2BMSGSVD,

    persistenceSAVEDSETTINGS,
    persistenceGOTSETTINGS,

    // for MSGSNT
    messagerelayMSGSNT,
    messagerelayB2BMSGSNT,
    messagerelayDELTDCHAN,
    messagerelayCHANCREAT,

    // b2b msgsnt
    b2bMSGSNT,
};

/**
 * An awaited request.
 */
struct OutstandingTransaction
{
    OutstandingType type;
    /// References another transaction, usually the sequence number of a request from the web application.
    sequence_t original_sequence_number;
    std::atomic<unsigned int>* remaining_count; // for messages to multiple recipients
};

# endif
