# ifndef TRANSACTION_MAPS_HPP
# define TRANSACTION_MAPS_HPP

# include <unordered_map>

using std::unordered_map;

# include "synchronization.hpp"
# include "sequence-number.hpp"
# include "webapp-proto.hpp"

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
    persistenceHeartbeated,

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

class TransactionMap
{
public:
    OutstandingTransaction& lookupTransaction(sequence_t);
    void insertTransaction(sequence_t,const OutstandingTransaction&);
    void eraseTransaction(sequence_t);
    void eraseAndInsertTransaction(sequence_t erase, sequence_t newkey, const OutstandingTransaction& ta);

    WebappRequest& lookupWebappRequest(sequence_t seqnum);
    void insertWebappRequest(sequence_t seqnum, const WebappRequest& wa_rq);
    void eraseWebappRequest(sequence_t seqnum);

    const string& lookupB2BOrigin(sequence_t seqnum);
    void insertB2BOrigin(sequence_t seqnum, const string& origin);
    void eraseB2BOrigin(sequence_t seqnum);
private:
    unordered_map<sequence_t,OutstandingTransaction> transactions;
    mutex transactions_mutex;

    unordered_map<sequence_t,WebappRequest> webapp_requests;
    mutex webapp_requests_mutex;

    unordered_map<sequence_t,string> b2b_origins;
    mutex b2b_origins_mutex;

};

# endif
