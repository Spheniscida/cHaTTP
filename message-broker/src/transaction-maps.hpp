# ifndef TRANSACTION_MAPS_HPP
# define TRANSACTION_MAPS_HPP

# include <unordered_map>

# include "synchronization.hpp"
# include "broker.hpp"

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
    shared_mutex transactions_mutex;

    unordered_map<sequence_t,WebappRequest> webapp_requests;
    shared_mutex webapp_requests_mutex;

    unordered_map<sequence_t,string> b2b_origins;
    shared_mutex b2b_origins_mutex;

};

# endif