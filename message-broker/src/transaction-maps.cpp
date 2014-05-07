# include "transaction-maps.hpp"


OutstandingTransaction& TransactionMap::lookupTransaction(sequence_t seqnum)
{
    lock_guard<mutex> transaction_lock(transactions_mutex);
    return transactions[seqnum];
}

void TransactionMap::insertTransaction(sequence_t seqnum, const OutstandingTransaction& ta)
{
    lock_guard<mutex> transaction_lock(transactions_mutex);
    transactions[seqnum] = ta;
}

void TransactionMap::eraseTransaction(sequence_t seqnum)
{
    lock_guard<mutex> transaction_lock(transactions_mutex);
    transactions.erase(seqnum);
}

void TransactionMap::eraseAndInsertTransaction(sequence_t erase, sequence_t newkey, const OutstandingTransaction& ta)
{
    lock_guard<mutex> transaction_lock(transactions_mutex);
    transactions[newkey] = ta;
    transactions.erase(erase);
}

////

WebappRequest& TransactionMap::lookupWebappRequest(sequence_t seqnum)
{
    lock_guard<mutex> webapp_requests_lock(webapp_requests_mutex);
    return webapp_requests[seqnum];
}

void TransactionMap::insertWebappRequest(sequence_t seqnum, const WebappRequest& wa_rq)
{
    lock_guard<mutex> webapp_requests_lock(webapp_requests_mutex);
    webapp_requests[seqnum] = wa_rq;
}

void TransactionMap::eraseWebappRequest(sequence_t seqnum)
{
    lock_guard<mutex> webapp_requests_lock(webapp_requests_mutex);
    webapp_requests.erase(seqnum);
}

////

const string& TransactionMap::lookupB2BOrigin(sequence_t seqnum)
{
    lock_guard<mutex> b2b_origins_lock(b2b_origins_mutex);
    return b2b_origins[seqnum];
}

void TransactionMap::insertB2BOrigin(sequence_t seqnum, const string& origin)
{
    lock_guard<mutex> b2b_origins_lock(b2b_origins_mutex);
    b2b_origins[seqnum] = origin;
}

void TransactionMap::eraseB2BOrigin(sequence_t seqnum)
{
    lock_guard<mutex> b2b_origins_lock(b2b_origins_mutex);
    b2b_origins.erase(seqnum);
}
