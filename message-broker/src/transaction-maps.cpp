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

////

/**
 * @brief Find transactions that are too long in the queue.
 *
 * This function increments the tick counter (ticks_here) of all OutstandingTransactions in the
 * transaction map and returns the sequence numbers of those that are longer in the "queue"
 * than `max_ticks`.
 *
 * @param max_ticks Transactions with `ticks_here >= max_ticks` are returned in the `timedout_transactions` list.
 * @param timedout_transactions Output argument for all sequence numbers of transactions that have timed out. Sequence numbers
 * are appended to the end of the list (push_back).
 */
void TransactionMap::findTimedout(unsigned int max_ticks, std::list<sequence_t>& timedout_transactions)
{
    lock_guard<mutex> find_timedout_lock(transactions_mutex);

    unordered_map<sequence_t,OutstandingTransaction>::iterator map_itor = transactions.begin();
    unordered_map<sequence_t,OutstandingTransaction>::iterator end_pos = transactions.end();

    for ( ; map_itor != end_pos; map_itor++ )
    {
	map_itor->second.ticks_here++;

	if ( map_itor->second.ticks_here >= max_ticks )
	    timedout_transactions.push_back(map_itor->first);
    }

    if ( debugging_mode )
	debug_log("Found ",timedout_transactions.size()," transactions that have timed out");
}

