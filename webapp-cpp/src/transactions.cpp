# include "transactions.hpp"

TransactionMap transaction_map;

void TransactionMap::insert(sequence_t seq, const SavedTransaction& ta)
{
    std::lock_guard<std::mutex> lck(mx);
    map[seq] = ta;
    (map[seq]).lookup_success = true;
}

SavedTransaction& TransactionMap::get(sequence_t seq)
{
    std::lock_guard<std::mutex> lck(mx);
    return map[seq];
}

void TransactionMap::erase(sequence_t seq)
{
    std::lock_guard<std::mutex> lck(mx);
    map.erase(seq);
}
