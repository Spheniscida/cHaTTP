# ifndef TRANSACTIONS_HPP
# define TRANSACTIONS_HPP


# include "protocol.hpp"

# include <unordered_map>
# include <mutex>

# include <fcgiapp.h>

struct SavedTransaction
{
    FCGX_Request request;
};


class TransactionMap
{
public:
    void insert(sequence_t seq, const SavedTransaction& ta);
    SavedTransaction& get(sequence_t seq);
    void erase(sequence_t seq);

private:
    std::mutex mx;
    std::unordered_map<sequence_t,SavedTransaction> map;
};

extern TransactionMap transaction_map;

# endif
