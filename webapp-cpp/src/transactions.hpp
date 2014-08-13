# ifndef TRANSACTIONS_HPP
# define TRANSACTIONS_HPP

# include "protocol.hpp"

# include <unordered_map>
# include <mutex>

# include <fcgiapp.h>

struct SavedTransaction
{
    SavedTransaction(void) : lookup_success(false) {}

    FCGX_Request* request;
    bool lookup_success; // default is false. If retrieved from a map using operator[] and the record doesn't exist, it's false.
			 // However, explicitly inserted entries have set this member to true.
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


# endif
