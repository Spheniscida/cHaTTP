# ifndef MESSAGE_RELAY_HPP
# define MESSAGE_RELAY_HPP

# include <string>

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;

class MessageForRelay
{
public:
    MessageForRelay(const string& mesg, const string& chan_id);

    string toString(void) const;
private:
    sequence_t seq_num;
    string message;
    string channel_id;
};

class MessageRelayResponse : public Receivable
{
public:
    MessageRelayResponse(const string& response);

    sequence_t seq_num;
    bool status;
};

# endif