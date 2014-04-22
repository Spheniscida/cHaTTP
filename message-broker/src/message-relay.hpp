# ifndef MESSAGE_RELAY_HPP
# define MESSAGE_RELAY_HPP

# include <string>

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;

enum class MessageForRelayType {
    sendMessage,
    deleteChannel
};

class MessageForRelay
{
public:
    MessageForRelay(const string& sender, const string& mesg, const string& chan_id);
    MessageForRelay(const string& chan_id);

    string toString(void) const;
    const sequence_t seq_num;
private:
    string message;
    string sender_user;
    string channel_id;
    MessageForRelayType type;
};

enum class MessageRelayResponseType {
    messageSent,
    channelDeleted
};

class MessageRelayResponse : public Receivable
{
public:
    MessageRelayResponse(const string& response);

    sequence_t sequence_number;
    MessageRelayResponseType type;
    bool status;
private:
    void parseMessage(const string& mesg);
};

# endif