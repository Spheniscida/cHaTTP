# ifndef BROKER2BROKER_HPP
# define BROKER2BROKER_HPP

# include <string>

# include "error.hpp"
# include "conf.hpp"
# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;

enum class B2BMessageType {
    B2BSNDMSG,
    B2BMSGSNT
};

/**
 * @brief Class for outgoing B2B messages.
 */
class MessageForB2B
{
public:
    // for SNDMSG
    MessageForB2B(const string& message, const string& channel_id);
    // for MSGSNT
    MessageForB2B(sequence_t sequence_number, bool status);

    string toString(void) const;

    const sequence_t sequence_number;

private:

    string message;
    string channel_id;
    B2BMessageType type;
    bool status;
};

/**
 * @brief Class for incoming B2B messages.
 */
class B2BIncoming : public Receivable
{
public:
    B2BIncoming(const string& message, const string& sender);

    sequence_t sequence_number;
    string message;
    string channel_id;
    string origin_broker;
    B2BMessageType type;
    bool status;

private:
    void parseMessage(const string& msg);
};

# endif