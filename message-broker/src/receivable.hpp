# ifndef RECEIVABLE_HPP
# define RECEIVABLE_HPP

/**
 * @brief Superclass for messages received from the outside.
 *
 * This class is used by the IPC part as returned object.
 *
 */

enum class ReceivedMessageType {
    fromWebApp,
    fromMessageRelay,
    fromPersistence
};

class Receivable
{
public:
    virtual ~Receivable(void) = default;
    ReceivedMessageType message_type;
};

# endif