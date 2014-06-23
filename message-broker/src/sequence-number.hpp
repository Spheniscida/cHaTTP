# ifndef SEQUENCE_NUMBER_HPP
# define SEQUENCE_NUMBER_HPP

# include <atomic>

/// A type for sequence numbers.
typedef unsigned long long sequence_t;

class SequenceCounter
{
public:
    SequenceCounter(void) { current_number = 1; }
    sequence_t get(void) { return current_number++; }

private:
    std::atomic<sequence_t> current_number;
};

extern SequenceCounter message_relay_counter, persistence_counter, b2b_counter;

# endif
