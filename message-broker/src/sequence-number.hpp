# ifndef SEQUENCE_NUMBER_HPP
# define SEQUENCE_NUMBER_HPP

# include <atomic>

/// A type for sequence numbers.
typedef unsigned long long sequence_t;


enum class SequenceCounter {
    PersistenceCounter,
    MessageRelayCounter,
    B2BCounter
};

extern sequence_t getNewSequenceNumber(SequenceCounter which);

extern void initializeGlobalSequenceNumber(void);

# endif