# ifndef SEQUENCE_NUMBER_HPP
# define SEQUENCE_NUMBER_HPP

# include <atomic>

/// A type for sequence numbers.
typedef unsigned long long sequence_t;

typedef std::atomic<sequence_t> atomicSequenceState;

/// The current global state of the sequence number.
extern atomicSequenceState current_sequence_number;

extern sequence_t getNewSequenceNumber(void);

extern void initializeGlobalSequenceNumber(void);

# endif