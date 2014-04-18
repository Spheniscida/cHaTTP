# include "sequence-number.hpp"

atomicSequenceState current_sequence_number;

/**
 * This function should be called once, during the broker's boot process.
 */
void initializeGlobalSequenceNumber(void)
{
    current_sequence_number = 1;
}

/**
 * Works atomically -- thread-safe.
 */
sequence_t getNewSequenceNumber(void)
{
    return current_sequence_number.fetch_add(1);
}