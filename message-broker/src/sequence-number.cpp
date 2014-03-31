# include "sequence-number.hpp"

atomicSequenceState current_sequence_number;

void initializeGlobalSequenceNumber(void)
{
    current_sequence_number = 1;
}

sequence_t getNewSequenceNumber(void)
{
    return current_sequence_number.fetch_add(1);
}