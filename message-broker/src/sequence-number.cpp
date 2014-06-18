# include "sequence-number.hpp"

namespace
{
    std::atomic<sequence_t> current_persistence_sequence_number,
			    current_messagerelay_sequence_number,
			    current_b2b_sequence_number;
}

/**
 * This function should be called once, during the broker's boot process.
 */
void initializeGlobalSequenceNumber(void)
{
    current_persistence_sequence_number = current_messagerelay_sequence_number = current_b2b_sequence_number = 1;
}

/**
 * Works atomically -- thread-safe.
 */
sequence_t getNewSequenceNumber(SequenceCounter which)
{
    switch ( which )
    {
	case SequenceCounter::PersistenceCounter:
	    return current_persistence_sequence_number++;
	case SequenceCounter::MessageRelayCounter:
	    return current_messagerelay_sequence_number++;
	case SequenceCounter::B2BCounter:
	    return current_b2b_sequence_number++;
    }

    return 0;
}