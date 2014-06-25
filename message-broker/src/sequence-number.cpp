# include "sequence-number.hpp"

SequenceCounter message_relay_counter, persistence_counter, b2b_counter;

std::atomic<sequence_t> SequenceCounter::current_number;
