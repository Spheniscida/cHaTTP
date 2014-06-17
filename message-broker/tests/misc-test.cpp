# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE MiscellaneousBrokerTests

# include <boost/test/unit_test.hpp>
# include <iostream>

# include <error.hpp>
# include <receivable.hpp>
# include <persistent.hpp>
# include <sequence-number.hpp>
# include <message-relay.hpp>
# include <webapp-proto.hpp>
# include <broker-util.hpp>
# include <broker2broker.hpp>

/*
 * For parts of the program that are not important enough to have a dedicated
 * test suite.
 */

BOOST_AUTO_TEST_SUITE(MiscellaneousBrokerTests)

BOOST_AUTO_TEST_CASE(global_sequence_number_init_1)
{
    initializeGlobalSequenceNumber();

    BOOST_CHECK_EQUAL(getNewSequenceNumber(),1);
}

BOOST_AUTO_TEST_CASE(global_sequence_number_after_increase)
{
    BOOST_CHECK_EQUAL(getNewSequenceNumber(),2);
}


BOOST_AUTO_TEST_SUITE_END()