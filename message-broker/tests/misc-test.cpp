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

using std::string;

/*
 * For parts of the program that are not important enough to have a dedicated
 * test suite.
 */

BOOST_AUTO_TEST_SUITE(MiscellaneousBrokerTests)

BOOST_AUTO_TEST_CASE(global_sequence_number_init_1)
{
    initializeGlobalSequenceNumber();

    BOOST_CHECK_EQUAL(getNewSequenceNumber(SequenceCounter::PersistenceCounter),1);
    BOOST_CHECK_EQUAL(getNewSequenceNumber(SequenceCounter::B2BCounter),1);
}

BOOST_AUTO_TEST_CASE(global_sequence_number_after_increase)
{
    BOOST_CHECK_EQUAL(getNewSequenceNumber(SequenceCounter::PersistenceCounter),2);
}

BOOST_AUTO_TEST_CASE(remove_error_code1)
{
    string orig = "7,some error";

    BOOST_CHECK_EQUAL(removeErrorCode(orig),7);
    BOOST_CHECK_EQUAL(orig,string("some error"));
}

BOOST_AUTO_TEST_CASE(remove_error_code2)
{
    string orig = "234,some other error";

    BOOST_CHECK_EQUAL(removeErrorCode(orig),234);
    BOOST_CHECK_EQUAL(orig,string("some other error"));
}

BOOST_AUTO_TEST_CASE(remove_error_code3)
{
    string orig = "error w/o code";

    BOOST_CHECK_EQUAL(removeErrorCode(orig),0);
    BOOST_CHECK_EQUAL(orig,string("error w/o code"));
}

BOOST_AUTO_TEST_CASE(remove_error_code4)
{
    string orig = "";

    BOOST_CHECK_EQUAL(removeErrorCode(orig),0);
    BOOST_CHECK_EQUAL(orig,string(""));
}

BOOST_AUTO_TEST_SUITE_END()
