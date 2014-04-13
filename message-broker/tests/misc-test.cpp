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

BOOST_AUTO_TEST_CASE(message_relay_parser1)
{
    try {
	MessageRelayResponse resp("27272\nMSGSNT\nOK");

	BOOST_CHECK_EQUAL(resp.seq_num,27272);
	BOOST_CHECK_EQUAL(resp.status,true);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(message_relay_parser2)
{
    try {
	MessageRelayResponse resp("23876123\nMSGSNT\nFAIL");

	BOOST_CHECK_EQUAL(resp.seq_num,23876123);
	BOOST_CHECK_EQUAL(resp.status,false);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(message_relay_generator1)
{
    initializeGlobalSequenceNumber();
    try {
	MessageForRelay mesg("Hello, World.","aa5128");

	BOOST_CHECK_EQUAL(mesg.toString(),"1\nSNDMSG\naa5128\nHello, World.");
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(receivable_cast1)
{
    PersistenceLayerResponse* persist_resp = new PersistenceLayerResponse("87\nCHKDPASS\nOK");
    PersistenceLayerResponse* answer __attribute__((unused));

    Receivable* r = persist_resp;

    BOOST_CHECK(r->message_type == ReceivedMessageType::fromPersistence);

    if ( r->message_type == ReceivedMessageType::fromPersistence )
	answer = dynamic_cast<PersistenceLayerResponse*>(r);
}

BOOST_AUTO_TEST_CASE(receivable_correct_message_types_in_messages)
{
    PersistenceLayerResponse pr("823\nUREGD\nOK");
    MessageRelayResponse mrr("677\nMSGSNT\nOK");
    WebappRequest wr("78\nLOGOUT\nusr");

    BOOST_CHECK(pr.message_type == ReceivedMessageType::fromPersistence);
    BOOST_CHECK(mrr.message_type == ReceivedMessageType::fromMessageRelay);
    BOOST_CHECK(wr.message_type == ReceivedMessageType::fromWebApp);
}

BOOST_AUTO_TEST_SUITE_END()