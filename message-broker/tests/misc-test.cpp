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

BOOST_AUTO_TEST_CASE(message_relay_parser1)
{
    try {
	MessageRelayResponse resp("27272\nMSGSNT\nOK");

	BOOST_CHECK_EQUAL(resp.sequence_number,27272);
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

	BOOST_CHECK_EQUAL(resp.sequence_number,23876123);
	BOOST_CHECK_EQUAL(resp.status,false);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(message_relay_parser3)
{
    try {
	MessageRelayResponse resp("23876123\nDELTDCHAN\nOK");

	BOOST_CHECK_EQUAL(resp.sequence_number,23876123);
	BOOST_CHECK_EQUAL(resp.status,true);
	BOOST_CHECK(resp.response_type == MessageRelayResponseType::channelDeleted);
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
	MessageForRelay mesg("sender","Hello, World.","aa5128");

	BOOST_CHECK_EQUAL(mesg.toString(),"1\nSNDMSG\nsender\naa5128\nHello, World.");
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(message_relay_generator2)
{
    initializeGlobalSequenceNumber();
    try {
	MessageForRelay mesg("asdjhasdkjh",MessageForRelayType::deleteChannel);

	BOOST_CHECK_EQUAL(mesg.toString(),"1\nDELCHAN\nasdjhasdkjh");
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(message_relay_generator3)
{
    initializeGlobalSequenceNumber();
    try {
	MessageForRelay mesg("asdjhasdkjh",MessageForRelayType::createChannel);

	BOOST_CHECK_EQUAL(mesg.toString(),"1\nNEWCHAN\nasdjhasdkjh");
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

    answer = dynamic_cast<PersistenceLayerResponse*>(r);

    if ( ! answer )
	BOOST_FAIL("Dynamic cast from Receivable to PersistenceLayerResponse failed.");
}

BOOST_AUTO_TEST_CASE(channel_id_length)
{
    initializeUrandomSource();
    string chanid(generateChannelId());

    BOOST_CHECK_EQUAL(chanid.size(),64);
}

BOOST_AUTO_TEST_CASE(channel_id_different)
{
    string chanid1(generateChannelId()), chanid2(generateChannelId());

    BOOST_CHECK_NE(chanid1,chanid2);
}

/*********************** B2B tests *****************/

BOOST_AUTO_TEST_CASE(b2b_outgoing_tostring1)
{
    MessageForB2B mesg(87000,true);

    BOOST_CHECK_EQUAL(mesg.toString(),"87000\nMSGSNT\nOK");
}

BOOST_AUTO_TEST_CASE(b2b_outgoing_tostring2)
{
    MessageForB2B mesg(2612,false);

    BOOST_CHECK_EQUAL(mesg.toString(),"2612\nMSGSNT\nFAIL");
}

BOOST_AUTO_TEST_CASE(b2b_outgoing_tostring3)
{
    initializeGlobalSequenceNumber();

    MessageForB2B mesg("sender","Hello, World!","channel_id");

    BOOST_CHECK_EQUAL(mesg.toString(),"1\nSNDMSG\nsender\nchannel_id\nHello, World!");
}

BOOST_AUTO_TEST_CASE(b2b_incoming1)
{
    B2BIncoming mesg("1\nMSGSNT\nOK","localhost");

    BOOST_CHECK_EQUAL(mesg.sequence_number,1);
    BOOST_CHECK_EQUAL(mesg.status,true);
    BOOST_CHECK(mesg.type == B2BMessageType::B2BMSGSNT);
}

BOOST_AUTO_TEST_CASE(b2b_incoming2)
{
    B2BIncoming mesg("87\nSNDMSG\nsender\nchannel-id\nHello, World!","localhost");

    BOOST_CHECK_EQUAL(mesg.sequence_number,87);
    BOOST_CHECK_EQUAL(mesg.sender_username,"sender");
    BOOST_CHECK_EQUAL(mesg.channel_id,"channel-id");
    BOOST_CHECK_EQUAL(mesg.message,"Hello, World!");
    BOOST_CHECK(mesg.type == B2BMessageType::B2BSNDMSG);
}

BOOST_AUTO_TEST_SUITE_END()