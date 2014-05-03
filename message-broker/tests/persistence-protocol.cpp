# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE PersistenceProtocolTest

# include <boost/test/unit_test.hpp>

# include <sstream>
# include <iostream>
# include <cstdlib>

# include <persistent.hpp>
# include <error.hpp>

BOOST_AUTO_TEST_SUITE(PersistenceProtocolTest)

/************************************ Persistence protocol - message parsing ***************************************/

BOOST_AUTO_TEST_CASE(persistent_response_code_stream1)
{
    using std::istringstream;

    istringstream s("UREGD\nULKDUP\nCHKDPASS\nLGDIN\nMSGS\nMSGSVD");

    PersistenceLayerResponseCode code;
    vector<PersistenceLayerResponseCode> should_be_codes{PersistenceLayerResponseCode::userRegistered,
	PersistenceLayerResponseCode::lookedUpUser, PersistenceLayerResponseCode::passwordChecked,
	PersistenceLayerResponseCode::loggedIn, PersistenceLayerResponseCode::messages,
	PersistenceLayerResponseCode::savedMessage };

    for ( int i = 0; i < 6; i++ )
    {
	try {
	    s >> code;
	} catch (BrokerError e)
	{
	BOOST_ERROR("An exception has been thrown:\n");
	    std::cerr << e.toString();
	}
	BOOST_CHECK(code == should_be_codes[i]);
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse1)
{

    try {
	PersistenceLayerResponse response("1234\nLGDIN\nOK");
	BOOST_CHECK(response.status);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::loggedIn);
	BOOST_CHECK_EQUAL(response.sequence_number,1234);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
};

BOOST_AUTO_TEST_CASE(persistent_response_parse2)
{
    try {
	PersistenceLayerResponse response("3372112\nCHKDPASS\nFAIL");
	BOOST_CHECK(!response.status);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::passwordChecked);
	BOOST_CHECK_EQUAL(response.sequence_number,3372112);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}
BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup)
{

    try {
	PersistenceLayerResponse response("23987\nULKDUP\nOK\nprod.spheniscida.de\n776ae45c");
	BOOST_CHECK(response.status);
	BOOST_CHECK(response.online);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response.sequence_number,23987);
	BOOST_CHECK_EQUAL(response.broker_name,"prod.spheniscida.de");
	BOOST_CHECK_EQUAL(response.channel_id,"776ae45c");
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup_offline)
{
    try {
	PersistenceLayerResponse response("77129\nULKDUP\nOFFLINE");
	BOOST_CHECK(response.status);
	BOOST_CHECK(! response.online);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response.sequence_number,77129);
	BOOST_CHECK(response.broker_name.empty());
	BOOST_CHECK(response.channel_id.empty());
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup_fail)
{
    try {
	PersistenceLayerResponse response("23988\nULKDUP\nFAIL");
	BOOST_CHECK(! response.status);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response.sequence_number,23988);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_messages)
{
    try {
	PersistenceLayerResponse response("11712393297\nMSGS\nOK\n{\"messages\": [\"Hello world 1.\", \"cHaTTP is awesome,\"]}");
	BOOST_CHECK(response.status);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::messages);
	BOOST_CHECK_EQUAL(response.sequence_number,11712393297);
	string s("{\"messages\": [\"Hello world 1.\", \"cHaTTP is awesome,\"]}");
	BOOST_CHECK_EQUAL(response.messages,s);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_logged_out)
{
    try {
	PersistenceLayerResponse response("26623723672908\nLGDOUT\nOK");
	BOOST_CHECK(response.status);
	BOOST_CHECK(response.response_type == PersistenceLayerResponseCode::loggedOut);
	BOOST_CHECK_EQUAL(response.sequence_number,26623723672908);
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

/************************* Persistence protocol - message creation ****************************/

BOOST_AUTO_TEST_CASE(persistent_create_lookup_message)
{
    initializeGlobalSequenceNumber();

    try {
	PersistenceLayerCommand c(PersistenceLayerCommandCode::lookUpUser,"user_name");

	BOOST_CHECK_EQUAL(c.toString(), "1\nULKUP\nuser_name");

    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }

}

BOOST_AUTO_TEST_CASE(persistent_create_login_message)
{
    initializeGlobalSequenceNumber();

    try {
	PersistenceLayerCommand c(PersistenceLayerCommandCode::logIn,"test_user","prod-brok-1.spheniscida.de","5e66c71da");

	BOOST_CHECK_EQUAL(c.toString(), "1\nLOGIN\ntest_user\nprod-brok-1.spheniscida.de\n5e66c71da");

    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }

}

BOOST_AUTO_TEST_CASE(persistent_create_msgsv_message)
{
    initializeGlobalSequenceNumber();

    try {
	PersistenceLayerCommand c(PersistenceLayerCommandCode::saveMessage,"to_user","message","from_user");

	BOOST_CHECK_EQUAL(c.toString(), "1\nMSGSV\nto_user\nfrom_user\nmessage");
    } catch (BrokerError e)
    {
	BOOST_ERROR("An exception has been thrown:\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_SUITE_END()

