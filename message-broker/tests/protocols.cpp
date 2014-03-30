# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE MessageBrokerTest

# include <boost/test/unit_test.hpp>

# include <sstream>
# include <iostream>
# include <cstdlib>

# include <persistent.hpp>
# include <error.hpp>

BOOST_AUTO_TEST_SUITE(Broker)

BOOST_AUTO_TEST_CASE(persistent_response_code_stream1)
{
    using std::istringstream;

    istringstream s("UREGD\nULKDUP\nCHKDPASS\nLGDIN\nMSGS\nMSGSVD");

    persistenceLayerResponseCode code;
    vector<persistenceLayerResponseCode> should_be_codes{persistenceLayerResponseCode::userRegistered,
	persistenceLayerResponseCode::lookedUpUser, persistenceLayerResponseCode::passwordChecked,
	persistenceLayerResponseCode::loggedIn, persistenceLayerResponseCode::messages,
	persistenceLayerResponseCode::savedMessage };

    for ( int i = 0; i < 6; i++ )
    {
	try {
	    s >> code;
	} catch (brokerError e)
	{
	    std::cerr << e.toString();
	}
	BOOST_CHECK(code == should_be_codes[i]);
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse1)
{
    persistenceLayerResponse* response;

    try {
	response = parsePersistenceResponse("1234\nLGDIN\nOK");
	BOOST_CHECK(response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::loggedIn);
	BOOST_CHECK_EQUAL(response->sequence_number,1234);
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
};

BOOST_AUTO_TEST_CASE(persistent_response_parse2)
{
    persistenceLayerResponse* response;

    try {
	response = parsePersistenceResponse("3372112\nCHKDPASS\nFAIL");
	BOOST_CHECK(!response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::passwordChecked);
	BOOST_CHECK_EQUAL(response->sequence_number,3372112);
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}
BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup)
{
    persistenceLayerLookupResponse* response;

    try {
	response = static_cast<persistenceLayerLookupResponse*>(parsePersistenceResponse("23987\nULKDUP\nOK\nprod.spheniscida.de\n776ae45c"));
	BOOST_CHECK(response->status);
	BOOST_CHECK(response->online);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response->sequence_number,23987);
	BOOST_CHECK_EQUAL(response->broker_name,"prod.spheniscida.de");
	BOOST_CHECK_EQUAL(response->channel_name,"776ae45c");
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup_offline)
{
    persistenceLayerLookupResponse* response;

    try {
	response = static_cast<persistenceLayerLookupResponse*>(parsePersistenceResponse("77129\nULKDUP\nOFFLINE"));
	BOOST_CHECK(response->status);
	BOOST_CHECK(! response->online);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response->sequence_number,77129);
	BOOST_CHECK(response->broker_name.empty());
	BOOST_CHECK(response->channel_name.empty());
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup_fail)
{
    persistenceLayerLookupResponse* response;

    try {
	response = static_cast<persistenceLayerLookupResponse*>(parsePersistenceResponse("23988\nULKDUP\nFAIL"));
	BOOST_CHECK(! response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response->sequence_number,23988);
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_messages)
{
    persistenceLayerMessagesResponse* response;

    try {
	response = static_cast<persistenceLayerMessagesResponse*>(parsePersistenceResponse("11712393297\nMSGS\nOK\nHello world 1.\ncHaTTP is awesome\n"));
	BOOST_CHECK(response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::messages);
	BOOST_CHECK_EQUAL(response->sequence_number,11712393297);
	BOOST_CHECK_EQUAL(response->messages.size(),2);
	BOOST_CHECK_EQUAL(response->messages[0],"Hello world 1.");
	BOOST_CHECK_EQUAL(response->messages[1],"cHaTTP is awesome");
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_logged_out)
{
    persistenceLayerResponse* response;
    try {
	response = parsePersistenceResponse("26623723672908\nLGDOUT\nOK");
	BOOST_CHECK(response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::loggedOut);
	BOOST_CHECK_EQUAL(response->sequence_number,26623723672908);
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_SUITE_END()

