# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE MessageBrokerTest

# include <boost/test/unit_test.hpp>

# include <persistent.hpp>
# include <error.hpp>
# include <sstream>
# include <iostream>

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
	response = parsePersistenceResponse("1234 LGDIN OK");
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
	response = parsePersistenceResponse("3372112 CHKDPASS FAIL");
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
	response = static_cast<persistenceLayerLookupResponse*>(parsePersistenceResponse("23987 ULKDUP OK prod.spheniscida.de 776ae45c"));
	BOOST_CHECK(response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response->sequence_number,23987);
	BOOST_CHECK_EQUAL(response->broker_name,"prod.spheniscida.de");
	BOOST_CHECK_EQUAL(response->channel_name,"776ae45c");
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(persistent_response_parse_lookup_fail)
{
    persistenceLayerLookupResponse* response;

    try {
	response = static_cast<persistenceLayerLookupResponse*>(parsePersistenceResponse("23988 ULKDUP FAIL"));
	BOOST_CHECK(! response->status);
	BOOST_CHECK(response->response_type == persistenceLayerResponseCode::lookedUpUser);
	BOOST_CHECK_EQUAL(response->sequence_number,23988);
    } catch (brokerError e)
    {
	std::cerr << e.toString();
    }
}


BOOST_AUTO_TEST_SUITE_END()

