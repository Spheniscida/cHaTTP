# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE WebAppBrokerTests

# include <boost/test/unit_test.hpp>
# include <vector>
# include <sstream>
# include <iostream>

# include <webapp-proto.hpp>
# include <error.hpp>

using std::istringstream;

BOOST_AUTO_TEST_SUITE(WebAppBrokerTests)

// This has to be the first test case. (too lazy for non-automated test creation...)
BOOST_AUTO_TEST_CASE(parse_request_codes)
{
    // Init
    initWebappProtocolParser();
    initializeGlobalSequenceNumber();

    // Test
    std::vector<WebappRequestCode> codes{WebappRequestCode::isOnline, WebappRequestCode::logIn, WebappRequestCode::logOut,
	WebappRequestCode::registerUser, WebappRequestCode::sendMessage };

    istringstream istr("UONLQ\nLOGIN\nLOGOUT\nUREG\nSNDMSG");

    WebappRequestCode c;

    try {
	for ( unsigned int i = 0; i < 4; i++ )
	{
	    istr >> c;
	    BOOST_CHECK(codes[i] == c);
	}
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(parse_ureg_request)
{
    try {
	WebappRequest rq("11765\nUREG\ntestuser1\ntestpassword");

	BOOST_CHECK(rq.request_type == WebappRequestCode::registerUser);
	BOOST_CHECK_EQUAL(rq.user,"testuser1");
	BOOST_CHECK_EQUAL(rq.password,"testpassword");
	BOOST_CHECK_EQUAL(rq.sequence_number,11765);
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(parse_login_request)
{
    try {
	WebappRequest rq("11765\nLOGIN\ntestuser1\ntestpassword");

	BOOST_CHECK(rq.request_type == WebappRequestCode::logIn);
	BOOST_CHECK_EQUAL(rq.user,"testuser1");
	BOOST_CHECK_EQUAL(rq.password,"testpassword");
	BOOST_CHECK_EQUAL(rq.sequence_number,11765);
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(parse_logout_request)
{
    try {
	WebappRequest rq("11765\nLOGOUT\ntestuser1");

	BOOST_CHECK(rq.request_type == WebappRequestCode::logOut);
	BOOST_CHECK_EQUAL(rq.user,"testuser1");
	BOOST_CHECK_EQUAL(rq.sequence_number,11765);
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(parse_message_send_request)
{
    try {
	WebappRequest rq("11765\nSNDMSG\ntestuser1\n663ef33b9a1\ntestuser2\nHallo Welt!");

	BOOST_CHECK(rq.request_type == WebappRequestCode::sendMessage);
	BOOST_CHECK_EQUAL(rq.user,"testuser1");
	BOOST_CHECK_EQUAL(rq.dest_user,"testuser2");
	BOOST_CHECK_EQUAL(rq.channel_id,"663ef33b9a1");
	BOOST_CHECK_EQUAL(rq.message,"Hallo Welt!");
	BOOST_CHECK_EQUAL(rq.sequence_number,11765);
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(parse_user_online_request)
{
    try {
	WebappRequest rq("11765\nUONLQ\ntestuser1");

	BOOST_CHECK(rq.request_type == WebappRequestCode::isOnline);
	BOOST_CHECK_EQUAL(rq.user,"testuser1");
	BOOST_CHECK_EQUAL(rq.sequence_number,11765);
    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_register_response1)
{
    try {
	WebappResponse r(1,WebappResponseCode::registeredUser,true);
	BOOST_CHECK_EQUAL(r.toString(),"1\nUREGD\nOK");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_register_response2)
{
    try {
	WebappResponse r(1,WebappResponseCode::registeredUser,false);
	BOOST_CHECK_EQUAL(r.toString(),"1\nUREGD\nFAIL");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_onlineq_response1)
{
    try {
	WebappResponse r(1,WebappResponseCode::isOnline,true);
	BOOST_CHECK_EQUAL(r.toString(),"1\nUONL\nY");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_onlineq_response2)
{
    try {
	WebappResponse r(1,WebappResponseCode::isOnline,false);
	BOOST_CHECK_EQUAL(r.toString(),"1\nUONL\nN");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_accmsg_response1)
{
    try {
	WebappResponse r(1,WebappResponseCode::acceptedMessage,true);
	BOOST_CHECK_EQUAL(r.toString(),"1\nACCMSG\nOK");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_accmsg_response2)
{
    try {
	WebappResponse r(1,WebappResponseCode::acceptedMessage,false);
	BOOST_CHECK_EQUAL(r.toString(),"1\nACCMSG\nFAIL");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_login_response1)
{
    try {
	WebappResponse r(1,WebappResponseCode::loggedIn,true,"aabce328");
	BOOST_CHECK_EQUAL(r.toString(),"1\nLGDIN\nOK\naabce328");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_login_response2)
{
    try {
	WebappResponse r(1,WebappResponseCode::loggedIn,false,"aabce328");
	BOOST_CHECK_EQUAL(r.toString(),"1\nLGDIN\nFAIL");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_CASE(create_logout_response)
{
    try {
	WebappResponse r(1,WebappResponseCode::loggedOut);
	BOOST_CHECK_EQUAL(r.toString(),"1\nLGDOUT");

    } catch (BrokerError e)
    {
	BOOST_ERROR("Exception!\n");
	std::cerr << e.toString();
    }
}

BOOST_AUTO_TEST_SUITE_END()