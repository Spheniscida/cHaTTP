# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE WebAppBrokerTests

# include <boost/test/unit_test.hpp>
# include <vector>
# include <sstream>

# include <webapp-proto.hpp>

BOOST_AUTO_TEST_CASE(parse_request_codes)
{
    std::vector<WebappRequestCode> codes{WebappRequestCode::isOnline, WebappRequestCode::logIn, WebappRequestCode::logOut,
	WebappRequestCode::registerUser, WebappRequestCode::sendMessage };

    std::istringstream istr("UONLQ\nLOGIN\nLOGOUT\nUREG\nSNDMSG");
    
    WebappRequestCode c;
    
    for ( unsigned int i = 0; i < 4; i++ )
    {
	istr >> c;
	BOOST_CHECK(codes[i] == c);
    }
}
