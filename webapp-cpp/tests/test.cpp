# define BOOST_ALL_DYN_LINK
# define BOOST_TEST_MODULE MiscellaneousBrokerTests

# include <boost/test/unit_test.hpp>
# include <iostream>

# include <json.hpp>
# include <url.hpp>
# include <error.hpp>

using std::string;

/*
 * For parts of the program that are not important enough to have a dedicated
 * test suite.
 */

BOOST_AUTO_TEST_SUITE(webappTests)

BOOST_AUTO_TEST_CASE(parseUrlType)
{
    Url u;

    u.parseUrl("/test/chattp_request/isonline");

    BOOST_CHECK(u.type == Url::ISONLINE);
    BOOST_CHECK_EQUAL(u.url_parameters.size(),0);
}

BOOST_AUTO_TEST_CASE(parseUrlParameter1)
{
    Url u;

    u.parseUrl("/test/chattp_request/register?user_name=usr&password=pwd");

    BOOST_CHECK(u.type == Url::REGISTER);
    BOOST_CHECK_EQUAL(u.getParameter("user_name"),"usr");
    BOOST_CHECK_EQUAL(u.getParameter("password"),"pwd");
    BOOST_CHECK_EQUAL(u.url_parameters.size(),2);
}

BOOST_AUTO_TEST_CASE(parseUrlParameter2WithPercent)
{
    Url u;

    u.parseUrl("/test/chattp_request/login?user_name=%C3%BC%C3%A4x&password=pwd");

    BOOST_CHECK(u.type == Url::LOGIN);
    BOOST_CHECK_EQUAL(u.getParameter("user_name"),"üäx");
    BOOST_CHECK_EQUAL(u.getParameter("password"),"pwd");
}

BOOST_AUTO_TEST_CASE(parseUrlThrowOnFail)
{
    Url u;

    BOOST_CHECK_THROW(u.parseUrl("/test/xyz/abc"),WebappError);
}

BOOST_AUTO_TEST_CASE(buildJSON1)
{
    JSONObject o;

    JSONBoolean b("key1",false);
    JSONNumber n("key2",23);
    JSONString s("key3","value3");

    o.addPair(b);
    o.addPair(n);
    o.addPair(s);

    BOOST_CHECK_EQUAL(o.toString(),"{\"key1\":false,\"key2\":23,\"key3\":\"value3\"}");

}

BOOST_AUTO_TEST_CASE(buildJSON2)
{
    JSONObject o, p, q;

    JSONBoolean b("key1",true);
    JSONNumber n("key2",23);
    JSONString s("key3","value3");

    o.addPair(b);
    p.addPair(n);
    p.addPair(s);

    JSONObjectPair obj("key4",o);

    JSONObjectList objlist("list1");
    objlist.addValue(o);
    objlist.addValue(p);

    q.addPair(obj);
    q.addPair(objlist);

    BOOST_CHECK_EQUAL(q.toString(),"{\"key4\":{\"key1\":true},\"list1\":[{\"key1\":true},{\"key2\":23,\"key3\":\"value3\"}]}");
}

BOOST_AUTO_TEST_SUITE_END()

