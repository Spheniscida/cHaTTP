# ifndef URL_HPP
# define URL_HPP

# include <string>
# include <map>

using std::string;

class RequestURI
{
public:
    void parseUrl(const string& url);

    const string& getParameter(const string& key) const;

    enum RequestType { REGISTER, LOGIN, LOGOUT, SENDMESSAGE, GETMESSAGES, GETCONF, SETCONF, HEARTBEAT, ISONLINE, CHANGEPASS };

    RequestType type;

    unsigned int _mapSize(void) { return url_parameters.size(); }

private:

    std::map<string,string> url_parameters;
    void setType(const string& typestring);
    void parseParameters(const string& parameter_string);

    static string decodePercent(const string& url);
    const string empty_dummy;

};

# endif
