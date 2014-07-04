# ifndef URL_HPP
# define URL_HPP

# include <string>
# include <map>

using std::string;

class Url
{
public:
    void parseUrl(const string& url);

    const string& getParameter(const string& key);

    enum RequestType { REGISTER, LOGIN, LOGOUT, SENDMESSAGE, GETMESSAGES, GETCONF, SETCONF, HEARTBEAT, ISONLINE };

    RequestType type;

private:

    void setType(const string& typestring);
    void parseParameters(const string& parameter_string);

    std::map<string,string> url_parameters;

};

# endif
