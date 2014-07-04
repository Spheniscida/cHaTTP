# include "url.hpp"
# include <iostream>
# include <curl/curl.h>

/**
 * @brief A very bad but working URL parser.
 *
 * The type is placed in the Url::type member, the parameters are inserted
 * into the url_parameters map.
 *
 * Features:
 * 	* Recognizing the request type using the path part after 'chattp_request/'
 * 	* Parsing parameters with and without values.
 *
 */
void Url::parseUrl(const string& encoded_url)
{
    // Gaah, I wanna have attoparsec here!

    unsigned int index = 0, curpp_index = 0;

    std::string url(curl_unescape(encoded_url.c_str(),encoded_url.size()));

    std::string current_path_part, type_string, parameter_string;

    current_path_part.resize(128);

    // Compute the index at which the type string starts (isonline, send etc)
    while ( current_path_part != "chattp_request" )
    {
	curpp_index = 0;
	current_path_part.clear();

	while ( url[index] != '/' && curpp_index < 128 && index < url.size() )
	{
	    current_path_part.push_back(url[index]);
	    curpp_index++, index++;
	}
	index++;
    }

    const unsigned int type_index = index;

    while ( url[index] != '?' )
	index++;
    index++;

    type_string = url.substr(type_index,index-type_index);
    setType(type_string);

    parameter_string = url.substr(index,url.size()-index);
    parseParameters(parameter_string);

}

void Url::setType(const string& typestring)
{
    if ( typestring == "isonline" )
	type = ISONLINE;
    else if ( typestring == "send" )
	type = SENDMESSAGE;
    else if ( typestring == "setconf" )
	type = SETCONF;
    else if ( typestring == "getconf" )
	type = GETCONF;
    else if ( typestring == "register" )
	type = REGISTER;
    else if ( typestring == "login" )
	type = LOGIN;
    else if ( typestring == "logout" )
	type = LOGOUT;
    else if ( typestring == "savedmessages" )
	type = GETMESSAGES;
    else if ( typestring == "heartbeat" )
	type = HEARTBEAT;
}

void Url::parseParameters(const string& parameter_string)
{
    std::size_t length = parameter_string.size();

    std::string key, value;
    unsigned int index = 0;

    // First character should be the one after the '?'
    while ( index < length )
    {
	while ( parameter_string[index] != '=' && parameter_string[index] != '&' && index < length )
	{
	    key.push_back(parameter_string[index]);
	    index++;
	}

	if ( parameter_string[index] == '=' )
	{
	    index++;

	    while ( parameter_string[index] != '&' && index < length )
	    {
		value.push_back(parameter_string[index]);
		index++;
	    }

	    index++;

	    url_parameters[key] = value;
	} else if ( parameter_string[index] == '&' || index == length /* End of URL */ ) // No value
	{
	    url_parameters[key] = "";
	    index++;
	}

	value.clear();
	key.clear();
    }

    /* DEBUG
    for ( auto x : url_parameters )
    {
	std::cout << x.first << " : " << x.second << std::endl;
    }
    */
}
