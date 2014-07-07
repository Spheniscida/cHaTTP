# include "url.hpp"
# include "error.hpp"
# include <iostream>
# include <memory>

const string& Url::getParameter(const string& k) const
{
    try
    {
	return url_parameters.at(k);
    } catch (std::out_of_range e)
    {
	return empty_dummy;
    }
}

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

    unsigned long long index = 1, next_index = 0;

    std::string url(std::move(decodePercent(encoded_url)));

    std::string current_path_part, type_string, parameter_string;

//     std::cout << url << std::endl;

    current_path_part.resize(128);

    // Compute the index at which the type string starts (isonline, send etc)
    while ( current_path_part != "chattp_request" && index < url.size() )
    {
	current_path_part.clear();

	if ( string::npos != (next_index = url.find('/', index)) )
	{
	    current_path_part = url.substr(index,next_index - index);
	    index = next_index+1;
	} else
	{
	    throw WebappError("Malformed URL: Expected path part \"chattp_request\"");
	}
    }

    const unsigned int type_index = index;

    while ( url[index] != '?' && index < url.size() )
	index++;

    type_string = url.substr(type_index,index-type_index);
    setType(type_string);

    if ( index < url.size() )
	index++;

    url_parameters.clear();

    parameter_string = url.substr(index,url.size()-index);
    parseParameters(parameter_string);

}

void Url::setType(const string& typestring)
{
    //std::cout << typestring;

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
    else
	throw WebappError("Unknown request type.");
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

string Url::decodePercent(const string& url)
{
    if ( string::npos == url.find('%') )
	return url;

    string new_url;

    string tmp_hex;
    tmp_hex = "0x";
    tmp_hex.resize(4);

    new_url.reserve(url.size()); // This is probably longer than needed

    for ( unsigned int index = 0; index < url.size(); )
    {
	unsigned long long next_pct = url.find('%',index);

	if ( string::npos == next_pct ) // No more encoded characters
	{
            new_url.append(url.substr(index,url.size()));
            index = url.size();
	} else if ( next_pct > index )
	{
	    new_url.append(url.substr(index,next_pct - index));
	    index += next_pct - index;
	} else if ( next_pct == index )
	{
	    index++;
	    tmp_hex[2] = url[index];
	    index++;
	    tmp_hex[3] = url[index];
	    index++;

	    new_url.push_back(static_cast<char>(std::stoi(tmp_hex,0,16)));
	}
    }

    return new_url;
}
