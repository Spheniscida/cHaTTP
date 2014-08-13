# include "json.hpp"

void JSONObject::addPair(const JSONPair& p)
{
    pairs.push_back(p.toString());
}

string JSONObject::toString(void) const
{
    std::ostringstream out;
    bool first = true;

    out << "{";

    for ( const string& p : pairs )
    {
	if ( ! first )
	{
	    out << ",";
	}

	first = false;
	out << p;
    }

    out << "}";

    return out.str();
}

string JSONPair::toString(void) const
{
    std::string out;

    // Slightly faster than ostringstream
    out = string("\"") + key + "\":" + valueToString();

    return out;
}

string JSONObjectList::valueToString(void) const
{
    std::ostringstream out;

    bool first = true;

    out << "[";

    for ( const JSONObject& o: objs )
    {
	if ( ! first )
	    out << ",";

	first = false;

	out << o.toString();
    }

    out << "]";

    return out.str();

}

