# include "json.hpp"


string JSONPair::toString(void) const
{
    std::ostringstream out;

    out << "\"" << key << "\":" << valueToString();

    return out.str();
}


string JSONObject::toString(void) const
{
    std::ostringstream out;
    bool first = true;

    out << "{";

    for ( shared_ptr<JSONPair> p : pairs )
    {
	if ( ! first )
	{
	    out << ",";
	}

	first = false;
	out << p->toString();
    }

    out << "}";

    return out.str();
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

