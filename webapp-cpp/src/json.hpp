# ifndef JSON_HPP
# define JSON_HPP

# include <string>
# include <list>
# include <sstream>

using std::string;
using std::list;

class JSONPair;

/**
 * @brief This is a very spartan JSON implementation.
 *
 * It doesn't have any features for comfort, e.g. construction with direct constructors (addPair(JSONNumber("a",2)))
 *
 * Caveats:
 * 	* This class stores pointers to the pairs, i.e. you shouldn't use a JSONObject
 * 	after one or more of the added pairs have been destroyed.
 *
 * Example:
 *
    JSONBoolean b("status",true);
    JSONNumber n("error-code",43);
    JSONString s("error","xyz");

    p.addPairs(b,n);

    JSONObjectPair op("xyz",p);

    o.addPairs(s,op);

    JSONObjectList l("list");

    l.addValue(o);
    l.addValue(p);

    q.addPair(l);

    std::cout << q.toString() << std::endl;
 */
class JSONObject
{
public:
    void addPair(const JSONPair& p);

    template<typename T, typename ... Ts>
    void addPairs(const T& pair, const Ts& ... pairs) { addPair(pair); addPairs(pairs...); }

    template<typename T>
    void addPairs(const T& pair) { addPair(pair); }

    string toString(void) const;

private:
    std::list<const JSONPair*> pairs;
};

class JSONPair
{
public:
    virtual string toString(void) const;

    JSONPair(const string& k) : key(k) {}
protected:
    virtual string valueToString(void) const = 0;
    string key;
};

class JSONNumber : public JSONPair
{
public:

    JSONNumber(const string& k, int v) : JSONPair(k), value(v) {}

private:
    string valueToString(void) const { std::ostringstream s; s << value; return s.str(); }
    int value;
};

class JSONString : public JSONPair
{
public:

    JSONString(const string& k,const string& v) : JSONPair(k), value(v) {}

private:
    string valueToString(void) const { return value; }
    string value;
};

class JSONBoolean : public JSONPair
{
public:

    JSONBoolean(const string& k,bool v) : JSONPair(k), value(v) {}

private:
    string valueToString(void) const { return value ? "true" : "false"; }
    bool value;
};

class JSONObjectPair : public JSONPair
{
public:

    JSONObjectPair(const string& k, const JSONObject& obj) : JSONPair(k), value(obj) {}

private:
    string valueToString(void) const { return value.toString(); }
    JSONObject value;
};

/**
 * A list of JSON objects [{x:y,z:a},{b:c,d:e}]
 */
class JSONObjectList : public JSONPair
{
public:
    JSONObjectList(const string& k) : JSONPair(k) {}

    void addValue(const JSONObject& obj) { objs.push_back(&obj); };

private:
    string valueToString(void) const;
    std::list<const JSONObject*> objs;

};

# endif
