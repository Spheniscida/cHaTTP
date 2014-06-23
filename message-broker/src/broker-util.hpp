# ifndef BROKER_UTIL_HPP
# define BROKER_UTIL_HPP

# include <string>
# include <fstream>
# include <iostream>

using std::string;

extern std::string generateChannelId(void);
extern void initializeUrandomSource(void);
extern std::fstream urandom;

void stderrWrite(void);

unsigned int removeErrorCode(string& message);

template<typename T>
const T removeStringNewlines(const T arg)
{
    return arg;
}

template<>
const string removeStringNewlines(const string orig_msg);

template<typename T>
void stderrWrite(const T& arg)
{
    std::cerr << removeStringNewlines(arg) << std::endl;
}

template<typename T, typename ... Ts>
void stderrWrite(const T& arg, const Ts& ... args)
{
    std::cerr << removeStringNewlines(arg);
    stderrWrite(args...);
}

# endif
