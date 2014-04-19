# ifndef BROKER_UTIL_HPP
# define BROKER_UTIL_HPP

# include <string>
# include <fstream>
# include <iostream>
# include <algorithm>

using std::for_each;

using std::string;

extern std::string generateChannelId(void);
extern void initializeUrandomSource(void);
extern std::fstream urandom;

void vDebugWrite(void);

template<typename T>
void removeStringNewlines(T arg)
{
    return;
}
template<>
void removeStringNewlines(string& msg);

template<typename T>
void vDebugWrite(T arg)
{
    std::cerr << arg << std::endl;
}

template<typename T, typename ... Ts>
void vDebugWrite(T arg, Ts ... args)
{
    std::cerr << removeStringNewlines(arg);
    vDebugWrite(args...);
}

# endif