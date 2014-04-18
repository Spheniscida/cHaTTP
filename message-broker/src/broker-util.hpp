# ifndef BROKER_UTIL_HPP
# define BROKER_UTIL_HPP

# include <string>
# include <fstream>

extern std::string generateChannelId(void);
extern void initializeUrandomSource(void);
extern std::fstream urandom;

# endif