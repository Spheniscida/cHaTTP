# ifndef CACHE_HPP
# define CACHE_HPP

# include <unordered_map>
# include <string>

# include "synchronization.hpp"

using std::unordered_map;
using std::string;

struct CachedUser;

extern unordered_map<string,CachedUser> user_cache;
extern shared_mutex user_cache_mutex;

/**
 * @brief A cache entry of a user.
 *
 * Important: first check if online or not. If online, channel_id and broker_name are correct, if offline their
 * contents may be undefined.
 */
struct CachedUser
{
    string channel_id;
    string broker_name;
    bool found;
    bool online;
};

extern void insertUserInCache(const string& user_name, const string& channel_id, const string& broker_name, bool online);
extern CachedUser lookupUserInCache(const string& user_name);

# endif