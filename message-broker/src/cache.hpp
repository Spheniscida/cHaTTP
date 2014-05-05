# ifndef CACHE_HPP
# define CACHE_HPP

# include <unordered_map>
# include <string>

# include "synchronization.hpp"

using std::unordered_map;
using std::string;

/**
 * @brief A User cache object stores information about users.
 *
 * This cache avoids too many lookups at the Persistence layer and
 * makes sending messages etc. faster.
 *
 * However, it does not work in clustered mode, i.e. when several
 * cHaTTP instances are networked together. In that case, the user cache
 * heavily impairs the functionality, i.e. it doesn't work.
 *
 * Control "clustered mode" with the CHATTP_MSGBROKER_RUN_CLUSTERED environment
 * variable. If it is set to "Y", the user cache is disabled.
 */
class UserCache
{
public:
    struct CachedUser;

    UserCache(void);

    void insertUserInCache(const string& user_name, const string& channel_id, const string& broker_name, bool online);
    const CachedUser& lookupUserInCache(const string& user_name);

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
private:

    CachedUser not_found;
    unordered_map<string,CachedUser> user_cache;
    shared_mutex user_cache_mutex;


};

# endif