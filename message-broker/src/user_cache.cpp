# include "user_cache.hpp"
# include "conf.hpp"

UserCache::UserCache(void)
{
    CachedUser c;
    c.found = false;

    not_found = c;
}

void UserCache::insertUserInCache(const string& user_name, const string& channel_id, const string& broker_name, bool online)
{
    if ( ! global_broker_settings.getClusteredMode() )
    {
	CachedUser entry;

	entry.found = true;
	entry.channel_id = channel_id;
	entry.broker_name = broker_name;
	entry.online = online;

	lock_guard<mutex> usr_wr_lck(user_cache_mutex);
	user_cache[user_name] = entry;
    }
}

const UserCache::CachedUser& UserCache::lookupUserInCache(const string& user_name)
{
    if ( ! global_broker_settings.getClusteredMode() )
    {
	unordered_map<string,CachedUser>::const_iterator it;

	lock_guard<mutex> usr_lck(user_cache_mutex);
	if ( (it = user_cache.find(user_name)) != user_cache.end() )
	{
	    return it->second;
	} else
	{
	    return not_found;
	}
    } else
    {
	return not_found;
    }
}
