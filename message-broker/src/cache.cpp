# include "cache.hpp"

unordered_map<string,CachedUser> user_cache;
shared_mutex user_cache_mutex;

void insertUserInCache(const string& user_name, const string& channel_id, const string& broker_name, bool online)
{
    CachedUser entry = { .found = true, .channel_id = channel_id, .broker_name = broker_name, .online = online };

    unique_lock<shared_mutex> usr_wr_lck(user_cache_mutex);
	user_cache[user_name] = entry;
    usr_wr_lck.unlock();
}

CachedUser lookupUserInCache(const string& user_name)
{
    unordered_map<string,CachedUser>::const_iterator it;

    shared_lock<shared_mutex> usr_lck(user_cache_mutex);
    if ( (it = user_cache.find(user_name)) != user_cache.end() )
	return it->second;
    else return { .found = false };
}
