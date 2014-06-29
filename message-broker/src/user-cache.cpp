# include "user-cache.hpp"
# include "error.hpp"

void UserCache::addForUser(const std::string& user_name, const chattp::PersistenceResponse::UserLocation& loc)
{
    if ( global_broker_settings.getClusteredMode() )
	return;

    lock_guard<mutex> lock(map_mutex);

    debug_log("Added user location: ",loc.DebugString());
    (cache[user_name]).push_back(loc);
}

void UserCache::removeForUser(const std::string& user_name, const std::string& channel_id)
{
    if ( global_broker_settings.getClusteredMode() )
	return;

    lock_guard<mutex> lock(map_mutex);

    std::list<chattp::PersistenceResponse::UserLocation>& locs = cache[user_name];

    debug_log("Removed user_location: ", channel_id);
    locs.remove_if([&channel_id](const chattp::PersistenceResponse::UserLocation& loc) -> bool { return loc.channel_id() == channel_id; });
}

void UserCache::clearForUser(const string& user_name)
{
    if ( global_broker_settings.getClusteredMode() )
	return;

    lock_guard<mutex> lock(map_mutex);

    (cache[user_name]).clear();
}

const std::list<chattp::PersistenceResponse::UserLocation>& UserCache::getLocations(const string& user_name)
{
    if ( global_broker_settings.getClusteredMode() )
	return empty_list;

    lock_guard<mutex> lock(map_mutex);

    return cache[user_name];
}

const std::list<chattp::PersistenceResponse::UserLocation> UserCache::empty_list;
