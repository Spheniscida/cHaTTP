# ifndef USER_CACHE_HPP
# define USER_CACHE_HPP

# include <unordered_map>
# include <string>
# include <list>
# include <algorithm>

# include "synchronization.hpp"
# include "conf.hpp"

# include <persistence.pb.h>

class UserCache
{
public:

    template<typename IteratorT>
    void setForUser(const std::string& user_name, IteratorT begin, IteratorT end);

    void addForUser(const std::string& user_name, const chattp::PersistenceResponse::UserLocation& loc);
    void removeForUser(const std::string& user_name, const std::string& channel_id);

    void clearForUser(const std::string& user_name);

    const std::list<chattp::PersistenceResponse::UserLocation>& getLocations(const std::string& user_name);

private:
    std::unordered_map<std::string,std::list<chattp::PersistenceResponse::UserLocation>> cache;
    static const std::list<chattp::PersistenceResponse::UserLocation> empty_list;

    mutex map_mutex;
};

/**
 * @brief Replaces the old user information (e.g. after a LOOKUP operation)
 */
template<typename IteratorT>
void UserCache::setForUser(const std::string& user_name, IteratorT begin, IteratorT end)
{
    if ( global_broker_settings.getClusteredMode() )
	return;

    lock_guard<mutex> lock(map_mutex);

    std::list<chattp::PersistenceResponse::UserLocation>& locs = cache[user_name];

    locs.clear();

    std::for_each(begin,end,[&locs,&user_name](const chattp::PersistenceResponse::UserLocation& loc) { if ( loc.user_name() == user_name) { locs.push_back(loc); } });
}

# endif
