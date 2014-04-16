# include "broker-util.hpp"

# include <fstream>

std::string generateChannelId(void)
{
    std::fstream urandom("/dev/urandom", std::ios_base::in);
    char channel_id[65];

    channel_id[64] = 0;

    urandom.get(channel_id,64);

    return std::string(channel_id);
}
