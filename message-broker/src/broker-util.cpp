# include "broker-util.hpp"

std::fstream urandom;

void initializeUrandomSource(void)
{
    urandom.open("/dev/urandom",std::ios_base::in);
}

std::string generateChannelId(void)
{
    char channel_id[65];

    channel_id[64] = 0;

    urandom.get(channel_id,64);

    for ( unsigned int i = 0; i < 64; i++ )
    {
	channel_id[i] = 'a' + ( (channel_id[i]  < 0 ? - channel_id[i] : channel_id[i]) % 26 );
    }

    return std::string(channel_id);
}
