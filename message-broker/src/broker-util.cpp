# include "broker-util.hpp"
# include "synchronization.hpp"

# include <algorithm>

using std::for_each;

std::fstream urandom;

void initializeUrandomSource(void)
{
    urandom.open("/dev/urandom",std::ios_base::in);
}

std::string generateChannelId(void)
{
    char channel_id[65];

    channel_id[64] = 0;

    unique_lock<mutex> urandom_lock(urandom_mutex);
	urandom.get(channel_id,64);
    urandom_lock.unlock();

    for ( unsigned int i = 0; i < 64; i++ )
    {
	channel_id[i] = 'a' + ( (channel_id[i]  < 0 ? - channel_id[i] : channel_id[i]) % 26 );
    }

    return std::string(channel_id);
}

void stderrWrite(void)
{
    return;
}

template<>
const string removeStringNewlines(const string orig_msg)
{
    string msg = orig_msg;
    for_each(msg.begin(),msg.end(),[](char& c) { c == '\n' ? c = ' ' : 0; });

    return msg;
}