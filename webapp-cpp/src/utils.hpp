# ifndef UTILS_HPP
# define UTILS_HPP

# include <string>
# include <ctime>

const char* rfc2822_date_format = "%a, %d %b %Y %T %z";

std::string get2822TimeStamp(void)
{
    std::time_t now_epoch = std::time(NULL);

    struct tm now;
    gmtime_r(&now_epoch,&now);

    char date_buffer[64];

    size_t used = std::strftime(date_buffer,64,rfc2822_date_format,&now);

    return std::string(date_buffer,used);
}

# endif
