# include "conf.hpp"
# include "error.hpp"

# include <fcgiapp.h>

int createFastCGISocket(void)
{
    if ( ! getenv("CHATTP_WEBAPP_FASTCGI_PATH") )
	throw WebappError("Missing CHATTP_WEBAPP_FASTCGI_PATH environment variable.");

    return FCGX_OpenSocket(getenv("CHATTP_WEBAPP_FASTCGI_PATH"),128);
}
