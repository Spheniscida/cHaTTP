# include <iostream>

# include "src/conf.hpp"
# include "src/error.hpp"
# include "src/json.hpp"

# include "src/url.hpp"

# include <fcgiapp.h>

int main(void)
{
    try
    {
	int fd = createFastCGISocket();
    } catch (WebappError e)
    {
	std::cerr << "Error was thrown: " << e.error_message << std::endl;
    }

    return 0;
}
