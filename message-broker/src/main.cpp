# include "conf.hpp"
# include "error.hpp"
# include <iostream>

int main(int argc, char** argv)
{
    // This is only testing yet.
    try {
	brokerSettings b;
    } catch (brokerError e)
    {
	std::cerr << e.error_message;
    }
    return 0;
}
