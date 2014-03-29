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
	std::cerr << e.toString();
    }
    return 0;
}
