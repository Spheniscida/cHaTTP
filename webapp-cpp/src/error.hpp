# ifndef ERROR_HPP
# define ERROR_HPP

# include <string>
# include <memory>

struct WebappError
{
    std::string error_message;

    WebappError(const std::string& msg) : error_message(msg) {}
    WebappError(std::string&& msg) : error_message(std::move(msg)) {}
};

# endif
