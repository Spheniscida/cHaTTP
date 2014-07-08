# ifndef ERROR_HPP
# define ERROR_HPP

# include <string>
# include <memory>

struct WebappError
{
    // This error is meant to be sent back via HTTP as status code explanation (à la '400 Unknown request type'
    std::string error_message;
    bool server_error;

    WebappError(const std::string& msg, bool isServerError = false) : error_message(msg), server_error(isServerError) {}
    WebappError(std::string&& msg, bool isServerError = false) : error_message(std::move(msg)), server_error(isServerError) {}
};

# endif
