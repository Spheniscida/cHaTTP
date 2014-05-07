# ifndef SYNCHRONIZATION_HPP
# define SYNCHRONIZATION_HPP

# include <thread>
# include <mutex>

using std::lock_guard;
using std::mutex;
using std::unique_lock;

extern mutex output_mutex;
extern mutex urandom_mutex;

# endif