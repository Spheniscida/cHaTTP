# ifndef SYNCHRONIZATION_HPP
# define SYNCHRONIZATION_HPP

# include <thread>
# include <mutex>

# include <boost/thread/locks.hpp>
# include <boost/thread/lock_types.hpp>
# include <boost/thread/shared_mutex.hpp>

using std::lock_guard;
using std::mutex;
using boost::unique_lock;
using boost::shared_mutex;
using boost::shared_lock;

extern mutex output_mutex;
extern mutex urandom_mutex;

# endif