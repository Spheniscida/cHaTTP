project(message-broker)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-g3")

find_library(boost_system_lib "boost_system")
find_library(boost_thread_lib "boost_thread")
set(boost_thread_libs ${boost_system_lib} ${boost_thread_lib})

add_subdirectory("src/")

if(unit_test)
    enable_testing()
    add_subdirectory("tests/")
endif()
