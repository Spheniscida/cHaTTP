project(message-broker)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS "-std=c++11")
set(CMAKE_CXX_FLAGS_DEBUG "-g3")

add_subdirectory("src/")

