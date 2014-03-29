cmake_minimum_required(VERSION 2.6)

set(src
conf.cpp
error.cpp
main.cpp
)

add_executable(msg-broker ${src})

target_link_libraries(msg-broker socket pthread)
