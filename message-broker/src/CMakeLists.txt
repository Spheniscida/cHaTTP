cmake_minimum_required(VERSION 2.6)

set(libsrc
conf.cpp
error.cpp
persistent.cpp
)

set(execsrc
main.cpp
)

add_library(msg_broker_lib STATIC ${libsrc})
add_executable(msg-broker ${execsrc})

target_link_libraries(msg-broker socket pthread msg_broker_lib)
