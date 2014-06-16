project(message-broker-test)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_BUILD_TYPE "Debug")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")

include_directories("../../src/")

set(manualsrc
manual-protobuf.cpp
persistence.pb.cc
)

add_executable(protobuf-test ${manualsrc})
target_link_libraries(protobuf-test msg_broker_lib socket++ protobuf)

