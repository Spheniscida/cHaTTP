project(message-broker-test)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_BUILD_TYPE "Debug")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")

include_directories("../../src/")
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(manualsrc
    manual-protobuf.cpp
    )

set(protofiles
    ../../../protocols/message.proto
    ../../../protocols/persistence.proto
    ../../../protocols/webapp.proto
    ../../../protocols/messagerelay.proto
    )

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${protofiles})

add_executable(protobuf-test ${manualsrc} ${PROTO_SRCS})
target_link_libraries(protobuf-test msg_broker_lib socket++ ${PROTOBUF_LIBRARIES})

