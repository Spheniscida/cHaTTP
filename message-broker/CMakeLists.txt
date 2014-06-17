project(message-broker)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-g3")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")


set(protofiles
    ../protocols/message.proto
    ../protocols/persistence.proto
    ../protocols/webapp.proto
    ../protocols/messagerelay.proto
    )
find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${protofiles})
add_library(protolib STATIC ${PROTO_SRCS})

add_subdirectory("src/")

if(unit_test)
    enable_testing()
    add_subdirectory("tests/")
endif()
