project(webapp-cpp)
cmake_minimum_required(VERSION 2.6)

set(protofiles
    ../protocols/message.proto
    ../protocols/webapp.proto
  )

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})            # for the header files, include 'build/'
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${protofiles})
add_library(protolib STATIC ${PROTO_SRCS})


add_executable(webapp-cpp main.cpp)
target_link_libraries(webapp-cpp socket++ pthread protobuf fcgi protolib)
