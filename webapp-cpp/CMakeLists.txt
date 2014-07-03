project(webapp-cpp)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-g3")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

set(protofiles
    ../protocols/message.proto
    ../protocols/webapp.proto
  )

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})            # for the header files, include 'build/'
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${protofiles})
add_library(protolib STATIC ${PROTO_SRCS})

add_subdirectory(src)

add_executable(webapp-cpp main.cpp)
target_link_libraries(webapp-cpp socket++ pthread protobuf fcgi protolib webapplib)
