project(message-broker-test)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")

include_directories("../src/")

set(testsrc
protocols.cpp
)

#add_library(boost_test_lib SHARED IMPORTED)
#set_property(TARGET boost_test_lib PROPERTY IMPORTED_LOCATION "/usr/lib64/libboost_unit_test_framework.so.1.54.0")

find_library(boost_test_lib "boost_unit_test_framework")

add_executable(protocols ${testsrc})

target_link_libraries(protocols msg_broker_lib ${boost_test_lib})
 
add_test(NAME msg_broker_test_main COMMAND protocols)

