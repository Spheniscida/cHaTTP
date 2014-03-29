project(message-broker-test)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")

include_directories("../src/")

set(testsrc
test-main.cpp
)

#add_library(boost_unit_test_framework STATIC IMPORTED)
#set_property(TARGET boost_unit_test_framework PROPERTY IMPORTED_LOCATION "/usr/lib/x86_64-linux-gnu/libboost_unit_test_framework.a")

find_library(boost_test_lib "boost_unit_test_framework")

add_executable(test-main ${testsrc})

target_link_libraries(test-main msg_broker_lib ${boost_test_lib})

