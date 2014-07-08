cmake_minimum_required(VERSION 2.6)

include_directories(../src/)

set(testsrc
    test.cpp
    )


find_library(boost_test_lib "boost_unit_test_framework")

add_executable(webapp-test ${testsrc})
target_link_libraries(webapp-test webapplib ${boost_test_lib} protobuf protolib fcgi)

add_test(NAME webapptest COMMAND webapp-test)
