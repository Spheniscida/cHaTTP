project(message-broker-test)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall")

include_directories("../src/")

set(persistsrc
persistence-protocol.cpp
)

set(miscsrc
misc-test.cpp
)

set(wasrc
web-app.cpp
)

set(manualsrc
manual-protocol-tester.cpp
)

find_library(boost_test_lib "boost_unit_test_framework")

add_executable(persistence-protocol ${persistsrc})
target_link_libraries(persistence-protocol msg_broker_lib ${boost_test_lib})

add_executable(misc-test ${miscsrc})
target_link_libraries(misc-test msg_broker_lib ${boost_test_lib})

add_executable(webapp-test ${wasrc})
target_link_libraries(webapp-test msg_broker_lib ${boost_test_lib})

add_executable(manual-test ${manualsrc})
target_link_libraries(manual-test msg_broker_lib socket++)

# Tests

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} "--output-on-failure")

add_test(NAME msg_broker_test_persist_protocol COMMAND persistence-protocol)
add_test(NAME msg_broker_test_misc COMMAND misc-test)
add_test(NAME msg_broker_test_wa COMMAND webapp-test)
