cmake_minimum_required(VERSION 2.6)

set(libsrc
    conf.cpp
    error.cpp
    persistent.cpp
    sequence-number.cpp
    broker-util.cpp
    webapp-proto.cpp
    ipc.cpp
    message-relay.cpp
    broker.cpp
    synchronization.cpp
    broker2broker.cpp
    transaction-maps.cpp
    )

set(execsrc
    main.cpp
    )

add_library(msg_broker_lib STATIC ${libsrc})

add_executable(msg-broker ${execsrc})
target_link_libraries(msg-broker msg_broker_lib socket++ pthread ${PROTOBUF_LIBRARIES} protolib)

