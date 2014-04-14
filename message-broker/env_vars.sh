#!/bin/bash

# Message broker configuration.
# May also used to initialize other parts of cHaTTP

export CHATTP_WEBAPP_ADDR="/tmp/path"
export CHATTP_WEBAPP_FAMILY="UNIX"
export CHATTP_WEBAPP_PORT=""

export CHATTP_MESG_RELAY_ADDR="/tmp/path2"
export CHATTP_MESG_RELAY_FAMILY="UNIX"
export CHATTP_MESG_RELAY_PORT=""

export CHATTP_PERSISTENCE_LAYER_ADDR="/tmp/path3"
export CHATTP_PERSISTENCE_LAYER_FAMILY="UNIX"
export CHATTP_PERSISTENCE_LAYER_PORT=""

export CHATTP_MSGBROKER_MSGRELAY_BIND_ADDR="/tmp/msg-broker-msgrelay"
export CHATTP_MSGBROKER_MSGRELAY_BIND_PORT=""

export CHATTP_MSGBROKER_PERSISTENCE_BIND_ADDR="/tmp/msg-broker-persistence"
export CHATTP_MSGBROKER_PERSISTENCE_BIND_PORT=""

export CHATTP_MSGBROKER_WEBAPP_BIND_ADDR="/tmp/msg-broker-webapp"
export CHATTP_MSGBROKER_WEBAPP_BIND_PORT=""
