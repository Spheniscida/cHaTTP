#!/bin/bash

# Message broker configuration.
# May also used to initialize other parts of cHaTTP.
# Just source this script (`source env_vars.sh`)

##### Application bind information

# The bind address of the webapp backend.
export CHATTP_WEBAPP_ADDR="/tmp/webapp"
export CHATTP_WEBAPP_FAMILY="UNIX"
export CHATTP_WEBAPP_PORT=""

# The bind address of the message relay application.
export CHATTP_MESG_RELAY_ADDR="/tmp/message-relay"
export CHATTP_MESG_RELAY_FAMILY="UNIX"
export CHATTP_MESG_RELAY_PORT=""

# The bind address of the persistence layer.
export CHATTP_PERSISTENCE_LAYER_ADDR="/tmp/persistence-layer"
export CHATTP_PERSISTENCE_LAYER_FAMILY="UNIX"
export CHATTP_PERSISTENCE_LAYER_PORT=""

# Database connections

# Only for conventional persistence-layer
export CHATTP_REDIS_FAMILY="INET"
export CHATTP_REDIS_ADDR="localhost"
export CHATTP_REDIS_PORT="6379"

# Only for persistence-pg

# Parameters: host port dbname user password. Quote the values in single quotes.
export CHATTP_POSTGRES_CONNECTION="dbname='chattp' user='chattp' password='chattp_default'"

##### Message broker bind ports -- the message broker uses as much as 4 sockets, each dedicated to one external application.
##### Those sockets are the ones the other applications send messages to if they want to talk to the broker.
##### The address family is the same as in the application bind addresses above.

# This one is used by the persistence layer
export CHATTP_MSGBROKER_PERSISTENCE_BIND_ADDR="/tmp/msg-broker-persistence"
export CHATTP_MSGBROKER_PERSISTENCE_BIND_PORT=""

# On this socket, the message relay may contact the broker
export CHATTP_MSGBROKER_MSGRELAY_BIND_ADDR="/tmp/msg-broker-msgrelay"
export CHATTP_MSGBROKER_MSGRELAY_BIND_PORT=""

# The webapp uses this socket to communicate with the broker
export CHATTP_MSGBROKER_WEBAPP_BIND_ADDR="/tmp/msg-broker-webapp"
export CHATTP_MSGBROKER_WEBAPP_BIND_PORT=""

##### General application configuration

# This variable determines whether a (yet experiemntal) user cache should
# be used to avoid expensive persistence lookups. The cache is only used
# if this variable is set to "N", meaning that the broker will not talk
# to other brokers.
export CHATTP_MSGBROKER_RUN_CLUSTERED="N"

# This is the host on which the broker listens. It should be a FQDN because
# it's also used in the persistence layer as identification string for a user's location.
# Other message brokers contacting the one on this host should be able to resolve this name.
export CHATTP_MSGBROKER_BROKER_NAME="vostro-linux.virt.goebo.site"

# Not more than the number of processors/cores in your machine.
export CHATTP_MSGBROKER_NUMBER_THREADS="3"

export CHATTP_MSGRELAY_PUBLISH_HOST="localhost"
export CHATTP_MSGRELAY_PUBLISH_PORT="80"

# Message may be received at $base/get
export CHATTP_MSGRELAY_PUBLISH_BASE_PATH="/test"

export CHATTP_MSGRELAY_PUBLISH_CHAN_ID_PARAMETER="channel_id"

