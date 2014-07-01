# cHaTTP

cHaTTP is an attempt to create a modular, high-performance and also distributed realtime chat system.

It uses C++11, Haskell, Redis, some Boost libraries, nginx and the nginx\_http\_push\_module
as well as dermesser/libsocket for network communication.

In general, cHaTTP is a rather simple chat system, especially in comparison to larger systems
like Facebook Chat. For example, it lacks the following features:

* Push messages to more than one device (a user can only log in once at a time)
* Efficient group messages via pseudo-multicast (up to now, the client would have
        to handle group messages instead of the message broker distributing
        the messages.) *This feature may be introduced in future*
* Detailed online stats; i.e. a user is displayed as online or offline, but not on what sort
of device that user is online or when the user was last online.

## Dependencies

On a usual Linux system, you will need the following software packages:

* git (I'm speaking of a really untouched system)
* cmake
* g++ >= 4.8 or clang++ (probably >= 3.4, 3.5 works in either case)
* libprotobuf-dev, protoc (usually called protobuf-compiler as package)
* libsocket ([on github](https://github.com/dermesser/libsocket))
* If you want to build unit tests (not really useful): The boost testing framework (e.g. libboost-test1.55-dev in Debian)
* ghc
* cabal-install
* Redis (if "persistence-layer" is updated) or PostgreSQL (for use with persistence-pg); you may want to install
    libpq-dev before trying to install the Haskell module for Postgres.
* nginx, of course. You may download a pre-packaged nginx version including the push module at http://cdn.spheniscida.de/lbo/chattp/nginx-push-src.txz
    nginx requires libpcre, openssl and zlib (the -dev/-devel packages!)
* spawn-fcgi for starting the webapp. Or another similar tool.

## Components

See doc/architekture.mkd for a full diagram of the components.

### Browser frontend

This is a HTML/CSS/JavaScript front-end for browser clients. It will use AJAX to communicate with
the →Webapp.

### The web backend, aka "Webapp"

This is a haskell program running as a gateway between the HTTP server (nginx, connected by FastCGI)
and the →message broker.

### The "message broker"

This is one of the largest software parts of cHaTTP. Written in C++ and running multi-threaded, it handles
the distribution of messages that come in from the →Webapp and other message brokers. Messages from other
message brokers may come from other machines also running the nginx-webapp-messagebroker stack. This allows
to build clusters of cHaTTP instances.

The →message relay is used to send messages to end users.

### The persistence layer

The persistence layer is an interface to an underlying Redis database (maybe even a whole Redis cluster).
The →message broker uses this daemon to save messages for users who are currently offline and save
user information in general (passwords, online/offline status...)

Currently, there are two persistence layers:

1. The "persistence-layer", founded on Redis but not speaking the new protobuf-based protocol
2. The "persistence-pg" application, working together with PostgreSQL and therefore being a bit slower.

### The message relay

The message relay is a gateway used by the →message broker to send messages to users. It translates requests
to HTTP and uses a publisher path of nginx (in combination with the excellent nginx\_http\_push\_module
written by @slact) to send them to the actual clients.

## Real-time performance

There are major differences in the performance of cHaTTP between clustered and non-clustered (stand-alone)
mode.

These numbers are not representative; they were measured using the excellent HTTP benchmarking tool `wrk` over a 1 GBit Ethernet
connection on a 2010 Core i5 CPU having 4 cores in an amd64 machine using 4 GB of RAM running Linux 3.15.
The CPU load during the tests went no higher than 50% utilization (on every core).

It's important to note that the message broker is no bottleneck in any case; the critical components are
the message relay and the FastCGI webapp. Nginx isn't suspicious as well.

### Stand-alone (user cache active)

* Sending messages: This is probably the slowest operation. The peak value was 590 messages/second, but
    it could go down as low as 300 msgs/sec.
* User lookups (stand-alone: cached): Around 1000 ops/second.

### Clustered (no user cache)

* In clustered mode, the rate of sending messages may be reduced to less than half the rate reached with
    user cache. This measurement has however been done using persistence-pg, and PostgreSQL is not that fast.
* The same goes for user lookups.

