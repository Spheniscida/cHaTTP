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

### The message relay

The message relay is a gateway used by the →message broker to send messages to users. It translates requests
to HTTP and uses a publisher path of nginx (in combination with the excellent nginx\_http\_push\_module
written by @slact) to send them to the actual clients.

