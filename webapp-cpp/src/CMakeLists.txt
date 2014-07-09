cmake_minimum_required(VERSION 2.6)

set(libsrc
conf.cpp
fastcgi.cpp
json.cpp
requesturi.cpp
ipc.cpp
transactions.cpp
protocol.cpp
fcgi-reply.cpp
  )

add_library(webapplib STATIC ${libsrc})


