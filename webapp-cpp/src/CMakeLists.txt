cmake_minimum_required(VERSION 2.6)

set(libsrc
conf.cpp
fastcgi.cpp
json.cpp
url.cpp
ipc.cpp
transactions.cpp
  )

add_library(webapplib STATIC ${libsrc})


