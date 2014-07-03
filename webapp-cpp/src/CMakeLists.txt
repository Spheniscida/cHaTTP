cmake_minimum_required(VERSION 2.6)

set(libsrc
conf.cpp
  )

add_library(webapplib STATIC ${libsrc})
