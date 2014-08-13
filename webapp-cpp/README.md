# WEBAPP-CPP

This is a replacement for the other webapp written in Haskell. It aims to be completely compatible
but faster and more efficient (as it is written in C++). In our benchmarks, it always performed better
than the Haskell webapp; sending messages could be accelerated from max. 500 msgs/sec (Haskell webapp) to
more than 1300 msgs/sec; cached user lookups were about 10 times faster, from 1000 lookups/sec over the Haskell
webapp to more than 13000 lookups/sec with the C++ webapp. In addition, the C++ webapp runs with constant
memory whereas the Haskell webapp needs increasingly more memory and becomes slower over time.

You will need the following software as Webapp-CPP depends on them:

* libsocket (https://github.com/dermesser/libsocket or http://cdn.spheniscida.de/lbo/libsocket/ for binary packages)
* libcurl (-dev/-devel)
* libprotobuf (-dev/-devel) and the protoc compiler (protobuf-compiler in Debian)
* libfcgi (-dev/-devel)

