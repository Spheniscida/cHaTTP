#!/bin/bash

PROTOFILES="message.proto persistence.proto"

PATH=.cabal-sandbox/bin:$PATH

if [ -z $(which hprotoc) ];
then echo "You need the protocol-buffers compiler for Haskell, hprotoc"; exit;
fi

for F in $PROTOFILES;
    do hprotoc -I ../protocols/ -d . ../protocols/${F};
done;
