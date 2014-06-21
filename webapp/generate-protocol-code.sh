#!/bin/bash

PROTOFILES="message.proto webapp.proto"

PATH=.cabal-sandbox/bin:$PATH

for F in $PROTOFILES;
    do hprotoc -I ../protocols/ -d . ../protocols/${F};
done;
