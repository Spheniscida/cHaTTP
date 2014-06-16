#!/bin/bash

PROTOFILES="message persistence webapp messagerelay"

for I in $PROTOFILES;
    do protoc -I ../../../protocols/ ../../../protocols/${I}.proto --cpp_out .;
done;
