#!/bin/bash

protoc -I ../../../protocols/ ../../../protocols/{message,persistence}.proto --cpp_out .
