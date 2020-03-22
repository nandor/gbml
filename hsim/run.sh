#!/bin/sh

set -e

ROOT=`dirname $(realpath "$0")`

(cd $ROOT && make)

vvp -M$ROOT -mgbenv $ROOT/gbsim.vvp rom=$1
