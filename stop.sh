#!/bin/bash
DIR=$(dirname $(readlink -f $0))

$DIR/rel/rtree_server/bin/rtree_server stop
`ps -def | grep epmd | head -1 | awk '{print "kill -9 "$2}'`

