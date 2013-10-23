#!/bin/bash
DIR=$(dirname $(readlink -f $0))
rebar compile generate && \
    $DIR/rel/rtree/bin/rtree start && \
    sleep 5 && \
    $DIR/rel/rtree/bin/rtree attach
