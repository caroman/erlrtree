#!/bin/bash
DIR=$(dirname $(readlink -f $0))
rebar compile generate && \
    $DIR/rel/rtree_server/bin/rtree_server start && \
    sleep 5 && \
    $DIR/rel/rtree_server/bin/rtree_server attach
