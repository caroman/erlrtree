#!/bin/bash
DIR=$(dirname $(readlink -f $0))
rebar compile generate && \
    $DIR/rel/rtree_server/bin/rtree_server start && \
    sleep 5 && \
    $DIR/apps/rtree_client/bin/rtree_client create blockid && \
    $DIR/apps/rtree_client/bin/rtree_client load blockid  $DIR/apps/rtree_client/test/polygon.shp 1 && \
    $DIR/apps/rtree_client/bin/rtree_client build blockid && \
    $DIR/rel/rtree_server/bin/rtree_server attach
