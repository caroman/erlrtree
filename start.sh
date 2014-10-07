#!/bin/bash
DIR=$(cd $(dirname $0) && pwd)
make release && make escriptize && \
    $DIR/rel/rtree_server/bin/rtree_server start && \
    sleep 5 && \
    $DIR/apps/rtree_client/bin/rtree_client create blockid && \
    $DIR/apps/rtree_client/bin/rtree_client load blockid  $DIR/apps/rtree_client/test/polygon.shp && \
    $DIR/apps/rtree_client/bin/rtree_client build blockid && \
    $DIR/rel/rtree_server/bin/rtree_server attach
