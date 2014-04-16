#!/bin/bash

rel/rtree_server/bin/rtree_server stop && \
    sleep 5 && \
    `ps -def | grep epmd | head -1 | awk '{print "kill -9 "$2}'`

