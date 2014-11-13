# erlrtree

Rtree server with Geos/STRtree

This is a project to learn and play with Erlang, specially in how integrating
the OGR and GEOS bindings. Each rtree server is a gen_server.

## Install and Run

### Dependencies

    * yum install gcc
    * yum install gdal gdal-devel
    * yum install geos geos-devel
    * make get-deps

### Create Release

    * make release

### Escript

    * make escriptize


### Start

    * rel/rtree_server/bin/rtree_server start


## Usage


### erlang console

    1. rel/rtree_server/bin/rtree_server attach
    2. rtree_server:create(tree01).
    3. rtree_server:load(tree01, "/Users/caroman/src/github/erlrtree/deps/erlogr/test/polygon.shp", 1).
    4. rtree_server:build(tree01).
    5. rtree_server:intersects(tree01, 1.0, -1.0).


### erl_call utility
    
    1. erl_call -c rtree_server -n rtree_server@127.0.0.1 -a \
        "rtree_server create [tree01]
    2. erl_call -c rtree_server -n rtree_server@127.0.0.1 -a \
        "rtree_server load [tree01 '\"/Users/caroman/src/github/erlrtree/deps/erlogr/test/polygon.shp\"']"
    3. erl_call -c rtree_server -n rtree_server@127.0.0.1 -a \
        "rtree_server build [tree01]"
    4. erl_call -c rtree_server -n rtree_server@127.0.0.1 -a \
        "rtree_server intersects [tree01 1.0 -1.0]"


### erlrtree_client python utility

    1. cd utils/py_rtree
    2. source setenv
    3. erlrtree_client.py create tree01
    4. erlrtree_client.py load tree01 /home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp
    5. erlrtree_client.py build tree01
    6. erlrtree_client.py --verbose debug intersects tree01 '1.0,1.0' '1.0,-1.0'


### rtree_client

    1.- apps/rtree_client/bin/rtree_client create blockid
    2.- apps/rtree_client/bin/rtree_client load blockid /mnt/disk/data/blockid/population_11.shp
    3.- apps/rtree_client/bin/rtree_client build blockid
    4.- apps/rtree_client/bin/rtree_client intersects blockid apps/rtree_client/test/input.csv.gz output.csv.gz



