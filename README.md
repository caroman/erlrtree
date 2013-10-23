# erlrtree

Rtree server with Geos/STRtree

This is a project to learn and play with Erlang, specially in how integrating
the OGR and GEOS bindings. Each rtree server is a gen_server.

## Install and Run

### Dependencies

    * py_interface --  https://github.com:skysbird/py_interface
    * rebar --  https://github.com:basho/rebar
    * GEOS Binding --  https://github.com:caroman/erlgeom
    * OGR Binding --  https://github.com:caroman/erlogr
    * OSR Binding --  https://github.com:caroman/erlosr

Bindings are downloaded using rebar.


### Create Node Structure

    * pushd rel/
    * rebar create-node nodeid=rtree
    * popd


### Compile

    * rebar get-deps
    * rebar compile generate


### Start

    * rel/rtree/bin/rtree start


## Usage


### erlang console

    1. rel/rtree/bin/rtree attach
    2. rtree_server:create(tree01).
    3. rtree_server:load(tree01, "/home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp").
    4. rtree_server:tree(tree01).
    5. rtree_server:intersects(tree01, 1.0, -1.0).


### erl_call utility
    
    1. erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server create [tree01]
    2. erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server load [tree01 '\"/home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp\"']"
    3. erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server tree [tree01]"
    4. erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server intersects [tree01 1.0 -1.0]"


### erlrtree_client python utility

    1. cd utils/py_rtree
    2. source setenv
    3. erlrtree_client.py create tree01
    4. erlrtree_client.py load tree01 /home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp
    5. erlrtree_client.py build tree01
    6. erlrtree_client.py --verbose debug intersects tree01 '1.0,1.0' '1.0,-1.0'

