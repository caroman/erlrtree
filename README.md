erlrtree
========

Rtree server with Geos/STRtree

This is a project to learn and play with Erlang, specially in how integrating
the OGR and GEOS bindings.

Install and Run
---------------

Dependencies
~~~~~~~~~~~~

* py\_interface --  https://github.com:skysbird/py\_interface
* rebar --  https://github.com:basho/rebar
* GEOS Binding --  https://github.com:caroman/erlgeom
* OGR Binding --  https://github.com:caroman/erlogr
* OSR Binding --  https://github.com:caroman/erlosr

Bindings are downloaded using rebar.

Create Node Structure
~~~~~~~~~~~~~~~~~~~~~

pushd rel/
rebar create-node nodeid=rtree
popd

Compile and Start
~~~~~~~~~~~~~~~~~

rebar get-deps
rebar compile generate

Start
~~~~~

rel/rtree/bin/rtree start

Usage
-----

attach
~~~~~~

    # rel/rtree/bin/rtree attach
    
    # rtree_server:load("/home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp").

    # rtree_server:intersects(1.0, -1.0).

erl\_call
~~~~~~~~
    
    # erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server load [\"/home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.shp\"']"

    # erl_call -c rtree -n rtree@127.0.0.1 -a \
        "rtree_server tree []"

    # erl_call -c rtree -a -n rtree@127.0.0.1 -a \
        "rtree_server intersects [1.0 -1.0]"

py\_interface
~~~~~~~~~~~~~

    # python \
         /home/caroman/Projects/skysbird/py_interface/examples/test_remote_exec.py \
         -d -t 10 -n pyrtree@127.0.0.1 -c rtree rtree@127.0.0.1 \
         rtree_server load '"/home/caroman/Projects/caroman/erlrtree/deps/erlogr/test/polygon.sh"'
     
    # python \
         /home/caroman/Projects/skysbird/py_interface/examples/test_remote_exec.py \
         -d -t 10 -n pyrtree@127.0.0.1 -c rtree rtree@127.0.0.1 \
         rtree_server tree 
     
    # python \
        /home/caroman/Projects/skysbird/py_interface/examples/test_remote_exec.py \
        -d -t 10 -n pyrtree@127.0.0.1 -c rtree rtree@127.0.0.1 \
        rtree_server intersects 1.0 -1.0

