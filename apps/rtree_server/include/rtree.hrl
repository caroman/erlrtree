-record(state, {
    name=undefined,
    capacity=undefined,
    tree=undefined,
    dirty=undefined,
    table=undefined,
    wkbreader=undefined,
    tree_count=0,
    ok_count=0,
    error_count=0}).

-record(feature,
    {id,
     fields,
     srid,
     wkb,
     geom}).


