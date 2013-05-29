-module(rtree).

-include("rtree.hrl").

-export([
    create/1
    intersects/2,
    load/2
    ]).

%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from ETS
%%% the geometry objects
%%% @spec load(File, Name) -> atom(ok) || {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
create(Table) ->
    GeosSTRtree = erlgeom:geosstrtree_create(),
    lists:foreach(
        fun(R) -> erlgeom:geosstrtree_insert(GeosSTRtree, R#feature.geom) end,
        ets:match_object(Table, '$1')),
    {ok, GeosSTRtree}.


%%% ----------------------------------------------------------------------------
%%% @doc Intersects X,Y point with rtree
%%% @spec intersects(float(), float()) -> [integer()]
%%% @end
%%% ----------------------------------------------------------------------------
intersects(X, Y) ->
    io:format("Intersects: ~p~n", [{X, Y}]),
    {ok, true}.

%%% ----------------------------------------------------------------------------
%%% @doc Load File into an ETS named Name to be used by rtree as a container for
%%% the geometry objects
%%% @spec load(File, Name) -> atom(ok) || {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
load(File, Name) ->
    io:format("File to load: ~p~n", [File]),
    Table = list_to_atom(Name),
    WkbReader = erlgeom:wkbreader_create(),
    case erlogr:open(File) of
        DataSource ->
            Layer = erlogr:ds_get_layer(DataSource, 0),
            Count = erlogr:l_get_feature_count(Layer),
            %% {ok, Feature} = erlogr:l_get_next_feature(Layer)
            Features = [element(2, erlogr:l_get_next_feature(Layer)) || 
                X <- lists:seq(1, Count)],
            ets:new(Table,
                [set,
                 named_table,
                 {keypos, #feature.id},
                 {read_concurrency, true}]),
            lists:foreach(
                fun(F) -> ets:insert(Table, feature_to_record(WkbReader, F)) end,
                Features);
        {error, Reason} ->
            {error, Reason}
    end,
    {ok, Table, Count}.

%%% ----------------------------------------------------------------------------
%%% @doc Helper to convert Feature from Layer into a Record for the ETS
%%% @spec feature_to_record(Feature) -> record(feature)
%%% @end
%%% ----------------------------------------------------------------------------
feature_to_record(WkbReader, Feature) ->
    Fields = erlogr:f_get_fields(Feature),
    Geom = erlogr:f_get_geometry_ref(Feature),
    Wkb = erlogr:g_export_to_wkb(Geom),
    GeosGeom = erlgeom:wkbreader_read(WkbReader, Wkb),
    #feature{
        id=element(1,Fields),
        fields=Fields,
        feature=Feature,
        geom=GeosGeom}.

