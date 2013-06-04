-module(rtree).

-include("rtree.hrl").

-export([
    tree/1,
    intersects/3,
    load/2
    ]).

%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from ETS
%%% @spec tree(File, Name) -> atom(ok) || {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
tree(Table) ->
    Tree = erlgeom:geosstrtree_create(),
    lists:foreach(
        fun(R) -> erlgeom:geosstrtree_insert(Tree, element(4,R)) end,
        ets:match_object(Table, '$1')),
    {ok, Tree}.


%%% ----------------------------------------------------------------------------
%%% @doc Intersects X,Y point with rtree
%%% @spec intersects(float(), float()) -> [integer()]
%%% @end
%%% ----------------------------------------------------------------------------
intersects(Tree, X, Y) ->
    io:format("Intersects: ~p~n", [{X, Y}]),
    Point = {'Point', [X, Y]},
    Geom = erlgeom:to_geom(Point),
    Geoms = erlgeom:geosstrtree_query(Tree, Geom),
    {ok, Geoms}.

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
        {ok, DataSource} ->
            Layer = erlogr:ds_get_layer(DataSource, 0),
            FeatDefn = erlogr:l_get_layer_defn(Layer),
            Header = lists:map(fun(Field) -> list_to_atom(Field) end,
                tuple_to_list(erlogr:fd_get_fields_name(FeatDefn))),
            Count = erlogr:l_get_feature_count(Layer),
            Records = [feature_to_tuple(WkbReader,
                element(2, erlogr:l_get_next_feature(Layer)), %% {ok, Feature}
                Header) || _ <- lists:seq(1, Count)],
            case ets:info(Table) of
                undefined -> ets:new(Table, [set, named_table,
                     {keypos, 5}, %% first 4 values are header,srid,wkb,geom
                     {read_concurrency, true}]);
                _ -> ok
            end,
            lists:foreach(fun(R) -> ets:insert(Table, R) end, Records);
        {error, Reason} ->
            {error, Reason}
    end,
    {ok, Table}.

%%% ----------------------------------------------------------------------------
%%% @doc Helper to convert Feature from Layer into a Record for the ETS
%%% @spec feature_to_record(WkbReader, Feature Header) -> record(feature)
%%% @end
%%% ----------------------------------------------------------------------------
feature_to_tuple(WkbReader, Feature, Header) ->
    Geom = erlogr:f_get_geometry_ref(Feature),
    Wkb = erlogr:g_export_to_wkb(Geom),
    GeosGeom = erlgeom:wkbreader_read(WkbReader, Wkb),
    FieldsA = [Header,
        -1, % srid
        Wkb,
        GeosGeom],
    FieldsB = tuple_to_list(erlogr:f_get_fields(Feature)),
    list_to_tuple(lists:append(FieldsA, FieldsB)).
