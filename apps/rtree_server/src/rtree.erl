% Copyright 2013 Carlos Roman
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%%%----------------------------------------------------------------
%%% @author Carlos Roman <caroman@gmail.com>
%%% @doc
%%%   Module with rtree functions
%%% @copyright 2013 Carlos Roman
%%% @end
%%%----------------------------------------------------------------
-module(rtree).
-compile([{parse_transform, lager_transform}]).

-include("rtree.hrl").

-export([
    build_tree_from_ets/1,
    build_tree_from_records/1,
    create_ets/2,
    delete/2,
    geom_from_record/2,
    insert_to_ets/2,
    intersects/3,
    intersects_file/3,
    filter/4,
    filter_file/4,
    load_to_ets/3,
    load_to_list/2,
    lookup/2
    ]).


%%% ----------------------------------------------------------------------------
%%% @doc Create ETS Table to hold elements for the RTree
%%% @spec create_ets(Table::atom, HeirTuple::tuple) 
%%%     -> {atom(ok), Table::atom} | {atom(error), Reason::string()}
%%%     where
%%%         HeirTuple = {heir, none} | {heir, Pid::pid, HeirData::term}   
%%% @end
%%% ----------------------------------------------------------------------------
create_ets(Table, _HeirTuple) ->
    case ets:info(Table) of
        undefined -> ets:new(Table, [set, public, named_table,
            {keypos, 1}, %% first 5 values are id,srid,geos,wkb,header
            {read_concurrency, true},
            {write_concurrency, true},
            {heir, none}
            ]),
            lager:debug("ETS table created: ~p~n", [Table]),
            {ok, Table};
        Info ->
            Reason = io_lib:format("ETS table already exists: ~p", [Info]),
            lager:debug("~p", [Reason]),
            {error,  Reason}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Lookup element in ETS table
%%% @spec delete(Tree, Id) -> [tuple()]
%%% where
%%%     Tree = atom()
%%%     Id = term()
%%% @end
%%% ----------------------------------------------------------------------------
delete(Tree, Id) ->
    lager:debug("Deleting {Tree, Id}:  {~p, ~p}", [Tree, Id]),
    ets:delete(Tree, Id),
    ok.

%%% ----------------------------------------------------------------------------
%%% @doc Load record or records an rtree ETS named Name to be used by rtree
%%%
%%% @spec insert_to_ets(Table, Records) -> true
%%%   where
%%%     Table = tab()
%%%     Records = [tuple()]
%%% @end
%%% ----------------------------------------------------------------------------
insert_to_ets(Table, Records) ->
    ets:insert(Table, Records),
    {ok, length(Records)}.

%%% ----------------------------------------------------------------------------
%%% @doc Load File into an rtree ETS named Name to be used by rtree as a 
%%% container for the geometry objects
%%% @spec load_to_ets(Dsn::string, IdIndex::integer, Table::atom) ->
%%%     atom(ok) | {atom(error), Reason:string}
%%% @end
%%% ----------------------------------------------------------------------------
load_to_ets(Dsn, IdIndex, Table) ->
    case load_to_list(Dsn, IdIndex) of
        {ok, Records} ->
           insert_to_ets(Table, Records),
           {ok, length(Records)};
        {error, Reason} ->
            {error, Reason}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Load into a list of features
%%% @spec load_to_list(Dsn, IdIndex) -> [tuple()] | {atom(error), atom()}
%%% where
%%%     Dsn = string()
%%%     IdIndex = integer()
%%% @end
%%% ----------------------------------------------------------------------------
load_to_list(Dsn, IdIndex) ->
    case erlogr:open(Dsn) of
        {ok, DataSource} ->
            {ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
            {ok, FeatDefn} = erlogr:l_get_layer_defn(Layer),
            Header = list_to_tuple(lists:map(fun(Field) -> list_to_atom(Field) end,
                tuple_to_list(element(2, erlogr:fd_get_fields_name(FeatDefn))))),
            {ok, Count} = erlogr:l_get_feature_count(Layer),
            case is_integer(IdIndex) and (IdIndex > 0) and (tuple_size(Header) >= IdIndex) of
                false ->
                    Reason = io_lib:format("IdIndex ~p not valid. Header length is ~p.",
                        [IdIndex, tuple_size(Header)]),
                    {error, Reason};
                true ->
                    Records = [feature_to_tuple(Header, IdIndex,
                        element(2, erlogr:l_get_next_feature(Layer))) %% {ok, Feature}
                        || _ <- lists:seq(1, Count)],
                    {ok, Records}
            end;
        undefined ->
            lager:error("Not possible to open datasource: ~p", [Dsn]),
            {error, "Not possible to open datasource"}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Lookup element in ETS table
%%% @spec lookup(Tree, Id) -> [tuple()]
%%% where
%%%     Tree = atom()
%%%     Id = term()
%%% @end
%%% ----------------------------------------------------------------------------
lookup(Tree, Id) ->
    ets:lookup(Tree, Id).

%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from rtree ETS
%%% @spec tree_insert_record(Tree, WkbReader, Record)
%%%     -> atom(ok) | {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
tree_insert_record(Tree, WkbReader, Record) ->
    case element(4, Record) of
        undefined ->
            GeosGeom = geom_from_record(WkbReader, Record),
            NewRecord = setelement(3, Record, GeosGeom),
            erlgeom:geosstrtree_insert(Tree, GeosGeom, NewRecord);
        GeosGeom ->
            erlgeom:geosstrtree_insert(Tree, GeosGeom, Record)
    end.


%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from rtree ETS
%%% @spec build_tree_from_records(Records)
%%%     -> {atom(ok), Tree} | {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
build_tree_from_records(Records) ->
    Size = length(Records),
    case Size of
        Size when Size > 0  ->
            WkbReader = erlgeom:wkbreader_create(),
            Tree = erlgeom:geosstrtree_create(),
            lists:foreach(
                fun(R) -> tree_insert_record(Tree, WkbReader, R) end,
                Records),
            {ok, Tree};
        Size when Size == 0 ->
            {error, "Empty table"};
        _ ->
            {error, "Bad arg"}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from rtree ETS
%%% @spec build_tree_from_ets(Table) 
%%%     -> atom(ok) | {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
build_tree_from_ets(Table) ->
    WkbReader = erlgeom:wkbreader_create(),
    Tree = erlgeom:geosstrtree_create(),
    Size = lists:foldl(
        fun(R, Acc) -> 
            tree_insert_record(Tree, WkbReader, R),
            Acc + 1
        end,
        0,
        ets:match_object(Table, '$1')
    ),
    case Size of
        0 -> {ok, undefined, 0};
        _ -> {ok, Tree, Size}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Helper to convert Feature from Layer into a Record for the ETS
%%% @spec feature_to_tuple(WkbReader, Header, Feature) -> record(feature)
%%% @end
%%% ----------------------------------------------------------------------------
feature_to_tuple(Header, IdIndex, Feature) ->
    Wkb = case erlogr:f_get_geometry_ref(Feature) of
        {ok, Geom} ->
            erlogr:f_get_geometry_ref(Feature),
            {ok, Binary} = erlogr:g_export_to_wkb(Geom), %% {ok, Wkb}
            Binary; %% TODO: Add exception
        undefined ->
            undefined
    end,
    {ok, Fields} = erlogr:f_get_fields(Feature),
    Id = element(IdIndex, Fields),
    FieldsBase = [Id, Header, -1, undefined, Wkb], % header, srid, geom, wkb
    FieldsFeature = tuple_to_list(Fields),
    list_to_tuple(lists:append(FieldsBase, FieldsFeature)).

%%% ----------------------------------------------------------------------------
%%% @doc Helper to convert Feature from Layer into a Record for the ETS
%%% @spec geom_from_record(WkbReader, Header, Feature) -> record(feature)
%%% @end
%%% ----------------------------------------------------------------------------
geom_from_record(WkbReader, Record) ->
    erlgeom:wkbreader_read(WkbReader, element(5, Record)).

%%% ----------------------------------------------------------------------------
%%% @doc Intersects X,Y point with Tree
%%% @spec intersects(Tree, float(), float()) -> [Element]
%%% @end
%%% ----------------------------------------------------------------------------
intersects(Tree, X, Y) ->
    Point = erlgeom:to_geom({'Point', [X, Y]}),
    Elements = erlgeom:geosstrtree_query(Tree, Point),
    InElements = [E || E <- Elements,
        erlgeom:intersects(element(3, E), Point) == true],
    {ok, InElements}.

%%% ----------------------------------------------------------------------------
%%% @doc Apply filter fun after bbox intersection of X,Y point with Tree.
%%% Fun should return atom true to pass the filter and be in the output
%%% @spec filter(Tree, float(), float(), fun()) -> [Element]
%%% @end
%%% ----------------------------------------------------------------------------
filter(Tree, X, Y, FunStr) ->
    %% Extract fun from fun string
    {ok, Tokens, _} = erl_scan:string(FunStr),
    case erl_parse:parse_exprs(Tokens) of
        {ok, [Form]} ->
            {value, Fun, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
            Point = erlgeom:to_geom({'Point', [X, Y]}),
            Elements = erlgeom:geosstrtree_query(Tree, Point),
            InElements = [E || E <- Elements, Fun(E, Point) == true],
            {ok, InElements};
        {error, Reason} ->
            lager:error("Parsing ~p: ~p", [FunStr, Reason]),
            {error, Reason}
    end.
%%{ok, [Form]} = erl_parse:parse_exprs(Tokens),
%%Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
%%{value, Fun, _} = erl_eval:expr(Form, Bindings),

%% =============================================================================
%%  File related functions
%% =============================================================================
tree_filter_points(Tree, Points, Filter) ->
    Ids = lists:map(fun({X, Y}) ->
            {ok, InElements} = rtree:filter(Tree, X, Y, Filter),
            Size = length(InElements),
            case Size of
                Size when Size > 0  ->
                    element(1, lists:last(InElements));
                Size when Size == 0 ->
                    0
            end 
        end,
        Points),
    Ids.

tree_query_points(Tree, Points) ->
    Ids = lists:map(fun({X, Y}) ->
            {ok, InElements} = rtree:intersects(Tree, X, Y),
            Size = length(InElements),
            case Size of
                Size when Size > 0  ->
                    element(1, lists:last(InElements));
                Size when Size == 0 ->
                    0
            end 
        end,
        Points),
    Ids.

lines_extract_points(Lines, PosXString, PosYString) ->
    [Header | Content] = Lines,
    PosXIndex = string:str(Header, [PosXString]),
    PosYIndex = string:str(Header, [PosYString]),
    if
        PosXIndex == 0 ->
            {error, "Missing longitude field in input file"};
        PosYIndex == 0 ->
            {error, "Missing latitude field in input file"};
        true ->
            Points = [{list_to_float(lists:nth(PosXIndex, Line)),
                       list_to_float(lists:nth(PosYIndex, Line))} 
                       || Line <- Content],
            {ok, Points}
    end.

file_read(FilePath) ->
    case file:open(FilePath, [raw,read,compressed]) of
        {ok, Device} -> 
            L = csv:parse(csv:lazy(Device)),
            {ok, L};
        {error, Reason} ->
            {error, Reason}
    end.

file_write(OutputFilename, Lines, ResultIds) ->
    lager:info("Saving file: ~p~n", [OutputFilename]),
    case file:open(OutputFilename, [raw,write,compressed]) of
        {ok, Device} ->
            [Header | Content] = Lines,
            file:write(Device, string:join(Header, ",")),
            file:write(Device, ",id\n"),
            lists:map(
                fun({Line, Id}) ->
                    file:write(Device, string:join(Line, ",")),
                    file:write(Device, ","),
                    file:write(Device, integer_to_list(Id)),
                    file:write(Device, "\n")
                end,
                lists:zip(Content, ResultIds)),
            file:close(Device),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

filter_file(Tree, InputPath, OutputPath, Filter) ->
    lager:info("File received: ~p~n", [InputPath]),
    OutputFilename = case filelib:is_dir(OutputPath) of
        true ->
            filename:join(OutputPath, filename:basename(InputPath));
        false ->
            OutputPath
    end,
    case file_read(InputPath) of
        {ok, Lines} ->
            case lines_extract_points(Lines, "longitude", "latitude") of
                {ok, Points} ->
                    ResultIds = tree_filter_points(Tree, Points, Filter),
                    file_write(OutputFilename, Lines, ResultIds);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.
intersects_file(Tree, InputPath, OutputPath) ->
    lager:info("File received: ~p~n", [InputPath]),
    OutputFilename = case filelib:is_dir(OutputPath) of
        true ->
            filename:join(OutputPath, filename:basename(InputPath));
        false ->
            OutputPath
    end,
    case file_read(InputPath) of
        {ok, Lines} ->
            case lines_extract_points(Lines, "longitude", "latitude") of
                {ok, Points} ->
                    ResultIds = tree_query_points(Tree, Points),
                    file_write(OutputFilename, Lines, ResultIds);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.
