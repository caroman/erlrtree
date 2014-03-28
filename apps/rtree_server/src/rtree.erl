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
    create_ets/1,
    load_to_ets/2,
    load_to_list/1,
    build_tree_from_ets/1,
    build_tree_from_records/1,
    intersects/3,
    intersects_file/3
    ]).


%%% ----------------------------------------------------------------------------
%%% @doc Create ETS Table to hold elements for the RTree
%%% @spec create_ets(Table::atom) 
%%%     -> {atom(ok), Table::atom} | {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
create_ets(Table) ->
    lager:error("ETS table created: ~p~n",[Table]),
    case ets:info(Table) of
        undefined -> ets:new(Table, [set, public, named_table,
            {keypos, 5}, %% first 4 values are header,srid,geos,wkb
            {read_concurrency, true}]),
            {ok, Table};
        Info ->
            Reason = io_lib:format("ETS table already exists: ~p", [Info]),
            lager:debug(Reason),
            {error,  Reason}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Load File into an rtree ETS named Name to be used by rtree as a 
%%% container for the geometry objects
%%% @spec load_to_ets(Dsn::string, Table::atom) ->
%%%     atom(ok) | {atom(error), Reason:string}
%%% @end
%%% ----------------------------------------------------------------------------
load_to_ets(Dsn, Table) ->
    case load_to_list(Dsn) of
        {ok, Records} ->
           lists:foreach(fun(R) -> ets:insert(Table, R) end, Records),
           ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Load into a list of features
%%% @spec load_to_list(Dsn) -> [tuple()] | {atom(error), atom()}
%%% @end
%%% ----------------------------------------------------------------------------
load_to_list(Dsn) ->
    WkbReader = erlgeom:wkbreader_create(),
    case erlogr:open(Dsn) of
        {ok, DataSource} ->
            {ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
            {ok, FeatDefn} = erlogr:l_get_layer_defn(Layer),
            Header = lists:map(fun(Field) -> list_to_atom(Field) end,
                tuple_to_list(element(2, erlogr:fd_get_fields_name(FeatDefn)))),
            {ok, Count} = erlogr:l_get_feature_count(Layer),
            Records = [feature_to_tuple(WkbReader, Header,
                element(2, erlogr:l_get_next_feature(Layer))) %% {ok, Feature}
                || _ <- lists:seq(1, Count)],
            {ok, Records};
        undefined -> {error, "Not possible to open datasource"}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Create STRtree from rtree ETS
%%% @spec tree_insert_record(Tree, WkbReader, Record)
%%%     -> atom(ok) | {atom(error), Reason::string()}
%%% @end
%%% ----------------------------------------------------------------------------
tree_insert_record(Tree, WkbReader, Record) ->
    case element(3, Record) of
        undefined ->
            Wkb = element(4, Record),
            Geom = erlgeom:wkbreader_read(WkbReader, Wkb),
            NewRecord = setelement(3, Record, Geom),
            erlgeom:geosstrtree_insert(Tree, Geom, NewRecord);
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
    case ets:info(Table, size) of
        Size when Size > 0  ->
            Tree = erlgeom:geosstrtree_create(),
            lists:foreach(
                fun(R) -> erlgeom:geosstrtree_insert(Tree, element(3, R), R)
                end,
                ets:match_object(Table, '$1')),
            {ok, Tree};
        Size when Size == 0 ->
            {error, "Empty table"};
        _ ->
            {error, "Bad arg"}
    end.

%%% ----------------------------------------------------------------------------
%%% @doc Helper to convert Feature from Layer into a Record for the ETS
%%% @spec feature_to_tuple(WkbReader, Header, Feature) -> record(feature)
%%% @end
%%% ----------------------------------------------------------------------------
feature_to_tuple(WkbReader, Header, Feature) ->
    {ok, Geom} = erlogr:f_get_geometry_ref(Feature),
    {ok, Wkb} = erlogr:g_export_to_wkb(Geom),
    GeosGeom = erlgeom:wkbreader_read(WkbReader, Wkb),
    FieldsA = [Header, -1, GeosGeom, Wkb], % header, srid, geom, wkb
    {ok, Fields} = erlogr:f_get_fields(Feature),
    FieldsB = tuple_to_list(Fields),
    list_to_tuple(lists:append(FieldsA, FieldsB)).

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


%% =============================================================================
%%  File related functions
%% =============================================================================
tree_query_points(Tree, Points) ->
    Ids = lists:map(fun({X, Y}) ->
            {ok, InElements} = rtree:intersects(Tree, X, Y),
            Size = length(InElements),
            case Size of
                Size when Size > 0  ->
                    element(5, lists:last(InElements));
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
