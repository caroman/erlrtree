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
-module(file_consumer).
-export([start/0,
    start_remote/0,
    query_file/3,
    stop/0,
    loop/0,
    loop_remote/0]).

start() ->
    spawn(?MODULE, loop, []).

start_remote() ->
    spawn(?MODULE, loop_remote, []).

read_file(FilePath) ->
    case file:open(FilePath, [raw,read,compressed]) of
        {ok, Device} -> 
            L = csv:parse(csv:lazy(Device)),
            {ok, L};
        {error, Reason} ->
            {error, Reason}
    end.

%%unused
%write_file(OutputFile, Lines) ->
%    io:format("Saving file: ~p~n", [OutputFile]),
%    case file:open(OutputFile, [raw,write,compressed]) of
%        {ok, Device} ->
%            lists:foreach(
%                fun(X) ->
%                    file:write(Device, string:join(X, ",")),
%                    file:write(Device, "\n")
%                end,
%                Lines),
%            file:close(Device),
%            ok;
%        {error, Reason} ->
%            {error, Reason}
%    end.

write_file(OutputFile, Lines, ResultIds) ->
    io:format("Saving file: ~p~n", [OutputFile]),
    case file:open(OutputFile, [raw,write,compressed]) of
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


extract_points(Lines, PosXString, PosYString) ->
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
query_tree(Tree, Points) ->
    Ids = lists:map(fun({X, Y}) ->
            {ok, InElements} = rtree_server:pintersects(Tree, X, Y),
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


query_tree_remote(RemoteNode, Tree, Points) ->
    Ids = lists:map(fun({X, Y}) ->
            {ok, InElements} = rpc:call(RemoteNode, rtree_server, intersects, [Tree, X, Y]),
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

query_file(RemoteNode, Tree, InputFile, OutputFile) ->
    io:format("File received: ~p~n", [InputFile]),
    case read_file(InputFile) of
        {ok, Lines} ->
            case extract_points(Lines, "longitude", "latitude") of
                {ok, Points} ->
                    ResultIds = query_tree_remote(RemoteNode, Tree, Points),
                    case write_file(OutputFile, Lines, ResultIds) of
                        ok ->
                            ok;
                        {error, Reason} -> 
                            {error, Reason}
                        end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.


query_file(Tree, InputFile, OutputFile) ->
    io:format("File received: ~p~n", [InputFile]),
    case read_file(InputFile) of
        {ok, Lines} ->
            case extract_points(Lines, "longitude", "latitude") of
                {ok, Points} ->
                    ResultIds = query_tree(Tree, Points),
                    case write_file(OutputFile, Lines, ResultIds) of
                        ok ->
                            ok;
                        {error, Reason} -> 
                            {error, Reason}
                        end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.

stop() ->
    self() ! stop.

loop() ->
    receive
        {From, {Tree, InputFile, OutputFile}} ->
            case query_file(Tree, InputFile, OutputFile) of
                ok ->
                    From ! {self(), ok, OutputFile};
                {error, Reason} ->
                    From ! {self(), error, Reason}
            end,
            loop();
        stop ->
            true
    end.

loop_remote() ->
    receive
        {From, {RemoteNode, Tree, InputFile, OutputFile}} ->
            case query_file(RemoteNode, Tree, InputFile, OutputFile) of
                ok ->
                    From ! {self(), ok, OutputFile};
                {error, Reason} ->
                    From ! {self(), error, Reason}
            end,
            loop();
        stop ->
            true
    end.
