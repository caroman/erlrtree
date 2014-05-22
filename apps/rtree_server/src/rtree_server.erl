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

%%------------------------------------------------------------------------------
%% @author Carlos Roman <caroman@gmail.com>
%% @doc
%%   Gen Server behaviour implementation for rtree server.
%% @copyright 2013 Carlos Roman
%% @end
%%------------------------------------------------------------------------------
-module(rtree_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% =============================================================================
%%  Server Interface
%% =============================================================================
-export([
    build/1,
    create/1,
    create/2,
    delete/3,
    filter/4,
    filter_file/4,
    insert/2,
    intersects/3,
    intersects_file/3,
    load/3,
    lookup/3,
    pfilter_file/5,
    pintersects_file/4,
    start_link/2,
    status/1,
    list/0,
    stop/1
    ]).

%% =============================================================================
%% Server call back functions
%% =============================================================================
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%% =============================================================================
%%  Server initial state
%% =============================================================================
-record(state, {
    name=undefined,
    capacity=undefined,
    tree=undefined,
    table=undefined,
    wkbreader=undefined,
    tree_count=0,
    ok_count=0,
    error_count=0}).

-define(SERVER, ?MODULE).
-define(DEFAULT_CAPACITY, 10). % 10 elements per node
-define(WAIT_FOR_SECONDS, 2500).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Server start_link interface
%% Register rtree server, max of one Name per node.
%% Local registration done using rtree_server_Name as ServerName
%%
%% @spec start_link(Name, Capacity) -> {ok, Pid} | {error, Error}
%% where
%%     Name = atom()
%%     Capacity = integer()
%% @end
%%------------------------------------------------------------------------------
start_link(Name, Capacity) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    resource_discovery:add_local_resource_tuple({ServerName, node()}),
    resource_discovery:add_target_resource_types([ServerName]),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_SECONDS),
    case gen_server:start_link({local, ServerName}, ?MODULE, [Name, Capacity], []) of
        {ok, Pid} ->
            lager:info("RTree server: ~p, started at pid: ~p", [Name, Pid]),
            {ok, Pid};
        Error ->
            lager:error("RTree server: ~p, starting error: ~p", [Name, Error]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Server tree interface
%%
%% @spec build(Name) -> {ok, Tree} | {error, Reason}
%%  where
%%      Name = term()
%% @end
%%------------------------------------------------------------------------------
build(Name) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {build}).

%%------------------------------------------------------------------------------
%% @doc
%% Server delete interface
%%
%% @spec delete(Name, Id) -> [Object] | {error, Reason}
%%  where
%%      Name = atom()
%%      Id = term()
%% @end
%%------------------------------------------------------------------------------
delete(Name, Id, From) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast(ServerName, {delete, Id, From}).

%%------------------------------------------------------------------------------
%% @doc
%%  Create a rtree element with node capacity Capacity
%%
%% @spec create(Name, Capacity) -> void()
%% where
%%     Name = string()
%%     Capacity = integer()
%% @end
%%------------------------------------------------------------------------------
create(Name, Capacity) ->
    % register child  
    rtree_server_sup:start_child(Name, Capacity).

%% @spec create(Name) -> void()
%% @equiv create(Name, DefaultCapacity)
create(Name) ->
    create(Name, ?DEFAULT_CAPACITY).

%%------------------------------------------------------------------------------
%% @doc
%% Server filter interface
%%
%% @spec filter(Name, X, Y, Filter) -> {ok, bool()} | {error, Reason}
%%  where
%%      Name = atom()
%%      X = float()
%%      Y = float()
%%      Filter = fun()
%% @end
%%------------------------------------------------------------------------------
filter(Name, X, Y, Filter) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {filter, X, Y, Filter}).

%%------------------------------------------------------------------------------
%% @doc
%% Server filter interface
%%
%% @spec filter_file(Name, InputPath, OutputPath, Filter) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      Name = atom()
%%      InputPath = string()
%%      OutputPath = string()
%%      Filter = fun()
%% @end
%%------------------------------------------------------------------------------
filter_file(Name, InputPath, OutputPath, Filter) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {filter_file, InputPath, OutputPath, Filter}).

%%------------------------------------------------------------------------------
%% @doc
%% Server insert interface
%%
%% @spec insert(Name, Records) -> {ok, Bool} | {error, Reason}
%%  where
%%      Name = tab()
%%      Records = [tuple()]
%% @end
%%------------------------------------------------------------------------------
insert(Name, Records) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {insert, Records}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects interface
%%
%% @spec intersects(Name, X, Y) -> {ok, bool()} | {error, Reason}
%%  where
%%      X = float()
%%      Y = float()
%% @end
%%------------------------------------------------------------------------------
intersects(Name, X, Y) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {intersects, X, Y}).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects file interface
%%
%% @spec intersects_file(Name, InputPath, OutputPath) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      InputPath = string()
%%      OutputPath = string()
%% @end
%%------------------------------------------------------------------------------
intersects_file(Name, InputPath, OutputPath) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {intersects_file, InputPath, OutputPath}).

%%------------------------------------------------------------------------------
%% @doc
%% Server load interface
%%
%% @spec load(Name, Dsn) -> {ok, Bool} | {error, Reason}
%%  where
%%      Dsn = string()
%%      Bool = bool()
%% @end
%%------------------------------------------------------------------------------
load(Name, Dsn, IdIndex) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {load, Dsn, IdIndex}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Server lookup interface
%%
%% @spec lookup(Name, Id) -> [Object] | {error, Reason}
%%  where
%%      Name = atom()
%%      Id = term()
%% @end
%%------------------------------------------------------------------------------
lookup(Name, Id, From) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast(ServerName, {lookup, Id, From}).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects pool file interface
%%
%% @spec pintersects_file(Name, InputPath, OutputPath) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      InputPath = string()
%%      OutputPath = string()
%% @end
%%------------------------------------------------------------------------------
pintersects_file(Name, InputPath, OutputPath, From) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast(ServerName, {pintersects_file, InputPath, OutputPath, From}).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects pool file filter interface
%%
%% @spec pfilter_file(Name, InputPath, OutputPath, Filter) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      Name = atom()
%%      InputPath = string()
%%      OutputPath = string()
%%      Filter = fun()
%% @end
%%------------------------------------------------------------------------------
pfilter_file(Name, InputPath, OutputPath, From, Filter) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast(ServerName, {pfilter_file, InputPath, OutputPath, From, Filter}).

%%------------------------------------------------------------------------------
%% @doc
%% Server status interface
%%
%% @spec status(Name) -> {ok, State}
%% @end
%%------------------------------------------------------------------------------
status(Name) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call(ServerName, {status}).

%%------------------------------------------------------------------------------
%% @doc
%% Server List rtrees
%%
%% @spec list() -> {rtree01, rtree02, ...}
%% @end
%%------------------------------------------------------------------------------
list([]) -> {ok};
list(nomatch) ->
    {empty};
list({match,[["rtree_server_",Tree]]}) ->
    io:format(" * ~p~n", [Tree]);
list([Resource | List]) ->
    %%Tree = string:substr(atom_to_list(Resource), 14),
    Matching = re:run(atom_to_list(Resource),
                      "(rtree_server_)(.*$)",
                      [global,{capture,[1,2],list}]),
    list(Matching),

    %%atom_to_list(Resource), Use string module
    list(List).

list() ->
    io:format("List of Trees: ~n"),
    Resources = resource_discovery:get_resource_types(),
    list(Resources).

%%------------------------------------------------------------------------------
%% @doc
%% Server stop interface
%%
%% @spec stop(Name) -> ok
%%  where
%%      Name = term()
%% @end
%%------------------------------------------------------------------------------
stop(Name) ->
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast(ServerName, stop).

%% =============================================================================
%% EXPORTED FUNCTIONS/GEN_SERVER CALLBACKS
%% =============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle start/start_link server callback function
%%
%% @spec init(Args) -> {ok, State} | ignore | {stop, Reason}
%% @end
%%------------------------------------------------------------------------------
init([Name, Capacity]) ->
    process_flag(trap_exit, true),
    ServerName = list_to_existing_atom("rtree_server_" ++ atom_to_list(Name)),
    HeirTuple = {heir, erlang:whereis(rtree_supervisor), []},
    case rtree:create_ets(ServerName, HeirTuple) of
        {ok, Table} ->
                WkbReader = erlgeom:wkbreader_create(),
                {ok, #state{name=Name,
                     capacity=Capacity,
                     tree=undefined,
                     table=Table,
                     wkbreader=WkbReader,
                     ok_count=0,
                     error_count=0}};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle call server callback function
%%
%% @spec handle_call(Request, From, State) ->
%%     {reply, Reply, State} |
%%     {noreply, State} |
%%     {stop, Reason, Reply, State} |
%%     {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_call({build}, _From, State) ->
    case rtree:build_tree_from_ets(State#state.table) of
        {ok, Tree, Size} -> {reply, {ok, Size}, State#state{tree=Tree,
            tree_count=Size}};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({insert, Records}, _From, State) ->
    Table = State#state.table,
    case rtree:insert_to_ets(Table, Records) of
        {ok, NumberInserted} ->
            {reply, {ok, NumberInserted}, State#state{table=Table}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({intersects, X, Y}, _, State) ->
    if 
        State#state.tree == undefined ->
            {reply, {error, "Tree not populated yet"}, State};

        true ->
            case rtree:intersects(State#state.tree, X, Y) of
                {ok, Geoms} -> {reply, {ok, Geoms},
                    State#state{ok_count=State#state.ok_count + 1}}
                %{error, Reason} -> {reply, {error, Reason},
                %    State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_call({filter, X, Y, Filter}, _, State) ->
    if 
        State#state.tree == undefined ->
            {reply, {error, "Tree not populated yet"}, State};

        true ->
            case rtree:filter(State#state.tree, X, Y, Filter) of
                {ok, Geoms} -> {reply, {ok, Geoms},
                    State#state{ok_count=State#state.ok_count + 1}}
                %{error, Reason} -> {reply, {error, Reason},
                %    State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_call({intersects_file, InputPath, OutputPath}, _, State) ->
    if 
        State#state.tree == undefined ->
            {reply, {error, "Tree not populated yet"}, State};

        true ->
            lager:debug("Tree populated: ~p~n", [true]),
            case rtree:intersects_file(State#state.tree, InputPath, OutputPath) of
                ok ->
                    {reply, {ok, InputPath},
                        State#state{ok_count=State#state.ok_count + 1}};
                {error, Reason} ->
                    {reply, {error, Reason},
                        State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_call({load, Dsn, IdIndex}, _From, State) ->
    Table = State#state.table,
    case rtree:load_to_ets(Dsn, IdIndex, Table) of
        {ok, NumberInserted} ->
            {reply, {ok, NumberInserted}, State#state{table=Table}};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({status}, _From, State) ->
    {reply, {ok, State}, State}.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast server callback function
%%
%% @spec handle_cast(Msg, State) ->
%%     {noreply, State} | {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_cast({pintersects_file, InputPath, OutputPath, From}, #state{tree=Tree}=State) ->
    if 
        Tree == undefined ->
            lager:debug("Tree not populated yet: ~p~n", [Tree]),
            {noreply, State#state{error_count=State#state.error_count + 1}};
        true ->
            lager:debug("Call rtree_worker_pool for file: ~p~n", [InputPath]),
            case poolboy:transaction(rtree_worker_pool, fun(Worker) ->
                gen_server:call(Worker, {intersects_file, Tree, InputPath, OutputPath}) end) of
                    {ok, InputFile} ->
                        lager:debug("{ok, ~p} From: ~p~n", [InputFile, From]),
                        From ! {ok, InputFile},
                        {noreply, State#state{ok_count=State#state.ok_count + 1}};
                    {error, Reason} ->
                        lager:error("{error, ~p, ~p}~n", [InputPath, Reason]),
                        {noreply, State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_cast({pfilter_file, InputPath, OutputPath, From, Filter}, #state{tree=Tree}=State) ->
    if 
        Tree == undefined ->
            lager:debug("Tree not populated yet: ~p~n", [Tree]),
            {noreply, State#state{error_count=State#state.error_count + 1}};
        true ->
            lager:debug("Call rtree_worker_pool for file: ~p~n", [InputPath]),
            case poolboy:transaction(rtree_worker_pool, fun(Worker) ->
                gen_server:call(Worker, {filter_file, Tree, InputPath, OutputPath, Filter}) end) of
                    {ok, InputFile} ->
                        lager:debug("{ok, ~p} From: ~p~n", [InputFile, From]),
                        From ! {ok, InputFile},
                        {noreply, State#state{ok_count=State#state.ok_count + 1}};
                    {error, Reason} ->
                        lager:error("{error, ~p, ~p}~n", [InputPath, Reason]),
                        {noreply, State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_cast({delete, Id, From}, State) ->
    Table = State#state.table,
    From ! {self(), rtree:delete(Table, Id)},
    {noreply, State};
handle_cast({lookup, Id, From}, State) ->
    Table = State#state.table,
    From ! {self(), rtree:lookup(Table, Id)},
    {noreply, State};
handle_cast(stop, State) ->
    lager:debug("Cast: ~p~n", [State]),
    {stop, normal, State};
handle_cast(Cast, State) ->
    lager:debug("Cast: ~p~n", [State]),
    {stop, {"Can't not handle cast", Cast}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle info server callback function
%%
%% @spec handle_info(Info, State) ->
%%     {noreply, State} |
%%     {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    lager:debug("Info: ~p~n", [State]),
    {stop, {"Can't not handle info", Info}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Code change server callback function
%%
%% @spec code_change(OldVsn, State, Extra) ->
%%     {ok, NewState}
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    lager:debug("Code Change: ~p~n", [State]),
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Terminate server callback function. Do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%------------------------------------------------------------------------------
terminate(shutdown, State) -> 
    lager:debug("Terminate: ~p~n", [State]);
terminate(_, _) -> ok.

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% rtree module
