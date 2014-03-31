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
    start_link/2,
    create/1,
    create/2,
    stop/1,
    build/1,
    intersects/3,
    intersects_file/3,
    pintersects_file/4,
    load/2,
    status/1
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
    resource_discovery:add_local_resource_tuple({rtree_server, 
        [{node, node()}, {name, Name}]}),
    resource_discovery:add_target_resource_types([rtree_server]),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_SECONDS),
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:start_link({local, ServerName}, ?MODULE, [Name, Capacity], []).
    %%case gen_server:start_link({local, ServerName}, ?MODULE, [Name, Capacity], []) of
    %%    {ok, Pid} ->
    %%        lager:info("RTree server: ~p, started at pid: ~p", [Name, Pid]),
    %%        {ok, Pid};
    %%    Error ->
    %%        lager:error("RTree server: ~p, starting error: ~p", [Name, Error]),
    %%        Error
    %%end.
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
%% Server stop interface
%%
%% @spec stop(Name) -> ok
%%  where
%%      Name = term()
%% @end
%%------------------------------------------------------------------------------
stop(Name) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast({local, ServerName}, stop).

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
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call({local, ServerName}, {build}).

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
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call({local, ServerName}, {intersects, X, Y}).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects interface
%%
%% @spec intersects_file(Name, InputPath, OutputPath) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      InputPath = string()
%%      OutputPath = string()
%% @end
%%------------------------------------------------------------------------------
intersects_file(Name, InputPath, OutputPath) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call({local, ServerName}, {intersects_file, InputPath, OutputPath}).

%%------------------------------------------------------------------------------
%% @doc
%% Server intersects interface
%%
%% @spec pintersects_file(Name, InputPath, OutputPath) ->
%%      {ok, InputPath} | {error, Reason}
%%  where
%%      InputPath = string()
%%      OutputPath = string()
%% @end
%%------------------------------------------------------------------------------
pintersects_file(Name, InputPath, OutputPath, From) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:cast({local, ServerName}, {pintersects_file, InputPath, OutputPath, From}).

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
load(Name, Dsn) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call({local, ServerName}, {load, Dsn}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Server status interface
%%
%% @spec status(Name) -> {ok, State}
%% @end
%%------------------------------------------------------------------------------
status(Name) ->
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    gen_server:call({local, ServerName}, {status}).


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
    ServerName = list_to_atom("rtree_server_" ++ atom_to_list(Name)),
    case rtree:create_ets(ServerName) of
        {ok, Table} ->
                {ok, #state{name=Name,
                     capacity=Capacity,
                     tree=undefined,
                     table=Table,
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
        {ok, Tree} -> {reply, ok, State#state{tree=Tree}};
        {error, Reason} -> {reply, {error, Reason}, State}
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
handle_call({load, Dsn}, _From, State) ->
    Table = State#state.table,
    case rtree:load_to_ets(Dsn, Table) of
        ok -> {reply, {ok, Table}, State#state{table=Table}};
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
