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

%% =============================================================================
%%  Server Interface
%% =============================================================================
-export([
    start_link/2,
    create/1,
    create/2,
    stop/1,
    tree/1,
    intersects/3,
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

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Server start_link interface
%%
%% @spec start_link(Name, Capacity) -> {ok, Pid} | {error, Error}
%% where
%%     Name = string()
%%     Capacity = integer()
%% @end
%%------------------------------------------------------------------------------
start_link(Name, Capacity) ->
    gen_server:start_link({global, Name}, ?MODULE, [Name, Capacity], []).

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
    gen_server:cast({global, Name}, stop).

%%------------------------------------------------------------------------------
%% @doc
%% Server tree interface
%%
%% @spec tree(Name) -> {ok, Tree} | {error, Reason}
%%  where
%%      Name = term()
%% @end
%%------------------------------------------------------------------------------
tree(Name) ->
    gen_server:call({global, Name}, {tree}).

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
    gen_server:call({global, Name}, {intersects, X, Y}).

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
    gen_server:call({global, Name}, {load, Dsn}).

%%------------------------------------------------------------------------------
%% @doc
%% Server status interface
%%
%% @spec status(Name) -> {ok, State}
%% @end
%%------------------------------------------------------------------------------
status(Name) ->
    gen_server:call({global, Name}, {status}).


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
    {ok, Table} = rtree:create_ets(Name),
    {ok, #state{name=Name,
            capacity=Capacity,
            tree=undefined,
            table=Table,
            ok_count=0,
            error_count=0}}.

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
handle_call({tree}, _From, State) ->
    case rtree:tree_from_ets(State#state.table) of
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
handle_cast(stop, State) ->
    io:format("Cast: ~p~n", [State]),
    {stop, normal, State};
handle_cast(Cast, State) ->
    io:format("Cast: ~p~n", [State]),
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
    io:format("Info: ~p~n", [State]),
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
    io:format("Code Change: ~p~n", [State]),
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
    io:format("Terminate: ~p~n", [State]);
terminate(_, _) -> ok.

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% rtree module
