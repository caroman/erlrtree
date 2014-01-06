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
-module(rtree_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% =============================================================================
%%  Server Interface
%% =============================================================================
-export([start_link/1]).

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
    ok_count=0,
    error_count=0}).

-define(SERVER, ?MODULE).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Server start_link interface
%%
%% @spec start_link(Aegs) -> {ok, Pid} | {error, Error}
%% where
%%     Args = proplists
%% @end
%%------------------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% =============================================================================
%% EXPORTED FUNCTIONS/GEN_SERVER CALLBACKS
%% =============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle start/start_link server callback function
%%
%% @spec init([]) -> {ok, State} | ignore | {stop, Reason}
%% @end
%%------------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{ok_count=0, error_count=0}}.

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
handle_call({intersects, Tree, X, Y}, _, State) ->
    if 
        Tree == undefined ->
            {reply, {error, "Tree not populated yet"}, State};

        true ->
            case rtree:intersects(Tree, X, Y) of
                {ok, Geoms} -> {reply, {ok, Geoms},
                    State#state{ok_count=State#state.ok_count + 1}}
                %{error, Reason} -> {reply, {error, Reason},
                %    State#state{error_count=State#state.error_count + 1}}
            end
    end;
handle_call({intersects_file, Tree, InputPath, OutputPath}, _, State) ->
    if 
        Tree == undefined ->
            {reply, {error, "Tree not populated yet"}, State};

        true ->
            io:format("XXX: ~p~n", [true]),
            case rtree:intersects_file(Tree, InputPath, OutputPath) of
                ok ->
                    {reply, {ok, InputPath},
                        State#state{ok_count=State#state.ok_count + 1}};
                {error, Reason} ->
                    {reply, {error, Reason},
                        State#state{error_count=State#state.error_count + 1}}
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast server callback function
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} | {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_cast(_Cast, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handle info server callback function
%%
%% @spec handle_info(Info, State) -> {noreply, State} | {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Code change server callback function
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Terminate server callback function. Do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> ok
%% @end
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% rtree module
