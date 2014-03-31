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
%%%   Application behaviour implementation for rtree server.
%%% @copyright 2013 Carlos Roman
%%% @end
%%%----------------------------------------------------------------
-module(rtree_server_app).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).


%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(WAIT_FOR_SECONDS, 2500).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % ok = ensure_contcat(),
    resource_discovery:add_local_resource_tuple({rtree_supervisor, node()}),
    resource_discovery:add_target_resource_types([rtree_supervisor]),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_SECONDS),
    lager:debug("Starting rtree supervisor"),
    case rtree_supervisor:start_link() of
        {ok, Pid} ->
            lager:info("Supervisor rtree started at pid: ~p", [Pid]),
            {ok, Pid};
        Error ->
            lager:error("Supervisor rtree starting error: ~p", [Error]),
            Error
    end.

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(rtree_server),
    ?assertNot(undefined == whereis(rtree_server_sup)).

-endif.
