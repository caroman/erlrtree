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

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rtree_server_sup:start_link(),
    rtree_worker_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(rtree_server),
    ?assertNot(undefined == whereis(rtree_server_sup)).

-endif.
