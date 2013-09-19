%%%----------------------------------------------------------------
%%% @author Carlos Roman <caroman@gmail.com>
%%% @doc
%%%   Application behaviour implementation for rtree server.
%%% @copyright 2013 Carlos Roman
%%% @end
%%%----------------------------------------------------------------
-module(rtree_app).

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
    rtree_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(rtree),
    ?assertNot(undefined == whereis(rtree_sup)).

-endif.
