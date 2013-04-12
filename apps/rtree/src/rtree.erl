-module(rtree).

-export([
    intersects/2
    ]).

%%% @doc Intersects X,Y point with rtree
%%% @spec intersects(float(), float()) -> [integer()]
%%%
intersects(X, Y) ->
    io:format("Intersects: ~p~n", [{X, Y}]),
    {ok, true}.

