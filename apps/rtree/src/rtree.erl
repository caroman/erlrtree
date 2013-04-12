-module(rtree).

-export([
    intersects/2
    ]).

%%%%%
%% @spec intersects(string(), string()) -> none()
%
intersects(Latitude, Longitude) ->
    Pt = {'Point',[0.0, 1.1]},
    Pt1 = erlgeom:to_geom(Pt),
    io:format("Intersects: ~p~n", [{Latitude, Longitude}]),
    {ok, true}.

