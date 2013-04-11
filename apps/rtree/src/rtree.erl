-module(rtree).

-export([
    intersects/2
    ]).

%%%%%
%% @spec intersects(string(), string()) -> none()
%
intersects(Latitude, Longitude) -> 
    io:format("Intersects: ~p~n", [{Latitude, Longitude}]),
    {ok, true}.

