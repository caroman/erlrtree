-module(rtree_client).
-export([main/1]).
-mode(compile).

main([StringX, StringY]) ->
    os:putenv("ESCRIPT", "1"),
    try
        X = list_to_float(StringX),
        Y = list_to_float(StringY),
        Wkt = astext(X, Y),
        io:format("Input:~f ~f = ~w\n", [X, Y, Wkt])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: rtree_client X Y\n"),
    halt(1).

astext(X, Y) ->
    Pt = {'Point',[X, Y]},
    Pt1 = erlgeom:to_geom(Pt),
    erlgeom:from_geom(Pt1).
