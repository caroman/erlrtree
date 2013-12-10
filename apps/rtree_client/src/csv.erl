% http://blog.vmoroz.com/2011/01/csv-in-erlang.html
% {ok,IO} = file:open("test.txt",[raw,read]),csv:parse(csv:lazy(IO)).
-module(csv).
 
-export([parse/1, lazy/1]).
 
-define(BUFFER_SIZE, 1024).
 
lazy(IO) -> lazy(IO, []).
 
lazy(IO, [C|S]) ->
  [C|fun()-> lazy(IO, S) end];
lazy(IO, []) ->
  case file:read(IO, ?BUFFER_SIZE) of
    {ok, [C|S]} ->
      [C|fun()-> lazy(IO, S) end];
    eof ->
      []
  end.
 
parse(Data) -> 
    parse(Data, [], [], []).
 
parse([$\r|Data], Field, Fields, Lines) ->
    parse_r(Data(), Field, Fields, Lines);
parse([$\n|Data], Field, Fields, Lines) ->
    parse(Data(), [], [], [[Field|Fields]|Lines]);
parse([$,|Data], Field, Fields, Lines) ->
    parse(Data(), [], [Field|Fields], Lines);
parse([$"|Data], [], Fields, Lines) ->
    parse_q(Data(), [], Fields, Lines);
parse([C|Data], Field, Fields, Lines) -> 
    parse(Data(), [C|Field], Fields, Lines);
parse([], [], [], Lines) -> 
    lists:reverse(
        [lists:reverse(
            [lists:reverse(F) || F <- L]
         ) || L <- Lines]
    );
parse([], Field, Fields, Lines) -> 
    parse([], [], [], [[Field|Fields]|Lines]).
 
parse_r([$\n|_] = Data, Field, Fields, Lines) -> 
    parse(Data, Field, Fields, Lines).
 
parse_q([$"|Data], Field, Fields, Lines) -> 
    parse_qq(Data(), Field, Fields, Lines);
parse_q([C|Data], Field, Fields, Lines) -> 
    parse_q(Data(), [C|Field], Fields, Lines).
 
parse_qq([$"|Data], Field, Fields, Lines) -> 
    parse_q(Data(), [$"|Field], Fields, Lines);
parse_qq([C|_] = Data, Field, Fields, Lines)  
    when C == $,; C == $\r; C == $\n -> 
    parse(Data, Field, Fields, Lines);
parse_qq([], Field, Fields, Lines) -> 
    parse([], Field, Fields, Lines).
