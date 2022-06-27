-module(x).
-export([x/1]).

x(Count) ->
    receive
        Msg ->
            self() ! Msg,
            ok
    end,

    case Count of
        0 ->
            io:format("Done~n");
        _ -> x(Count -1)
    end.
