-module(x).
-export([x/1]).

x(Count) ->
    Start = erlang:system_time(millisecond),
    Pid = spawn(fun() -> x(Start, Count) end),
    Pid ! {right, appMsg},
    ok.

x(Start, Count) ->
    receive
        Msg ->
          case Msg of
            {left, {trapExitMsg}} -> ok;

            {right, _} ->
                  self() ! {right, { appMsg}},
                  ok;
              {left, _} ->
                  ok
          end
    end,

    case Count of
        0 ->
            End = erlang:system_time(millisecond),
            io:format("Done ~p~n", [End - Start]);
        _ -> x(Start, Count -1)
    end.
