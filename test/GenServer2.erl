-module(test_genServer2@foreign).

-export([ sleep/1
        ]).

sleep(Ms) ->
  fun() ->
      timer:sleep(Ms),
      unit
  end.
