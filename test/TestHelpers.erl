-module(test_testHelpers@foreign).

-export([ sleep/1
        ]).

sleep(Ms) ->
  fun() ->
      timer:sleep(Ms),
      unit
  end.
