-module(test_genServer@foreign).

-export([ startGprocFFI/0
        , sleep/1
        ]).

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startGprocFFI() ->
  fun() ->
      %% TODO - not this, anything but this
      code:add_patha("/Users/adrianroe/dev/rtsv2/_build/default/lib/gproc/ebin/"),
      application:ensure_started(gproc),
      unit
  end.


sleep(Ms) ->
  fun() ->
      timer:sleep(Ms),
      unit
  end.
