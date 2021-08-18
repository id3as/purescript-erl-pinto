-module(pinto_genServer@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ selfFFI/0
        , startLinkFFI/3
        , callFFI/2
        , castFFI/2
        , replyTo/2
        , stopFFI/1
        ]).


-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        ]).


startLinkFFI(MaybeName, Module, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            gen_server:start_link(Module, [InitEffect], []);
          {just, Name} ->
            gen_server:start_link(Name, Module, [InitEffect], [])
        end,

      start_link_result_to_ps(Result)
  end.

castFFI(ServerRef, CastFn) ->
  fun() ->
      gen_server:cast(ServerRef, CastFn),
      unit
  end.

callFFI(ServerRef, CallFn) ->
  fun() ->
      gen_server:call(ServerRef, CallFn)
  end.

replyTo(From, Reply) ->
  fun() ->
      gen_server:reply(From, Reply),
      unit
  end.

selfFFI() ->
  fun() ->
      self()
  end.

stopFFI(ServerRef) ->
  fun() ->
      gen_server:stop(ServerRef),
      unit
  end.



