-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

%% FFI Exports
-export([ startLinkFFI/3
        , selfFFI/0
        , callFFI/2
        , castFFI/2
        , mkReply/1
        , parseEventFFI/2
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        ]).

%%% ----------------------------------------------------------------------------
%%% Directly Exported FFI
%%% ----------------------------------------------------------------------------
mkReply(To) ->
  fun(Reply) ->
      {reply, To, Reply}
  end.

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startLinkFFI(MaybeName, Module, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            gen_statem:start_link(Module, InitEffect, []);
          {just, Name} ->
            gen_statem:start_link(Name, Module, InitEffect, [])
        end,

      start_link_result_to_ps(Result)
  end.

callFFI(StatemRef, CallFn) ->
  fun() ->
      gen_server:call(StatemRef, CallFn)
  end.

castFFI(StatemRef, CastFn) ->
  fun() ->
      ok = gen_server:cast(StatemRef, CastFn),
      unit
  end.

selfFFI() ->
  fun() ->
      self()
  end.

parseEventFFI(T, E) ->
  case T of
    { call, From } -> {handleEventCall, From, E};
    cast -> {handleEventCast, E};
    enter -> {handleEventEnter, E};
    _Event -> {handleEvent, event_to_ps(T, E)}
  end.

event_to_ps(info, Info) -> {eventInfo, Info};
event_to_ps(internal, Internal) -> {eventInternal, Internal};
event_to_ps(timeout, Content) ->  {eventTimeout, Content};
event_to_ps({timeout, Name}, Content) -> {eventNamedTimeout, Name, Content};
event_to_ps(state_timeout, Content) -> {eventStateTimeout, Content}.
