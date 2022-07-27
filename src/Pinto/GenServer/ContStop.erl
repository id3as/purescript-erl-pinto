-module(pinto_genServer_contStop@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ startLinkFFI/3
        , callFFI/2
        , castFFI/2
        , replyToFFI/2
        , stopFFI/1
        , whereIs/1
        ]).


-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        ]).

-define(just(A), {just, A}).
-define(nothing, {nothing}).


startLinkFFI(MaybeName, Module, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          ?nothing ->
            gen_server:start_link(Module, [InitEffect], []);
          ?just(Name) ->
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

replyToFFI(From, Reply) ->
  fun() ->
      gen_server:reply(From, Reply),
      unit
  end.

stopFFI(ServerRef) ->
  fun() ->
      gen_server:stop(ServerRef),
      unit
  end.

whereIs(RegistryName) ->
  %%----------------------------------------------------------------------------
  %% This function is basically (the private) 'where' from OTP's gen.erl
  %%----------------------------------------------------------------------------
  fun() ->
    RawResp =
      case RegistryName of
        {global, Name} -> global:whereis_name(Name);
        {via, Module, Name} -> Module:whereis_name(Name);
        {local, Name}  -> whereis(Name)
      end,
    undefined_to_maybe(RawResp)
  end.

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------
undefined_to_maybe(undefined) ->
  ?nothing;
undefined_to_maybe(Other) ->
  ?just(Other).
