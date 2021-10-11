-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

%% FFI Exports
-export([ startLinkFFI/3
        , procLibStartLinkFFI/3
        , selfFFI/0
        , callFFI/2
        , castFFI/2
        , reply/1
        , parseEventFFI/3
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        ]).

%%% ----------------------------------------------------------------------------
%%% Directly Exported FFI
%%% ----------------------------------------------------------------------------
reply(To) ->
  fun(Reply) ->
      {reply, To, Reply}
  end.

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startLinkFFI(MaybeName, Module, Spec) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            gen_statem:start_link(Module, Spec, []);
          {just, Name} ->
            gen_statem:start_link(Name, Module, Spec, [])
        end,

      start_link_result_to_ps(Result)
  end.

procLibStartLinkFFI(MaybeName, Module, Spec) ->
  fun() ->
      Pid = proc_lib:spawn_link(fun() ->
                                    Name = case MaybeName of
                                             {nothing} -> self();
                                             {just, JustName} -> JustName
                                           end,
                                    case register_name(Name) of
                                      true ->
                                         Result = case Module:init(Spec) of
                                                    {ok, State, Data} ->
                                                      {ok, State, Data, []};
                                                    {ok, State, Data, Actions} ->
                                                      {ok, State, Data, Actions};
                                                    Other ->
                                                      Other
                                                  end,

                                         case Result of
                                           {ok, State2, Data2, Actions2} ->
                                             gen_statem:enter_loop(Module, [], State2, Data2, Name, Actions2);
                                           {stop, Reason} ->
                                             {error, Reason};
                                           ignore ->
                                             ok
                                         end;
                                      {false, Pid} ->
                                        {error, {already_started, Pid}}
                                    end
                                end),
      {right, Pid}
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

parseEventFFI(T, TE, E) ->
  case T of
    { call, From } -> {handleEventCall, From, E};
    cast -> {handleEventCast, E};
    enter -> {handleEventEnter, E};
    _Event -> {handleEvent, event_to_ps(T, TE, E)}
  end.

event_to_ps(info, TE, Info) -> {eventInfo, TE(Info)};
event_to_ps(internal,_, Internal) -> {eventInternal, Internal};
event_to_ps(timeout, _, Content) ->  {eventTimeout, Content};
event_to_ps({timeout, Name}, _, Content) -> {eventNamedTimeout, Name, Content};
event_to_ps(state_timeout,_, Content) -> {eventStateTimeout, Content}.


where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).

register_name(Pid) when is_pid(Pid) ->
  true;
register_name({local, Name} = LN) ->
  try register(Name, self()) of
    true -> true
  catch
    error:_ ->
	    {false, where(LN)}
  end;
register_name({global, Name} = GN) ->
  case global:register_name(Name, self()) of
    yes -> true;
    no -> {false, where(GN)}
  end;
register_name({via, Module, Name} = GN) ->
  case Module:register_name(Name, self()) of
    yes ->
	    true;
    no ->
	    {false, where(GN)}
  end.
