-module(pinto_sup@foreign).

-export([
         start_from_spec/1,
         startLinkImpl/2,
         startChildImpl/2,
         startSpeccedChildImpl/4,
         stopImpl/1,
         terminateChildImpl/2,
         deleteChildImpl/2,
         foreignToSlr/1,
         slrToForeign/1,
         foreignToScr/1,
         scrToForeign/1,
         init/1
        ]).

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

startLinkImpl(Name, Effect) ->
  fun() ->
      foreignToSlr(supervisor:start_link(Name, ?MODULE, [Effect]))
  end.

foreignToSlr({ok, Pid})  -> {ok, Pid};
foreignToSlr(ignore) -> {ignore};
foreignToSlr({error, {already_started, Pid}}) -> {alreadyStarted, Pid};
foreignToSlr({error, E}) -> {failed, E}.

slrToForeign({ok, Pid})  -> {ok, Pid};
slrToForeign({ignore}) -> ignore;
slrToForeign({alreadyStarted, Pid}) -> {error, {already_started, Pid}};
slrToForeign({failed, E}) -> {error, E}.

startChildImpl(Name, Args) ->
  fun() ->
      foreignToScr(supervisor:start_child(Name, [Args]))
  end.

stopImpl(Name) -> fun() ->
                      gen:stop(Name)
                  end.

foreignToScr({ok, Pid}) -> {childStarted, Pid};
foreignToScr({ok, Pid, Info}) -> {childStartedWithInfo, Pid, Info};
foreignToScr({error, {already_started, Pid}}) -> {childAlreadyStarted, Pid};
foreignToScr({error, already_present}) -> {childAlreadyPresent};
foreignToScr({error, Err}) -> {childFailed, Err}.

scrToForeign({childStarted, Pid}) -> {ok, Pid};
scrToForeign({childStartedWithInfo, Pid, Info}) -> {ok, Pid, Info};
scrToForeign({childAlreadyStarted, Pid}) -> {error, {already_started, Pid}};
scrToForeign({childAlreadyPresent}) -> {error, already_present};
scrToForeign({childFailed, Err}) -> {error, Err}.

startSpeccedChildImpl(AlreadyStarted, Started, Name, Args) ->
  fun() ->
    case supervisor:start_child(Name, Args) of
      {error, {already_started, Pid}} -> AlreadyStarted(Pid);
      {error, {already_started, Pid},  _Child} -> AlreadyStarted(Pid);
      { ok, Pid } -> Started(Pid);
      Other ->
        io:format(user, "What on earth ~p", [ Other ])
    end
  end.

terminateChildImpl(Name, Args) ->
  fun() ->
    supervisor:terminate_child(Name, Args)
  end.

deleteChildImpl(Name, Args) ->
  fun() ->
    supervisor:delete_child(Name, Args)
  end.

init([Effect]) ->
  Spec = Effect(),
  { ok, pinto_sup@ps:reify(Spec) }.
