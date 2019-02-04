-module(pinto_sup@foreign).

-export([
         start_from_spec/1,
         startLinkImpl/2,
         startChildImpl/4,
         init/1
        ]).

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

startLinkImpl(Name, Effect) ->
  fun() ->
      supervisor:start_link({local, Name}, ?MODULE, [Effect])
  end.

startChildImpl(AlreadyStarted, Started, Name, Args) ->
  fun() ->
    case supervisor:start_child(Name, [Args]) of
      {error, {already_started, Pid}} -> AlreadyStarted(Pid);
      { ok, Pid } -> Started(Pid)
    end
  end.

init([Effect]) ->
  Spec = Effect(),
  { ok, frameworkSup@ps:reify(Spec) }.

