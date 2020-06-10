-module(pinto_sup@foreign).

-export([
         start_from_spec/1,
         startLinkImpl/4,
         startChildImpl/4,
         startSpeccedChildImpl/4,
         terminateChildImpl/2,
         deleteChildImpl/2,
         init/1
        ]).

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().



startLinkImpl(Left, Right, Name, Effect) ->
  fun() ->
      case supervisor:start_link(Name, ?MODULE, [Effect]) of
        {ok, Pid}  -> Right(Pid);
        {error, E} -> Left(E)
      end
  end.

startChildImpl(AlreadyStarted, Started, Name, Args) ->
  fun() ->
    case supervisor:start_child(Name, [Args]) of
      {error, {already_started, Pid}} -> AlreadyStarted(Pid);
      { ok, Pid } -> Started(Pid)
    end
  end.

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
