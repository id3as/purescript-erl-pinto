-module(pinto_sup@foreign).

-export([
         start_from_spec/1,
         startLinkImpl/4,
         startChildImpl/4,
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


startChildImpl(Left, Right, Name, Args) ->
  fun() ->
    case supervisor:start_child(Name, [Args]) of
      {ok, Pid}  -> Right(Pid);
      {error, E} -> Left(E)
    end
  end.

init([Effect]) ->
  Spec = Effect(),
  { ok, pinto_sup@ps:reify(Spec) }.
