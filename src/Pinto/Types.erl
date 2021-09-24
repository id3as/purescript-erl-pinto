-module(pinto_types@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ start_link_result_to_ps/1
        , start_link_result_from_ps/1
        , parseTrappedExitFFI/2
        , parseShutdownReasonFFI/1
        , whereIsNameFFI/1
        ]).


whereIsNameFFI(Name) ->
  fun() ->
    case whereis(Name) of
      undefined -> {nothing};
      Pid -> {just, Pid}
    end
  end.


start_link_result_to_ps({ok, Pid})                       -> {right, Pid};
start_link_result_to_ps(ignore)                          -> {left, {ignore}};
start_link_result_to_ps({error, {already_started, Pid}}) -> {left, {alreadyStarted, Pid}};
start_link_result_to_ps({error, Other})                  -> {left, {failed, Other}}.

start_link_result_from_ps({right, Pid})                  -> {ok, Pid};
start_link_result_from_ps({left, {ignore}})              -> ignore;
start_link_result_from_ps({left, {alreadyStarted, Pid}}) -> {error, {already_started, Pid}};
start_link_result_from_ps({left, {failed, Other}})       -> {error, Other}.

parseTrappedExitFFI({ 'EXIT', Pid,  Reason }, ExitMsg) ->
  {just, (ExitMsg(Pid))(Reason)};

parseTrappedExitFFI(_,_) ->
  {nothing}.

parseShutdownReasonFFI(normal) ->
  {reasonNormal};

parseShutdownReasonFFI(shutdown) ->
  {reasonShutdown, {nothing}};

parseShutdownReasonFFI({shutdown, Reason}) ->
  {reasonShutdown, {just, Reason}};

parseShutdownReasonFFI(SomethingElse) ->
  {reasonOther, SomethingElse}.
