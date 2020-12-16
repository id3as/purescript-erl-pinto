-module(pinto_types@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ start_link_result_to_ps/1
        , registry_name_from_ps/1
        , instance_ref_from_ps/1
        ]).

start_link_result_to_ps({ok, Pid})                        -> {right, Pid};
start_link_result_to_ps(ignore)                           -> {left, {ignore}};
start_link_result_to_ps({error, {already_started, Pid}})  -> {left, {alreadyStarted, Pid}};
start_link_result_to_ps({error, Other})                   -> {left, {failed, Other}}.

registry_name_from_ps({local, _} = Local) -> Local;
registry_name_from_ps({global, _} = Global) -> Global;
registry_name_from_ps({via, _, _} = Via) -> Via.


instance_ref_from_ps({byName, PsRegistryName}) -> registry_name_from_ps(PsRegistryName);
instance_ref_from_ps({byPid, Pid}) -> Pid.