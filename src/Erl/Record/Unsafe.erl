-module(erl_record_unsafe@foreign).
-export([unsafeHas/2, unsafeGet/2, unsafeSet/3, unsafeDelete/2]).

unsafeHas(Label, Rec) -> maps:is_key(Label, Rec).
unsafeGet(Label, Rec) -> maps:get(Label, Rec).
unsafeSet(Label, Value, Rec) -> maps:put(Label, Value, Rec).
unsafeDelete(Label, Rec) -> maps:remove(Label, Rec).
