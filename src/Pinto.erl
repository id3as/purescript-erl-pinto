-module(pinto@foreign).

-export([ isRegisteredImpl/1
        , node/0
        ]).

isRegisteredImpl(ServerName) ->
  fun() ->
      case ServerName of
        {via, Module, Name} ->
          Module:whereis_name(Name);
        {global, Name} ->
          global:whereis_name(Name);
        {local, Name} ->
          whereis(Name)
      end =/= undefined
  end.

node() ->
  fun() ->
      atom_to_binary(erlang:node(), utf8)
  end.
