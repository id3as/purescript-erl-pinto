-module(pinto@foreign).

-export([ isRegisteredImpl/1
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
