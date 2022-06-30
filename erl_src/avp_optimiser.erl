-module(avp_optimiser).

-export([ parse_transform/2
        ]).

-define(match_function(Name, Arity, Clauses), {function, _, Name, Arity, Clauses}).
-define(match_clause(Args, Guards, Body), {clause, _, Args, Guards, Body}).
-define(match_call(Fun, Args), {call, _, Fun, Args}).
-define(local_call(Fun), {atom, _, Fun}).
-define(remote_call(Mod, Fun), {remote, _, {atom, _, Mod}, {atom, _, Fun}}).
-define(match_atom(Atom), {atom, _, Atom}).
-define(match_var(Name), {var, _, Name}).
-define(match_map(Fields), {map, _, Fields}).
-define(map_field(Name, Value), {map_field_assoc, _, Name, Value}).

-define(make_call(Fun, Args), {call, 0, Fun, Args}).
-define(make_local_call(Name), ?make_atom(Name)).
-define(make_remote_call(Mod, Fun), {remote, 0, ?make_atom(Mod), ?make_atom(Fun)}).
-define(make_atom(Name), {atom, 0, Name}).
-define(make_var(Name), {var, 0, Name}).
-define(make_clause(Args, Guards, Body), {clause, 0, Args, Guards, Body}).
-define(make_case(Value, Clauses), {'case', 0, Value, Clauses}).
-define(make_tuple(Items), {tuple, 0, Items}).
-define(make_map(Fields), {map, 0, Fields}).
-define(make_map_field(Name, Value), {map_field_assoc, 0, Name, Value}).
%% dups(Forms) ->
%%   walk(Forms, fun dups/2, maps:new()).

%% strip_lines(Forms) ->
%%   element(1, modify(Forms, fun preIdentity/2, fun do_strip/2, undefined)).

%% do_strip(Form, State) when is_tuple(Form) ->
%%   {setelement(2, Form, 0), State};

%% do_strip(Form, State) ->
%%   {Form, State}.

%% dups(Form, Map) ->
%%   Key = erlang:phash2(Form),
%%   case maps:find(Key, Map) of
%%     {ok, {N, T, Size}} ->
%%       maps:put(Key, {N + 1, T, Size}, Map);
%%     error ->
%%       maps:put(Key, {1, Form, erts_debug:size(Form)}, Map)
%%   end.

%% parse_transform(Forms = [{attribute, _, file, _}, {attribute, _, module, Module} | _], _Options) -> %%when Module == workflow_runtime_workflowProcessor@ps ->
%%   case Module of
%%     'conflux_links@ps' ->
%%     %%'tC@ps' ->
%%       Map = dups(strip_lines(Forms)),
%%       Top = lists:reverse(lists:sort(maps:values(Map))),
%%       io:format(user, "~p: ~p, ~p~n", [Module, maps:size(Map), Top]),
%%       ok;
%%     _ ->
%%       ok
%%   end,
%%   Forms;

%% - _Newtype (todo comment below) - needs doing properly
%% - Erl.Data.List.uncons
%% - map with functorFn - look for `<$> Just` for example
%% - compose with semigroupFn
%% - spot mkFn[N] where args are destructured and return it to a non-curried form
%% - data_boolean@ps:otherwise() / when used in guards?



%% any() -> (?MEMOIZE((data_foldable@ps:any((erl_data_list_types@ps:foldableList()), (data_heytingAlgebra@ps:heytingAlgebraBoolean()))))).
%% -file("/Users/steve/dev/id3as.playground/server/src/Workflow/Runtime/Common.purs", 0).
%% -spec all() -> any().
%% all() -> (?MEMOIZE((data_foldable@ps:all((erl_data_list_types@ps:foldableList()), (data_heytingAlgebra@ps:heytingAlgebraBoolean()))))).

parse_transform(Forms = [{attribute, _, file, _}, {attribute, _, module, Module} | _], _Options) ->

  case is_purs(Module) of
    true ->
      Final = lists:foldl(fun(Fn, Acc) ->
                              Fn(Acc)
                          end,
                          Forms,
                          [ fun unroll/1
                          , fun add_reflect_symbol_as_atom/1
                          , fun optimise_reflect_symbol/1
                          ]
                         ),

      %% Debug output...
      case os:getenv("PURS_OPTIMISER_DEBUG") of
        false ->
          ok;
        _ ->
          filelib:ensure_dir("/tmp/purs_optimiser/foo.txt"),
          _ = file:delete("/tmp/purs_optimiser/" ++ atom_to_list(Module) ++ ".erl"),
          file:write_file("/tmp/purs_optimiser/" ++ atom_to_list(Module) ++ ".forms", io_lib:format("~p~n.", [Final])),
          lists:foreach(fun(Form) ->
                            ok = file:write_file("/tmp/purs_optimiser/" ++ atom_to_list(Module) ++ ".erl", erl_pp:form(Form, [{indent, 2}, {linewidth, 120}]), [append]),
                            ok
                        end,
                        Final)
      end,

      Final;
    _ ->
      Forms
  end;

parse_transform(Forms, _Options) ->
  Forms.

%%------------------------------------------------------------------------------
%%-- Unroll
unroll(Forms) ->
  {NewForms, _} = modify(Forms, fun preIdentity/2, fun unroll_form/2, 0),
  NewForms.

unroll_form(_Form = ?match_call(?remote_call('common_utils@ps', 'unsafeFromJust'),
                                 [Message, Value]),
             N) ->
  Var = list_to_atom("__@A" ++ integer_to_list(N)),
  {{'case', 0, Value, [ {clause, 0, [{tuple, 0, [{atom, 0, nothing}]}], [], [{call, 0, {remote, 0, {atom, 0, 'partial_unsafe@ps'}, {atom, 0, unsafeCrashWith}}, [Message]}]}
                      , {clause, 0, [{tuple, 0, [{atom, 0, just}, {var, 0, Var}]}], [], [{var, 0, Var}]}
                      ]}, N + 1};

unroll_form(_Form = ?match_call(?remote_call('workflow_nodeM@ps', 'runNodeM'),
                                [Context, Fn]
                               ), State) ->
  {?make_call(Fn, [Context]), State};

%% Not currently in use, since the mapping fun itself is not currently inlined
%% unroll_form(_Form = ?match_call(
%%                        ?match_call(
%%                           ?match_call(
%%                              ?local_call(memoize),
%%                              [?match_call(
%%                                  ?remote_call('data_functor@ps', map),
%%                                  [?match_map([
%%                                               ?map_field(?match_atom(map),
%%                                                          {'fun', _, {clauses, [{clause, _, [?match_var(F)], [],
%%                                                                                 [{'fun', _, {clauses, [{clause, _, [?match_var(V)], [],
%%                                                                                                         [{'case', _, {tuple, _, [?match_var(F), ?match_var(V)]}, Body}
%%                                                                                                         ]
%%                                                                                                         }]}}
%%                                                                                 ]
%%                                                                                }
%%                                                                               ]}}
%%                                                         )
%%                                              ])]
%%                                 )
%%                              ]
%%                             ),
%%                           [Fn]
%%                          ),
%%                        [Value]
%%                       ), N) ->
%%   {{'case', 0, {tuple, 0, [Fn, Value]}, Body}, N};

unroll_form(Form, State) ->
  {Form, State}.

%%------------------------------------------------------------------------------
%%-- add_reflect_symbol_as_atom - looks for the creation of the IsSymbol typeclass,
%%-- and adds a reflectSymbolAtom function
add_reflect_symbol_as_atom(Forms) ->
  {NewForms, _} = modify(Forms, fun find_reflect_symbol/2, fun postIdentity/2, undefined),
  NewForms.

find_reflect_symbol(?match_map([
                                ReflectSymbolString = ?map_field(?match_atom(reflectSymbol),
                                                                 { 'fun'
                                                                 , _
                                                                 , { clauses
                                                                   , [?match_clause(
                                                                        [?match_var('_')]
                                                                      , []
                                                                      , [{bin, _, [{ bin_element
                                                                                   , _
                                                                                   , {string, _, _}
                                                                                   , default
                                                                                   , [utf8]
                                                                                   }
                                                                                  ]}]
                                                                      )
                                                                     ]
                                                                   }
                                                                 }
                                                                )
                               ]),
                    State) ->

  {ReflectSymbolAtom, TheAtom} = modify(ReflectSymbolString, fun make_reflect_symbol_atom/2, fun postIdentity/2, {}),
  Form2 = ?make_map([ReflectSymbolString,
                     ReflectSymbolAtom,
                     ?make_map_field(?make_atom(reflectSymbolAtomZeroArity), ?make_atom(TheAtom))
                     ]),

  {replace, Form2, State};

find_reflect_symbol(_, State) ->
  {undefined, State}.

make_reflect_symbol_atom(?match_atom(reflectSymbol), State) ->
  {replace, ?make_atom(reflectSymbolAtom), State};

make_reflect_symbol_atom({bin, _, [{ bin_element
                                   , _
                                   , {string, _, Str}
                                   , default
                                   , [utf8]
                                   }
                                  ]}, _State) ->
  {replace, ?make_atom(list_to_atom(Str)), list_to_atom(Str)};

make_reflect_symbol_atom(_, State) ->
  {undefined, State}.

%%------------------------------------------------------------------------------
%%-- optimise_reflect_symbol - looks for calls to reflectSymbolAsAtom and replaces
%%-- with a call to the a reflectSymbolAsAtom function
optimise_reflect_symbol(Forms) ->
  {NewForms, _} = modify(Forms, fun optimise_reflect_symbol_form/2, fun postIdentity/2, undefined),
  NewForms.

optimise_reflect_symbol_form(?match_call(
                                ?match_call(?remote_call('common_utils@ps', reflectSymbolAsAtom),
                                            [IsSymbolDict]),
                                [_Sym]),
                             State) ->

  NewForm = ?make_call(?make_remote_call(maps, get), [?make_atom(reflectSymbolAtom), IsSymbolDict]),

  {replace, NewForm, State};

optimise_reflect_symbol_form(?match_call(?remote_call('erl_atom@ps', atom),
                                         [ ?match_call(
                                              ?match_call(?remote_call('data_symbol@ps', reflectSymbol), [IsSymbolDict]),
                                              [_Sym]
                                             )
                                         ]
                                        ),
                             State) ->
  NewForm = ?make_call(?make_remote_call(maps, get), [?make_atom(reflectSymbolAtom), IsSymbolDict]),

  {replace, NewForm, State};

optimise_reflect_symbol_form(?match_call(?remote_call('erl_atom@ps', atom),
                                         [ ?match_call(?remote_call('data_symbol@ps', reflectSymbol), [IsSymbolDict, _Sym])
                                         ]
                                        ),
                             State) ->
  NewForm = ?make_call(?make_remote_call(maps, get), [?make_atom(reflectSymbolAtomZeroArity), IsSymbolDict]),

  {replace, NewForm, State};

optimise_reflect_symbol_form(?match_call(?remote_call('erl_atom_symbol@ps', toAtom),
                                         [ ?match_var(IsSymbolDict)
                                         , ?match_call(?remote_call('erl_atom_symbol@ps', atom), [?match_var(IsSymbolDict)])
                                         ]
                                        ),
                             State) ->
  NewForm = ?make_call(?make_remote_call(maps, get), [?make_atom(reflectSymbolAtomZeroArity), ?make_var(IsSymbolDict)]),

  {replace, NewForm, State};

optimise_reflect_symbol_form(?match_call(?remote_call('data_eq@ps', eq),
                                         [ ?match_call(?remote_call('erl_atom@ps', 'atomEq'), [])
                                         , Arg1
                                         , Arg2
                                         ]
                                        ),
                             State) ->
  NewForm = {op, 0, '==', optimise_reflect_symbol(Arg1), optimise_reflect_symbol(Arg2)},

  {replace, NewForm, State};

optimise_reflect_symbol_form(_, State) ->
  {undefined, State}.


%%------------------------------------------------------------------------------
%%-- Helpers
is_purs(Module) ->
  case lists:reverse(atom_to_list(Module)) of
    [$s, $p, $@ | _] -> true;
    _ -> false
  end.

preIdentity(_, State) -> {undefined, State}.
postIdentity(X, State) -> {X, State}.

%%------------------------------------------------------------------------------
%%-- Walker
walk(List, Fun, State) when is_list(List) ->
  lists:foldl(fun(Item, InnerState) -> walk(Item, Fun, InnerState) end,
              State,
              List);

walk(Form = {clause, _Line, Args, Guards, Statements}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Args, Fun, State2),
  State4 = walk(Guards, Fun, State3),
  walk(Statements, Fun, State4);

walk(Form = {'case', _Line, Of, Clauses}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Of, Fun, State2),
  walk(Clauses, Fun, State3);

walk(Form = {'block', _Line, Statements}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Statements, Fun, State2);

walk(Form = {'match', _Line, Var, Statement}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Var, Fun, State2),
  walk(Statement, Fun, State3);

walk(Form = {'call', _Line, Target, Args}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Target, Fun, State2),
  walk(Args, Fun, State3);

walk(Form = {'var', _Line, _Name}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'op', _Line, _Operator, Lhs, Rhs}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Lhs, Fun, State2),
  walk(Rhs, Fun, State3);

walk(Form = {'op', _Line, _Operator, Lhs}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Lhs, Fun, State2);

walk(Form = {'integer', _Line, _Val}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'float', _Line, _Val}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'bin', _Line, _Value}, Fun, State) ->
  Fun(Form, State); %% todo

walk(Form = {'remote', _Line, {atom, _, _Module}, {atom, _, _Name}}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'fun', _Line, {clauses, Clauses}}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Clauses, Fun, State2);

walk(Form = {'fun', _Line, {function, _Module, _Name, _Arity}}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'fun', _Line, {function, _Name, _Arity}}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'function', _Line, _Name, _, Body}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Body, Fun, State2);

walk(Form = {'named_fun', _Line, _Name, Clauses}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Clauses, Fun, State2);

walk(Form = {'tuple', _Line, Statements}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(Statements, Fun, State2);

walk(Form = {'cons', _Line, Head, Tail}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Head, Fun, State2),
  walk(Tail, Fun, State3);

walk(Form = {'map', _Line, MapStatements}, Fun, State) ->
  State2 = Fun(Form, State),
  walk(MapStatements, Fun, State2);

walk(Form = {'map', _Line, Map, MapStatements}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Map, Fun, State2),
  walk(MapStatements, Fun, State3);

walk(Form = {'nil', _Line}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'atom', _Line, _}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'char', _Line, _}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'eof', _Line}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'attribute', _Line, _Attribute, _Value}, Fun, State) ->
  Fun(Form, State);

walk(Form = {'map_field_exact', _Line, Key, Value}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Key, Fun, State2),
  walk(Value, Fun, State3);

walk(Form = {'map_field_assoc', _Line, Key, Value}, Fun, State) ->
  State2 = Fun(Form, State),
  State3 = walk(Key, Fun, State2),
  walk(Value, Fun, State3).

%%------------------------------------------------------------------------------
%% Modifier
modify(List, PreFun, PostFun, State) when is_list(List) ->
  case PreFun(List, State) of
    {undefined, State2} ->
      {Out, State3} = lists:foldl(fun(Item, {Acc, InnerState}) ->
                                      {Item2, InnerState2} = modify(Item, PreFun, PostFun, InnerState),
                                      {[Item2 | Acc], InnerState2}
                                  end,
                                  {[], State2},
                                  List),

      PostFun(lists:reverse(Out), State3);

    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {attribute, _, _, _}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun(Form, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {function, Line, Name, Arity, Body}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Body2, State3} = modify(Body, PreFun, PostFun, State2),
      PostFun({function, Line, Name, Arity, Body2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {eof, _Line}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun(Form, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {clause, Line, Args, Guards, Statements}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Args2, State3} = modify(Args, PreFun, PostFun, State2),
      {Guards2, State4} = modify(Guards, PreFun, PostFun, State3),
      {Statements2, State5} = modify(Statements, PreFun, PostFun, State4),
      PostFun({clause, Line, Args2, Guards2, Statements2}, State5);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'case', Line, Of, Clauses}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Of2, State3} = modify(Of, PreFun, PostFun, State2),
      {Clauses2, State4} = modify(Clauses, PreFun, PostFun, State3),
      PostFun({'case', Line, Of2, Clauses2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'block', Line, Statements}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Statements2, State3} = modify(Statements, PreFun, PostFun, State2),
      PostFun({'block', Line, Statements2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'match', Line, Var, Statement}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Statement2, State3} = modify(Statement, PreFun, PostFun, State2),
      PostFun({'match', Line, Var, Statement2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'call', Line, Target, Args}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Target2, State3} = modify(Target, PreFun, PostFun, State2),
      {Args2, State4} = modify(Args, PreFun, PostFun, State3),
      PostFun({'call', Line, Target2, Args2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'var', Line, Name}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'var', Line, Name}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'op', Line, Operator, Lhs, Rhs}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Lhs2, State3} = modify(Lhs, PreFun, PostFun, State2),
      {Rhs2, State4} = modify(Rhs, PreFun, PostFun, State3),
      PostFun({'op', Line, Operator, Lhs2, Rhs2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'op', Line, Operator, Lhs}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Lhs2, State3} = modify(Lhs, PreFun, PostFun, State2),
      PostFun({'op', Line, Operator, Lhs2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'integer', Line, Val}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'integer', Line, Val}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'float', Line, Val}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'float', Line, Val}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'bin', Line, Val}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'bin', Line, Val}, State2); %% TODO - recurse on value?
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'nil', Line}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'nil', Line}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'cons', Line, Head, Tail}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Head2, State3} = modify(Head, PreFun, PostFun, State2),
      {Tail2, State4} = modify(Tail, PreFun, PostFun, State3),
      PostFun({'cons', Line, Head2, Tail2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'remote', Line, Module, Name}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Module2, State3} = modify(Module, PreFun, PostFun, State2),
      {Name2, State4} = modify(Name, PreFun, PostFun, State3),
      PostFun({'remote', Line, Module2, Name2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'fun', Line, {clauses, Clauses}}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Clauses2, State3} = modify(Clauses, PreFun, PostFun, State2),
      PostFun({'fun', Line, {clauses, Clauses2}}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'fun', Line, {function, Module, Name, Arity}}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'fun', Line, {function, Module, Name, Arity}}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'fun', Line, {function, Name, Arity}}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'fun', Line, {function, Name, Arity}}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'named_fun', Line, Name, Clauses}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Clauses2, State3} = modify(Clauses, PreFun, PostFun, State2),
      PostFun({'named_fun', Line, Name, Clauses2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'tuple', Line, Statements}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Statements2, State3} = modify(Statements, PreFun, PostFun, State2),
      PostFun({'tuple', Line, Statements2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'map', Line, MapStatements}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {MapStatements2, State3} = modify(MapStatements, PreFun, PostFun, State2),
      PostFun({'map', Line, MapStatements2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'map', Line, Var, MapStatements}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Var2, State3} = modify(Var, PreFun, PostFun, State2),
      {MapStatements2, State4} = modify(MapStatements, PreFun, PostFun, State3),
      PostFun({'map', Line, Var2, MapStatements2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'lc', Line, Item, Generators}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Item2, State3} = modify(Item, PreFun, PostFun, State2),
      {Generators2, State4} = modify(Generators, PreFun, PostFun, State3),
      PostFun({'lc', Line, Item2, Generators2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'generate', Line, Var, Source}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Var2, State3} = modify(Var, PreFun, PostFun, State2),
      {Source2, State4} = modify(Source, PreFun, PostFun, State3),
      PostFun({'generate', Line, Var2, Source2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

%% todo
modify(Form = {'try', Line, Call, Something, Clauses, SomethingElse}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Call2, State3} = modify(Call, PreFun, PostFun, State2),
      {Something2, State4} = modify(Something, PreFun, PostFun, State3),
      {Clauses2, State5} = modify(Clauses, PreFun, PostFun, State4),
      {SomethingElse2, State6} = modify(SomethingElse, PreFun, PostFun, State5),
      PostFun({'try', Line, Call2, Something2, Clauses2, SomethingElse2}, State6);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'receive', Line, Clauses}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Clauses2, State3} = modify(Clauses, PreFun, PostFun, State2),
      PostFun({'receive', Line, Clauses2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'receive', Line, Clauses, Delay, After}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Clauses2, State3} = modify(Clauses, PreFun, PostFun, State2),
      {Delay2, State4} = modify(Delay, PreFun, PostFun, State3),
      {After2, State5} = modify(After, PreFun, PostFun, State4),
      PostFun({'receive', Line, Clauses2, Delay2, After2}, State5);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'record', Line, Name, Fields}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Fields2, State3} = modify(Fields, PreFun, PostFun, State2),
      PostFun({'record', Line, Name, Fields2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'record', Line, Variable, Name, Fields}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Variable2, State3} = modify(Variable, PreFun, PostFun, State2),
      {Fields2, State4} = modify(Fields, PreFun, PostFun, State3),
      PostFun({'record', Line, Variable2, Name, Fields2}, State4);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'record_field', Line, Name, Value}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Value2, State3} = modify(Value, PreFun, PostFun, State2),
      PostFun({'record_field', Line, Name, Value2}, State3);
    {replace, Val, State2} ->
      {Val, State2}
  end;

modify(Form = {'record_field', Line, Variable, Name, Value}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Variable2, State3} = modify(Variable, PreFun, PostFun, State2),
      {Value2, State4} = modify(Value, PreFun, PostFun, State3),
      PostFun({'record_field', Line, Variable2, Name, Value2}, State4);
    {replace, Val, State2} ->
      {Val, State2}
  end;

modify(Form = {'b_generate', Line, Bin, Source}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Source2, State3} = modify(Source, PreFun, PostFun, State2),
      PostFun({'b_generate', Line, Bin, Source2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'if', Line, Clauses}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Clauses2, State3} = modify(Clauses, PreFun, PostFun, State2),
      PostFun({'if', Line, Clauses2}, State3);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'string', Line, String}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'string', Line, String}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'char', Line, Char}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'char', Line, Char}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'atom', Line, Atom}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      PostFun({'atom', Line, Atom}, State2);
    {replace, Value, State2} ->
      {Value, State2}
  end;

modify(Form = {'map_field_exact', Line, Key, Value}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Key2, State3} = modify(Key, PreFun, PostFun, State2),
      {Value2, State4} = modify(Value, PreFun, PostFun, State3),
      PostFun({'map_field_exact', Line, Key2, Value2}, State4);
    {replace, Value2, State2} ->
      {Value2, State2}
  end;

modify(Form = {'map_field_assoc', Line, Key, Value}, PreFun, PostFun, State) ->
  case PreFun(Form, State) of
    {undefined, State2} ->
      {Key2, State3} = modify(Key, PreFun, PostFun, State2),
      {Value2, State4} = modify(Value, PreFun, PostFun, State3),
      PostFun({'map_field_assoc', Line, Key2, Value2}, State4);
    {replace, Value2, State2} ->
      {Value2, State2}
  end.


%% %% Current purerl output
%% monadTransStateT() -> ((control_monad_trans_class@ps:'MonadTrans'())(fun (DictMonad) ->
%%   fun (M) ->
%%     fun (S) ->
%%       (((control_bind@ps:bind())((maps:get('Bind1', DictMonad))(undefined)))(M))(fun (X) ->
%%         (((control_applicative@ps:pure())(((maps:get('Applicative0', DictMonad))(undefined))))({ tuple, X, S }))
%%       end)
%%     end
%%   end
%% end)).

%% %% Proposed changed
%% monadTransStateT() -> ((control_monad_trans_class@ps:'MonadTrans'())(fun (DictMonad) ->
%%   Bind1 = (control_bind@ps:bind())((maps:get('Bind1', DictMonad))(undefined)),
%%   Applicative0 = ((control_applicative@ps:pure()))((maps:get('Applicative0', DictMonad))(undefined)),
%%   fun (M) ->
%%     fun (S) ->
%%       ((Bind1)(M))(fun (X) ->
%%         (Applicative0)({ tuple, X, S })
%%       end)
%%     end
%%   end
%% end)).

%% Look for zero-arity function with single clause of 1-arity call with single clause of 1-arity call etc - number of 1-arity calls
%% is the overall arity of the final step that has an 'actual' body.
%% Then within the body, search for terms that only reference the variable form the 1st 1-arity call -
%% including looking for calls of form: fun(X, Y, Z) and turning them into memoised fun()(X) and then evaluated ((memorised)(Y))(Z)
