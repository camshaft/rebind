-module(rebind).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
  {Forms2, _} = parse_trans:transform(fun do_transform/4, #{}, Forms, Options),
  parse_trans:revert(Forms2).

do_transform(application,{call,_,{atom,_,rebind},[{var,Line,Name}]}, _Context, Acc) ->
  Count = maps:get(Name, Acc, 0) + 1,
  {rebind(Name, Line, Count), false, maps:put(Name, Count, Acc)};
do_transform(variable,{var,Line,Name}, _Context, Acc) ->
  {rebind(Name, Line, Acc), false, Acc};
do_transform(match_expr, {match,Line,LH,RH}, Context, Acc) ->
  %% reverse the recurse order
  {RH2, Acc2} = traverse_match(RH,Acc,Context),
  {LH2, Acc3} = traverse_match(LH,Acc2,Context),
  {{match,Line,LH2,RH2}, false, Acc3};
do_transform(_Type, Form, _Context, Acc) ->
  {Form, true, Acc}.

rebind(Name, Line, Acc) when is_map(Acc) ->
  case maps:find(Name, Acc) of
    error ->
      {var, Line, Name};
    {ok, Count} ->
      rebind(Name, Line, Count)
  end;
rebind(Name, Line, Count) when is_integer(Count) ->
  {var, Line, list_to_atom("_" ++ atom_to_list(Name) ++ "__REBIND__" ++ integer_to_list(Count))}.

traverse_match(Side, Acc, Context) ->
  {[Side2], Acc2} = parse_trans:do_transform(fun do_transform/4, Acc, [Side], Context),
  [Out] = parse_trans:revert([Side2]),
  {Out, Acc2}.
