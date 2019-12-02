%%% @copyright 2019 Dániel Szoboszlay
-module(a02).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(input()).

e1(Memory) ->
  run_with_input(Memory, 12, 02).

e2() ->
  e2(input()).

e2(Memory) ->
  L = lists:seq(0, 99),
  try
    [throw({found, Noun, Verb})
     || Noun <- L,
        Verb <- L,
        run_with_input(Memory, Noun, Verb) =:= 19690720
    ]
  catch
    {found, Noun, Verb} ->
      100 * Noun + Verb
  end.

input() ->
  ModFile = code:which(?MODULE),
  ProjDir = filename:dirname(filename:dirname(ModFile)),
  InputFile = filename:join([ProjDir, "input", ?MODULE_STRING ".term"]),
  {ok, [List]} = file:consult(InputFile),
  list2memory(List).

list2memory(List) ->
  maps:from_list(lists:zip(lists:seq(0, length(List) - 1), List)).

memory2list(Memory) ->
  element(2, lists:unzip(lists:sort(maps:to_list(Memory)))).

run_with_input(Memory0, Noun, Verb) ->
  Memory1 = maps:put(1, Noun, Memory0),
  Memory2 = maps:put(2, Verb, Memory1),
  maps:get(0, run(Memory2)).

run(List) when is_list(List) ->
  memory2list(run(list2memory(List)));
run(Memory) when is_map(Memory) ->
  run(Memory, 0).

run(Memory, Ip) ->
  case maps:get(Ip, Memory) of
    1 ->
      run(fun erlang:'+'/2, Memory, Ip);
    2 ->
      run(fun erlang:'*'/2, Memory, Ip);
    _Other ->
      Memory
  end.

run(Fun, Memory, Ip) ->
  Op1 = maps:get(maps:get(Ip + 1, Memory), Memory),
  Op2 = maps:get(maps:get(Ip + 2, Memory), Memory),
  run(maps:put(maps:get(Ip + 3, Memory), Fun(Op1, Op2), Memory), Ip + 4).

list2memory_test() ->
  ?assertEqual( #{ 0 => 1
                 , 1 => 0
                 , 2 => 0
                 , 3 => 0
                 , 4 => 99
                 }
              , list2memory([1, 0, 0, 0, 99])
              ).

run_test_() ->
  [ ?_assertEqual([2, 0, 0, 0, 99], run([1, 0, 0, 0, 99]))
  , ?_assertEqual([2, 3, 0, 6, 99], run([2, 3, 0, 3, 99]))
  , ?_assertEqual([2, 4, 4, 5, 99, 9801], run([2, 4, 4, 5, 99, 0]))
  , ?_assertEqual( [30, 1, 1, 4, 2, 5, 6, 0, 99]
                 , run([1, 1, 1, 4, 99, 5, 6, 0, 99])
                 )
  ].
