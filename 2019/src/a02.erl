%%% @copyright 2019 Dániel Szoboszlay
-module(a02).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(file2vm()).

e1(VM) ->
  run_with_input(VM, 12, 02).

e2() ->
  e2(file2vm()).

e2(VM) ->
  L = lists:seq(0, 99),
  try
    [throw({found, Noun, Verb})
     || Noun <- L,
        Verb <- L,
        run_with_input(VM, Noun, Verb) =:= 19690720
    ]
  catch
    {found, Noun, Verb} ->
      100 * Noun + Verb
  end.

-include("intcode.hrl").

run_with_input(VM0, Noun, Verb) ->
  VM1 = write(1, Noun, VM0),
  VM2 = write(2, Verb, VM1),
  {stopped, VM3} = run(VM2),
  read(0, VM3).
