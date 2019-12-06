%%% @copyright 2019 Dániel Szoboszlay
-module(a05).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(file2vm()).

e1(VM) ->
  run_with_input(VM, [1]).

e2() ->
  e2(file2vm()).

e2(VM) ->
  run_with_input(VM, [5]).

-include("intcode.hrl").

run_with_input(VM0, Input) ->
  VM1 = set_input(Input, VM0),
  {stopped, VM2} = run(VM1),
  get_output(VM2).
