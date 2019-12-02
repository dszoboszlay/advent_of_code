-module(a01).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(input()).

e1(Masses) ->
  lists:sum([mass2fuel(Mass) || Mass <- Masses]).

e2() ->
  e2(input()).

e2(Masses) ->
  lists:sum([fuel4fuel(mass2fuel(Mass)) || Mass <- Masses]).

input() ->
  ModFile = code:which(?MODULE),
  ProjDir = filename:dirname(filename:dirname(ModFile)),
  InputFile = filename:join([ProjDir, "input", ?MODULE_STRING ".term"]),
  {ok, Input} = file:consult(InputFile),
  Input.

mass2fuel(Mass) ->
  (Mass div 3) - 2.

fuel4fuel(Fuel) ->
  fuel4fuel(Fuel, 0).

fuel4fuel(Fuel, Acc) when Fuel =< 0 ->
  Acc;
fuel4fuel(Fuel, Acc) ->
  fuel4fuel(mass2fuel(Fuel), Acc + Fuel).

mass2fuel_test_() ->
  [ ?_assertEqual(    2, mass2fuel(   12))
  , ?_assertEqual(    2, mass2fuel(    14))
  , ?_assertEqual(  654, mass2fuel(  1969))
  , ?_assertEqual(33583, mass2fuel(100756))
  ].

fuel4fuel_test_() ->
  [ ?_assertEqual(    2, fuel4fuel(mass2fuel(    14)))
  , ?_assertEqual(  966, fuel4fuel(mass2fuel(  1969)))
  , ?_assertEqual(50346, fuel4fuel(mass2fuel(100756)))
  ].
