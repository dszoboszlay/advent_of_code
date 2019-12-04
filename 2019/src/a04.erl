%%% @copyright 2019 Dániel Szoboszlay
-module(a04).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  length([N
          || N <- lists:seq(178416, 676461),
             S <- [integer_to_list(N)],
             S =:= lists:sort(S),
             S =/= lists:usort(S)
         ]).

e2() ->
  length([N
          || N <- lists:seq(178416, 676461),
             S <- [integer_to_list(N)],
             verify(S, -1, 1, false)
         ]).

verify([], _, Repeat, HasTwo) ->
  HasTwo orelse Repeat =:= 2;
verify([N | Rest], N, Repeat, HasTwo) ->
  verify(Rest, N, Repeat + 1, HasTwo);
verify([N | Rest], M, Repeat, HasTwo) ->
  N > M andalso verify(Rest, N, 1, HasTwo orelse Repeat =:= 2).
