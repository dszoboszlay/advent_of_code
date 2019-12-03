%%% @copyright 2019 Dániel Szoboszlay
-module(a03).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(input()).

e1({L1, L2}) ->
  line_min_cross_dist(line_with_dist(L1), line_with_dist(L2)).

e2() ->
  e2(input()).

e2({L1, L2}) ->
  line_min_cross_delay(line_with_delay(L1), line_with_delay(L2)).

input() ->
  ModFile = code:which(?MODULE),
  ProjDir = filename:dirname(filename:dirname(ModFile)),
  InputFile = filename:join([ProjDir, "input", ?MODULE_STRING ".term"]),
  {ok, [L1, L2]} = file:consult(InputFile),
  {L1, L2}.

%% e1 --------------------------------------------------------------------------

line_with_dist(L) ->
  lists:usort(line_with_dist(L, 0, 0)).

%% y
%% ^
%% |
%% |
%% +----> x
line_with_dist([], _, _) ->
  [];
line_with_dist([{u, N} | L], X, Y) ->
  Y0 = Y + 1,
  Y1 = Y + N,
  [{dist(X, Y0, Y1), {v, X, Y0, Y1}} | line_with_dist(L, X, Y1)];
line_with_dist([{d, N} | L], X, Y) ->
  Y0 = Y - N,
  Y1 = Y - 1,
  [{dist(X, Y0, Y1), {v, X, Y0, Y1}} | line_with_dist(L, X, Y0)];
line_with_dist([{r, N} | L], X, Y) ->
  X0 = X + 1,
  X1 = X + N,
  [{dist(Y, X0, X1), {h, Y, X0, X1}} | line_with_dist(L, X1, Y)];
line_with_dist([{l, N} | L], X, Y) ->
  X0 = X - N,
  X1 = X - 1,
  [{dist(Y, X0, X1), {h, Y, X0, X1}} | line_with_dist(L, X0, Y)].

dist(A, B1, B2) ->
  if B1 > B2 -> infinity;
     B2 =< 0 -> abs(A) - B2;
     B1 >= 0 -> abs(A) + B1;
     true    -> abs(A)
  end.

line_min_cross_dist(L1, L2) ->
  line_min_cross_dist(infinity, L1, L2).

line_min_cross_dist(MinD, [{D, S} | L1], L2) when D < MinD ->
  line_min_cross_dist(section_min_cross_dist(MinD, S, L2), L1, L2);
line_min_cross_dist(MinD, _, _) ->
  MinD.

section_min_cross_dist(MinD, S1, [{D, S2} | L2]) when D < MinD ->
  section_min_cross_dist(min(MinD, cross_dist(S1, S2)), S1, L2);
section_min_cross_dist(MinD, _, _) ->
  MinD.

cross_dist({Dir1, A, B1, B2}, {Dir2, B, A1, A2}) when Dir1 =/= Dir2,
                                                      A1 =< A, A =< A2,
                                                      B1 =< B, B =< B2 ->
  abs(A) + abs(B);
cross_dist({Dir, A, B1, B2}, {Dir, A, B3, B4}) ->
  dist(A, max(B1, B3), min(B2, B4));
cross_dist(_, _) ->
  infinity.

%% e2 --------------------------------------------------------------------------

line_with_delay(L) ->
  line_with_delay(L, 0, 0, 0).

line_with_delay([], _, _, _) ->
  [];
line_with_delay([{u, N} | L], X, Y, D) ->
  D0 = D + 1,
  D1 = D + N,
  Y0 = Y + 1,
  Y1 = Y + N,
  [{D0, {v, D0, X, Y0, Y1}} | line_with_delay(L, X, Y1, D1)];
line_with_delay([{d, N} | L], X, Y, D) ->
  D0 = D + 1,
  D1 = D + N,
  Y0 = Y - N,
  Y1 = Y - 1,
  [{D0, {v, -D1, X, Y0, Y1}} | line_with_delay(L, X, Y0, D1)];
line_with_delay([{r, N} | L], X, Y, D) ->
  D0 = D + 1,
  D1 = D + N,
  X0 = X + 1,
  X1 = X + N,
  [{D0, {h, D0, Y, X0, X1}} | line_with_delay(L, X1, Y, D1)];
line_with_delay([{l, N} | L], X, Y, D) ->
  D0 = D + 1,
  D1 = D + N,
  X0 = X - N,
  X1 = X - 1,
  [{D0, {h, -D1, Y, X0, X1}} | line_with_delay(L, X0, Y, D1)].


line_min_cross_delay(L1, L2) ->
  line_min_cross_delay(infinity, L1, L2).

line_min_cross_delay( MinD
                    , [{D1, S} | L1]
                    , L2 = [{D2, _} | _]
                    ) when D1 + D2 < MinD ->
  line_min_cross_delay(section_min_cross_delay(MinD, D1, S, L2), L1, L2);
line_min_cross_delay(MinD, _, _) ->
  MinD.

section_min_cross_delay(MinD, D1, S1, [{D2, S2} | L2]) when D1 + D2 < MinD ->
  section_min_cross_delay(min(MinD, cross_delay(S1, S2)), D1, S1, L2);
section_min_cross_delay(MinD, _, _, _) ->
  MinD.

cross_delay( {Dir1, Delay1, A, B1, B2}
           , {Dir2, Delay2, B, A1, A2}
           ) when Dir1 =/= Dir2,
                  A1 =< A, A =< A2,
                  B1 =< B, B =< B2 ->
  delay(Delay1, B1, B) + delay(Delay2, A1, A);
cross_delay({Dir, Delay1, A, B1, B2}, {Dir, Delay2, A, B3, B4}) ->
  C1 = max(B1, B3),
  C2 = min(B2, B4),
  if C1 > C2 ->
      infinity;
     true ->
      delay(Delay1, B1, C1) + delay(Delay2, B3, C1)
  end;
cross_delay(_, _) ->
  infinity.

delay(D, A, B) when D > 0 ->
  B - A + D;
delay(D, A, B) when D < 0 ->
  A - B - D.
