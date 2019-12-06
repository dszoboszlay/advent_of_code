%%% @copyright 2019 Dániel Szoboszlay
-module(a06).
-export([ e1/0
        , e2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

e1() ->
  e1(input()).

e1(Orbits) ->
  {Sum, _} = maps:fold( fun (Obj, _, {Sum, Map}) ->
                            {Orbs, NewMap} = orbits(Obj, Map),
                            {Sum + Orbs, NewMap}
                        end
                      , {0, Orbits}
                      , Orbits),
  Sum.

e2() ->
  e2(input()).

e2(Map) ->
  {N, NewMap} = orbits(xYOU, Map),
  dist(xSAN, NewMap, N - 1).

input() ->
  ModFile = code:which(?MODULE),
  ProjDir = filename:dirname(filename:dirname(ModFile)),
  InputFile = filename:join([ProjDir, "input", ?MODULE_STRING ".term"]),
  {ok, Lines} = file:consult(InputFile),
  maps:from_list([{xCOM, 0} | Lines]).

orbits(Obj, Map) ->
  case maps:get(Obj, Map) of
    Orbs when is_integer(Orbs) ->
      {Orbs, Map};
    OtherObj ->
      {OtherOrbs, NewMap} = orbits(OtherObj, Map),
      Orbs = OtherOrbs + 1,
      {Orbs, NewMap#{Obj => Orbs}}
  end.

dist(Obj, Map, Steps) ->
  case maps:get(Obj, Map) of
    Orbs when is_integer(Orbs) ->
      Steps - Orbs - 1;
    OtherObj ->
      dist(OtherObj, Map, Steps + 1)
  end.
