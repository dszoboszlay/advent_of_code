%%% @copyright 2019 Dániel Szoboszlay

-record(vm, { memory
            , ip = 0
            , input = []
            , routput = []
            }).

-compile({nowarn_unused_function, [ memory2list/1
                                  , vm2list/1
                                  , read/2
                                  , write/3
                                  , set_input/2
                                  , get_output/1
                                  ]}).
-ifndef(MEM_IMPL).
-define(MEM_IMPL, atomics).
-endif.

%% ----------------------------------------------------------------------------
-if(?MEM_IMPL == map).
list2memory(List) ->
  maps:from_list(lists:zip(lists:seq(0, length(List) - 1), List)).

memory2list(Memory) ->
  element(2, lists:unzip(lists:sort(maps:to_list(Memory)))).

read_memory(Addr, Memory) ->
  maps:get(Addr, Memory).

write_memory(Addr, Val, Memory) ->
  maps:put(Addr, Val, Memory).

%% ----------------------------------------------------------------------------
-elif(?MEM_IMPL == atomics).
list2memory(List) ->
  Memory = atomics:new(length(List), [{signed, true}]),
  lists:foldl( fun (Val, Ix) -> atomics:put(Memory, Ix, Val), Ix + 1 end
             , 1
             , List),
  Memory.

memory2list(Memory) ->
  #{size := Size} = atomics:info(Memory),
  [atomics:get(Memory, Ix) || Ix <- lists:seq(1, Size)].

read_memory(Addr, Memory) ->
  atomics:get(Memory, Addr + 1).

write_memory(Addr, Val, Memory) ->
  atomics:put(Memory, Addr + 1, Val),
  Memory.

%% ----------------------------------------------------------------------------
-else.
-error("Invalid value for the MEM_IMPL macro").
-endif.

file2vm() ->
  ModFile = code:which(?MODULE),
  ProjDir = filename:dirname(filename:dirname(ModFile)),
  InputFile = filename:join([ProjDir, "input", ?MODULE_STRING ".term"]),
  file2vm(InputFile).

file2vm(File) ->
  {ok, [List]} = file:consult(File),
  list2vm(List).

list2vm(List) ->
  #vm{memory = list2memory(List)}.

vm2list(#vm{memory = Memory}) ->
  memory2list(Memory).

read(Addr, #vm{memory = Memory}) ->
  read_memory(Addr, Memory).

write(Addr, Val, VM = #vm{memory = Memory}) ->
  VM#vm{memory = write_memory(Addr, Val, Memory)}.

set_input(Input, VM) ->
  VM#vm{input = Input}.

get_output(#vm{routput = Output}) ->
  lists:reverse(Output).

run(#vm{ memory = Memory
       , ip = Ip
       , input = Input
       , routput = Output
       } = VM) ->
  Ops = #{ 1 => {add,    [r, r, w]}
         , 2 => {mul,    [r, r, w]}
         , 3 => {input,  [w, i]}
         , 4 => {output, [r]}
         , 5 => {jit,    [r, r]}
         , 6 => {jif,    [r, r]}
         , 7 => {lt,     [r, r, w]}
         , 8 => {eq,     [r, r, w]}
         },
  Op = read_memory(Ip, Memory),
  case maps:get(Op rem 100, Ops, undefined) of
    undefined ->
      {stopped, VM};
    {OpName, ArgSpecs} ->
      {Args, WriteAddr, NextIp} =
        fetch_args(ArgSpecs, Op div 100, Ip + 1, Memory, Input),
      Res = eval(OpName, Args),
      run(VM#vm{ memory =
                   if WriteAddr =:= undefined ->
                       Memory;
                      is_map(Res) ->
                       write_memory(WriteAddr, maps:get(value, Res), Memory);
                      true ->
                       write_memory(WriteAddr, Res, Memory)
                   end
               , ip = if is_map(Res) -> maps:get(ip, Res, NextIp);
                         true        -> NextIp
                      end
               , input = if is_map(Res) ->  maps:get(input, Res, Input);
                            true        -> Input
                         end
               , routput = case Res of
                             #{output := O} when is_list(O) ->
                               lists:reverse(O, Output);
                             #{output := O} ->
                               [O | Output];
                             _ ->
                               Output
                           end
               })
  end.

fetch_args(ArgSpecs, Modes, ArgAddr, Memory, Input) ->
  fetch_args(ArgSpecs, Modes, [], undefined, ArgAddr, Memory, Input).

fetch_args([], 0,  Args, WriteAddr, Ip, _Memory, _Input) ->
  {lists:reverse(Args), WriteAddr, Ip};
fetch_args([i], 0, Args, WriteAddr, Ip, _Memory, Input) ->
  {lists:reverse(Args, Input), WriteAddr, Ip};
fetch_args([ArgSpec | Rest], Modes, Args, WriteAddr, ArgAddr, Memory, Input) ->
  Mode = Modes rem 10,
  NextModes = Modes div 10,
  NextArgAddr = ArgAddr + 1,
  Val = read_memory(ArgAddr, Memory),
  if ArgSpec =:= w, Mode =:= 0, WriteAddr =:= undefined ->
      fetch_args(Rest, NextModes, Args, Val, NextArgAddr, Memory, Input);
     ArgSpec =:= r, Mode =:= 0 ->
      NextArgs = [read_memory(Val, Memory) | Args],
      fetch_args(Rest, NextModes, NextArgs, WriteAddr, NextArgAddr, Memory,
                 Input);
     ArgSpec =:= r, Mode =:= 1 ->
      NextArgs = [Val | Args],
      fetch_args(Rest, NextModes, NextArgs, WriteAddr, NextArgAddr, Memory,
                 Input)
  end.

eval(add, [A, B]) ->
  A + B;
eval(mul, [A, B]) ->
  A * B;
eval(input, [V | Input]) ->
  #{value => V, input => Input};
eval(output, [A]) ->
  #{output => A};
eval(jit, [C, Ip]) ->
  case C of
    0 -> undefined;
    _ -> #{ip => Ip}
  end;
eval(jif, [C, Ip]) ->
  case C of
    0 -> #{ip => Ip};
    _ -> undefined
  end;
eval(lt, [A, B]) ->
  case A < B of
    true  -> 1;
    false -> 0
  end;
eval(eq, [A, B]) ->
  case A =:= B of
    true  -> 1;
    false -> 0
  end.
