-module(d15).

-include_lib("eunit/include/eunit.hrl").

p1(Input, MaxTurns) ->
    {Turn, LastSpoken, TurnsMemory} =
        lists:foldl(fun(N, {T, _, TM}) ->
                       TM2 = maps:put(N, [T], TM),
                       {T + 1, N, TM2}
                    end,
                    {1, 0, #{}},
                    Input),
    p1(Turn, MaxTurns, LastSpoken, TurnsMemory).

p1(Turn, MaxTurns, LastSpoken, _TurnsMemory) when Turn > MaxTurns ->
    %% stop the game
    LastSpoken;
p1(Turn, MaxTurns, LastSpoken, TurnsMemory) ->
    %io:fwrite("Turn #~p~n", [Turn]),
    case maps:get(LastSpoken, TurnsMemory) of
        [_] ->
            %% last spoken was the 1st time
            Speak = 0,
            TurnsMemory2 =
                case TurnsMemory of
                    #{Speak := [ST1]} ->
                        maps:put(Speak, [ST1, Turn], TurnsMemory);
                    #{Speak := [_ST1, ST2]} ->
                        maps:put(Speak, [ST2, Turn], TurnsMemory);
                    _ ->
                        maps:put(Speak, [Turn], TurnsMemory)
                end,
            p1(Turn + 1, MaxTurns, Speak, TurnsMemory2);
        [T1, T2] ->
            %% last spoken was not the 1st time
            Speak = T2 - T1,
            TurnsMemory2 =
                case TurnsMemory of
                    #{Speak := [ST1]} ->
                        maps:put(Speak, [ST1, Turn], TurnsMemory);
                    #{Speak := [_ST1, ST2]} ->
                        maps:put(Speak, [ST2, Turn], TurnsMemory);
                    _ ->
                        maps:put(Speak, [Turn], TurnsMemory)
                end,
            p1(Turn + 1, MaxTurns, Speak, TurnsMemory2)
    end.

p1_test() ->
    ?assertEqual(436, p1([0, 3, 6], 2020)),
    ?assertEqual(639, p1([11,18,0,20,1,7,16], 2020)).

p2_test_() ->
    {timeout, 60, [fun() -> ?assertEqual(266, p1([11,18,0,20,1,7,16], 30000000)) end]}.
