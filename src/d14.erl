-module(d14).

-include_lib("eunit/include/eunit.hrl").

p1() ->
    {ok, Bin} = file:read_file("src/d14_input.txt"),
    Str = binary_to_list(Bin),
    Lines = string:tokens(Str, "\n"),
    p1(Lines).

p1(Lines) ->
    do_p1(undefined, Lines, #{}).

do_p1(_, [], Map) ->
    lists:sum(
        maps:values(Map));
do_p1(Mask, [Line | Lines], Map) ->
    case parse(Line) of
        {mask, Mask2} ->
            do_p1(Mask2, Lines, Map);
        {I, V} ->
            Missing = length(Mask) - length(V),
            V2 = lists:duplicate(Missing, $0) ++ V,
            V3 = overwrite(Mask, V2, []),
            Map2 = maps:put(I, V3, Map),
            do_p1(Mask, Lines, Map2)
    end.

parse(Line) ->
    case re:split(Line, "^mask = ", [{return, list}]) of
        [[], Mask] ->
            {mask, Mask};
        _ ->
            [Index, Value] = string:tokens(Line, "mem[] ="),
            Value2 = io_lib:format("~.2B", [list_to_integer(Value)]),
            {Index, Value2}
    end.

overwrite([], [], Acc) ->
    list_to_integer(Acc, 2);
overwrite([$X | Mask], [Char | Chars], Acc) ->
    overwrite(Mask, Chars, Acc ++ [Char]);
overwrite([$0 | Mask], [_Char | Chars], Acc) ->
    overwrite(Mask, Chars, Acc ++ [$0]);
overwrite([$1 | Mask], [_Char | Chars], Acc) ->
    overwrite(Mask, Chars, Acc ++ [$1]).

p1_test() ->
    ?assertEqual(165,
                 p1(["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                     "mem[8] = 11",
                     "mem[7] = 101",
                     "mem[8] = 0"])),
    ?assertEqual(14553106347726, p1()),
    ok.

p2() ->
    {ok, Bin} = file:read_file("src/d14_input.txt"),
    Str = binary_to_list(Bin),
    Lines = string:tokens(Str, "\n"),
    p2(Lines).

p2(Lines) ->
    do_p2(undefined, Lines, #{}).

parse2(Line) ->
    case re:split(Line, "^mask = ", [{return, list}]) of
        [[], Mask] ->
            {mask, Mask};
        _ ->
            [Index, Value] = string:tokens(Line, "mem[] ="),
            Index2 = io_lib:format("~.2B", [list_to_integer(Index)]),
            {Index2, list_to_integer(Value)}
    end.

do_p2(_, [], Map) ->
    lists:sum(
        maps:values(Map));
do_p2(Mask, [Line | Lines], Map) ->
    case parse2(Line) of
        {mask, Mask2} ->
            do_p2(Mask2, Lines, Map);
        {I, V} ->
            Missing = length(Mask) - length(I),
            I2 = lists:duplicate(Missing, $0) ++ I,
            Indexes = overwrite2(Mask, I2, [""]),
            Map2 = lists:foldl(fun(Index, M) -> maps:put(Index, V, M) end, Map, Indexes),
            do_p2(Mask, Lines, Map2)
    end.

overwrite2([], [], Acc) ->
    Acc;
overwrite2([$X | Mask],
           [_Char | Chars],
           Acc1) -> % ["00", "01"] -> ["000", "000"]
    Acc2 = lists:foldl(fun(A, Acc) -> Acc ++ [A ++ "0"] ++ [A ++ "1"] end, [], Acc1),
    overwrite2(Mask, Chars, Acc2);
overwrite2([$0 | Mask], [Char | Chars], Acc1) ->
    Acc2 = lists:foldl(fun(A, Acc) -> Acc ++ [A ++ [Char]] end, [], Acc1),
    overwrite2(Mask, Chars, Acc2);
overwrite2([$1 | Mask], [_Char | Chars], Acc1) ->
    Acc2 = lists:foldl(fun(A, Acc) -> Acc ++ [A ++ "1"] end, [], Acc1),
    overwrite2(Mask, Chars, Acc2).

p2_test() ->
    ?assertEqual(208,
                 p2(["mask = 000000000000000000000000000000X1001X",
                     "mem[42] = 100",
                     "mask = 00000000000000000000000000000000X0XX",
                     "mem[26] = 1"])),
    ?assertEqual(2737766154126, p2()),
    ok.
