-module(d10).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    [list_to_integer(S) || S <- string:tokens(Str, "\n")].

p1(File) ->
    Adapters = input(File),
    do_p1(Adapters).

do_p1(Adapters) ->
    do_p1([0] ++ lists:sort(Adapters) ++ [lists:max(Adapters) + 3], {0, 0, 0}).

do_p1([], {Diff1, _Diff2, Diff3}) ->
    Diff1 * Diff3;
do_p1([_], {Diff1, _Diff2, Diff3}) ->
    Diff1 * Diff3;
do_p1([N1, N2 | Rest], {Diff1, Diff2, Diff3}) ->
    case N2 - N1 of
        3 ->
            do_p1([N2 | Rest], {Diff1, Diff2, Diff3 + 1});
        2 ->
            do_p1([N2 | Rest], {Diff1, Diff2 + 1, Diff3});
        1 ->
            do_p1([N2 | Rest], {Diff1 + 1, Diff2, Diff3})
    end.

p1_test() ->
    Adapters = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4],
    ?assertEqual(7 * 5, do_p1(Adapters)),
    ?assertEqual(2482, p1("src/d10_input.txt")).

p2(File) ->
    Adapters = input(File),
    do_p2(Adapters).

do_p2(Adapters) ->
    Adapters2 = lists:sort(Adapters ++ [lists:max(Adapters) + 3]),
    do_p2(Adapters2, #{0 => 1}).

do_p2([], Map) ->
    Keys = maps:keys(Map),
    Max = lists:max(Keys),
    maps:get(Max, Map);
do_p2([Adapter|Adapters], Map1) ->
    Map2 = maps:put(Adapter, 0, Map1),
    Map3 =
    case maps:is_key(Adapter-1, Map2) of
        true ->
            NewVal3 = maps:get(Adapter-1, Map2) + maps:get(Adapter, Map2),
            maps:put(Adapter, NewVal3, Map2);
        false ->
            Map2
    end,
    Map4 =
    case maps:is_key(Adapter-2, Map3) of
        true ->
            NewVal4 = maps:get(Adapter-2, Map3) + maps:get(Adapter, Map3),
            maps:put(Adapter, NewVal4, Map3);
        false ->
            Map3
    end,
    Map5 =
    case maps:is_key(Adapter-3, Map4) of
        true ->
            NewVal5 = maps:get(Adapter-3, Map4) + maps:get(Adapter, Map4),
            maps:put(Adapter, NewVal5, Map4);
        false ->
            Map4
    end,
    do_p2(Adapters, Map5).

p2_test() ->
    Adapters = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4],
    ?assertEqual(8, do_p2(Adapters)),
    ?assertEqual(96717311574016, p2("src/d10_input.txt")).