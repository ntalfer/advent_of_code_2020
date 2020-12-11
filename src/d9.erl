-module(d9).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    [list_to_integer(N) || N <- string:tokens(Str, "\n")].

p1(File, Preamble) ->
    List = input(File),
    do_p1(List, Preamble).

do_p1(List, Preamble) ->
    {Head, Tail} = lists:split(Preamble, List),
    N = hd(Tail),
    case is_not_number_of_2(Head, N) of
        true ->
            N;
        false ->
            NewList = tl(Head) ++ Tail,
            do_p1(NewList, Preamble)
    end.

is_not_number_of_2([], _) ->
    true;
is_not_number_of_2([A | Rest], N) ->
    case lists:member(N-A, Rest) of
        true ->
            false;
        false ->
            is_not_number_of_2(Rest, N)
    end.

p1_test() ->
    ?assertEqual(127, p1("src/d9_input_example.txt", 5)),
    ?assertEqual(20874512, p1("src/d9_input.txt", 25)).

p2(File, Preamble) ->
    List = input(File),
    N = do_p1(List, Preamble),
    NewList = input(File) -- [N],
    do_p2(NewList, NewList, N, []).

do_p2(OriginalList, [H|T], N, Set) ->
    NewSet = Set ++ [H],
    Sum = lists:sum(NewSet),
    case Sum of
        _ when Sum > N ->
            %% that's not the right set, shift
            do_p2(tl(OriginalList), tl(OriginalList), N, []);
        N ->
            %% found it!
            lists:min(NewSet) + lists:max(NewSet);
        _ ->
            %% continue
            do_p2(OriginalList, T, N, NewSet)
    end.

p2_test() ->
    ?assertEqual(62, p2("src/d9_input_example.txt", 5)),
    ?assertEqual(3012420, p2("src/d9_input.txt", 25)).