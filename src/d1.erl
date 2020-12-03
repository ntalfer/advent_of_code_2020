-module(d1).

 -include_lib("eunit/include/eunit.hrl").

p1(List) ->
    {N1, N2} = do_p1(List, 2020),
    N1 * N2.

do_p1([], _) ->
    undefined;
do_p1([N1|Rest], Sum) ->
    C = Sum - N1,
    case lists:member(C, Rest) of
        true ->
            {N1, C};
        false ->
            do_p1(Rest, Sum)
    end.

input() ->
    {ok, Bin} = file:read_file("src/d1_input.txt"),
    Str = binary_to_list(Bin),
    [list_to_integer(T) || T <- string:tokens(Str, "\n")].

p1_test() ->
    List = input(),
    ?assertEqual(787776, p1(List)).

p2(List) ->
    {N1, N2, N3} = do_p2(List, 2020),
    N1 * N2 * N3.

do_p2([N1|Rest], Sum) ->
    C = Sum - N1,
    case do_p1(Rest, C) of
        undefined ->
            do_p2(Rest, Sum);
        {N2, N3} ->
            {N1, N2, N3}
    end.

p2_test() ->
    List = input(),
    ?assertEqual(262738554, p2(List)).