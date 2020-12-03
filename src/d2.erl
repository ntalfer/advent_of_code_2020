-module(d2).

-include_lib("eunit/include/eunit.hrl").

p1(List) ->
    do_p1(List, 0).

do_p1([], Acc) ->
    Acc;
do_p1([[Min, Max, Letter, Password] | Rest], Acc) ->
    case is_valid_password_p1(Min, Max, Letter, Password) of
        true ->
            do_p1(Rest, Acc + 1);
        false ->
            do_p1(Rest, Acc)
    end.

is_valid_password_p1(Min, Max, Letter, Password) ->
    [Code] = Letter,
    Count =
        lists:foldl(fun(C, Acc) ->
                       case C of
                           Code -> Acc + 1;
                           _ -> Acc
                       end
                    end,
                    0,
                    Password),
    Count >= Min andalso Count =< Max.

input() ->
    {ok, Bin} = file:read_file("src/d2_input.txt"),
    Str = binary_to_list(Bin),
    List = string:tokens(Str, "\n"),
    lists:map(fun(Line) ->
                 [N1, N2, Letter, Password] = string:tokens(Line, "- :"),
                 [list_to_integer(N1), list_to_integer(N2), Letter, Password]
              end,
              List).

p1_test() ->
    List = input(),
    ?assertEqual(477, p1(List)).

p2(List) ->
    do_p2(List, 0).

do_p2([], Acc) ->
    Acc;
do_p2([[Pos1, Pos2, Letter, Password] | Rest], Acc) ->
    case is_valid_password_p2(Pos1, Pos2, Letter, Password) of
        true ->
            do_p2(Rest, Acc + 1);
        false ->
            do_p2(Rest, Acc)
    end.

is_valid_password_p2(Pos1, Pos2, Letter, Password) ->
    [Code] = Letter,
    Code1 = (catch lists:nth(Pos1, Password)),
    Code2 = (catch lists:nth(Pos2, Password)),
    lists:member(Code, [Code1, Code2]) andalso (Code1 =/= Code2).

p2_test() ->
    List = input(),
    ?assertEqual(686, p2(List)).