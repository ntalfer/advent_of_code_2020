-module(d6).

-include_lib("eunit/include/eunit.hrl").

p1(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    Groups = re:split(Str, "\n\n", [{return, list}]),
    GroupsAnswers = [re:replace(G, "\n", "", [global, {return, list}]) || G <- Groups],
    lists:foldl(fun(GroupAnswers, Acc) ->
                   Acc +
                       sets:size(
                           sets:from_list(GroupAnswers))
                end,
                0,
                GroupsAnswers).

p1_test() ->
    ?assertEqual(6799, p1("src/d6_input.txt")).

p2(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    Groups = re:split(Str, "\n\n", [{return, list}]),
    lists:foldl(fun(Group, Acc) ->
                   Lines = string:tokens(Group, "\n"),
                   GroupSize = length(Lines),
                   GroupAnswers = string:join(Lines, ""),
                   AC = answers_count(GroupAnswers, []),
                   Count = lists:foldl(fun({_, C}, Acc2) when C == GroupSize -> Acc2+1;
                                          (_, Acc2) -> Acc2
                                          end,
                                          0,
                                          AC),
                   Acc + Count
                end,
                0,
                Groups).

answers_count([], Acc) ->
    Acc;
answers_count([Answer | Answers], Acc) ->
    V = proplists:get_value(Answer, Acc, 0),
    NewAcc = lists:keystore(Answer, 1, Acc, {Answer, V+1}),
    answers_count(Answers, NewAcc).

p2_test() ->
    ?assertEqual(3354, p2("src/d6_input.txt")).