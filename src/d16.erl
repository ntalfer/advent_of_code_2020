-module(d16).

-include_lib("eunit/include/eunit.hrl").

input() ->
    {ok, Bin} = file:read_file("src/d16_input.txt"),
    Str = binary_to_list(Bin),
    string:tokens(Str, "\n").

p1() ->
    do_p1(input()).

do_p1(Lines) ->
    {Fields, _Ticket, Nearby} = parse_lines(Lines),
    Ranges =
        lists:flatten(
            maps:values(Fields)),
    do_p1(Ranges, Nearby, 0).

do_p1(_, [], Rate) ->
    Rate;
do_p1(Ranges, [NT | Nearby], Rate) ->
    NTRate =
        lists:foldl(fun(N, Acc) ->
                       case lists:any(fun({S, E}) -> (N >= S) and (N =< E) end, Ranges) of
                           true -> Acc;
                           false -> Acc + N
                       end
                    end,
                    0,
                    NT),
    do_p1(Ranges, Nearby, Rate + NTRate).

parse_lines(Lines) ->
    parse_lines(Lines, fields, #{}, [], []).

parse_lines([], _, Fields, Ticket, Nearby) ->
    {Fields, Ticket, Nearby};
parse_lines(["" | Lines], Step, Fields, Ticket, Nearby) ->
    parse_lines(Lines, Step, Fields, Ticket, Nearby);
parse_lines(["your ticket:" | Lines], _, Fields, Ticket, Nearby) ->
    parse_lines(Lines, ticket, Fields, Ticket, Nearby);
parse_lines(["nearby tickets:" | Lines], _, Fields, Ticket, Nearby) ->
    parse_lines(Lines, nearby, Fields, Ticket, Nearby);
parse_lines([Line | Lines], fields, Fields, Ticket, Nearby) ->
    [Name | Rest] = string:tokens(Line, ":"),
    Ranges1 =
        string:tokens(
            lists:flatten(Rest), " or"),
    Ranges2 =
        lists:map(fun(R) ->
                     [S, E] = string:tokens(R, "-"),
                     {list_to_integer(S), list_to_integer(E)}
                  end,
                  Ranges1),
    Fields2 = maps:put(Name, Ranges2, Fields),
    parse_lines(Lines, fields, Fields2, Ticket, Nearby);
parse_lines([Line | Lines], ticket, Fields, _, Nearby) ->
    Ticket = [list_to_integer(C) || C <- string:tokens(Line, ",")],
    parse_lines(Lines, ticket, Fields, Ticket, Nearby);
parse_lines([Line | Lines], nearby, Fields, Ticket, Nearby) ->
    NT = [list_to_integer(C) || C <- string:tokens(Line, ",")],
    parse_lines(Lines, nearby, Fields, Ticket, [NT | Nearby]).

p2() ->
    do_p2(input()).

do_p2(Lines) ->
    {FieldsRanges, Ticket, Nearby} = parse_lines(Lines),
    %io:fwrite("FieldsRanges = ~p~n", [FieldsRanges]),
    io:fwrite("Ticket = ~p~n", [Ticket]),
    Val = lists:nth(8, Ticket) *  lists:nth(12, Ticket) * lists:nth(3, Ticket) * lists:nth(15, Ticket) * lists:nth(17, Ticket) * lists:nth(5, Ticket),
    exit(Val),
    Ranges =
        lists:flatten(
            maps:values(FieldsRanges)),
    GoodNT = lists:filter(fun(NT) -> 0 == do_p1(Ranges, [NT], 0) end, Nearby),
    %io:fwrite("GoodNT = ~p~n", [GoodNT]),
    Fields = maps:keys(FieldsRanges),
    %io:fwrite("Fields = ~p~n", [Fields]),
    %Candidates = lists:foldl(fun(E, M) -> maps:put(E, Fields, M) end, #{}, lists:seq(1, length(Ticket))),
    %io:fwrite("Candidates = ~p~n", [Candidates]),
    FieldsCount = length(Fields),
    FieldPos = lists:seq(1, FieldsCount),
    FieldsPositions = lists:foldl(fun(F, Acc) -> maps:put(F, FieldPos, Acc) end, #{}, Fields),
    %FieldsPositions = lists:foldl(fun(F, Acc) -> maps:put(F, [], Acc) end, #{}, Fields),
    %io:fwrite("FieldsPositions = ~p~n", [FieldsPositions]),
    Res = traverse_tickets(FieldsRanges, FieldsPositions, GoodNT),
    io:fwrite("Res = ~p~n", [Res]),
    %throw(1).

    Reduced = reduce(Res, #{}),
    io:fwrite("Reduced = ~p~n", [Reduced]),
    Reduced.

traverse_tickets(_FieldsRanges, FieldsPositions, []) ->
    FieldsPositions;
traverse_tickets(FieldsRanges, FieldsPositions, [Ticket | Tickets]) ->
    FieldsPositions2 = traverse_ticket(FieldsRanges, FieldsPositions, Ticket),
    traverse_tickets(FieldsRanges, FieldsPositions2, Tickets).

traverse_ticket(FieldsRanges, FieldsPositions, Ticket) ->
    %% for each nb in the ticket
    {FieldsPositions2, _} =
        lists:foldl(fun(Number, {FieldsPositions_Acc1, Index}) ->
                    %    case Index of
                    %        16 ->
                    %            %io:fwrite("Index = ~p~n", [Index]),
                    %            io:fwrite("FieldsPositions_Acc1 = ~p~n", [FieldsPositions_Acc1]),
                    %            ok;
                    %        _ -> ok
                    %    end,
                       %% for each field-range
                       FieldsPositions_Acc1_updated =
                           maps:fold(fun(Field, Ranges, FieldsPositions_Acc2) ->
                                        % case Index of
                                        %     16 ->
                                        %         io:fwrite("FieldsPositions_Acc2 = ~p~n",
                                        %                   [FieldsPositions_Acc2]),
                                        %         io:fwrite("Field = ~p~n", [Field]),
                                        %         io:fwrite("Number = ~p~n", [Number]),
                                        %         io:fwrite("Index = ~p~n", [Index]),
                                        %         io:fwrite("Ranges = ~p~n", [Ranges]),
                                        %         ok;
                                        %     _ -> ok
                                        % end,
                                        %% for each range
                                        InRange =
                                            lists:any(fun({Min, Max}) ->
                                                         Number >= Min andalso Number =< Max
                                                      end,
                                                      Ranges),
                                        % case Index of
                                        %     16 ->
                                        %         io:fwrite("InRange = ~p~n", [InRange]),
                                        %         ok;
                                        %     _ -> ok
                                        % end,
                                        case InRange of
                                            true ->
                                                %% number is in one of the ranges, this is a possible position for this field
                                                FieldsPositions_Acc2;
                                            false ->
                                                %% number is not in any range, this is not a possible position for this field
                                                %% remove index for field in FieldsPositions
                                                FieldPositions =
                                                    maps:get(Field, FieldsPositions_Acc2),
                                                FieldPositions2 = FieldPositions -- [Index],
                                                maps:put(Field,
                                                         FieldPositions2,
                                                         FieldsPositions_Acc2)
                                        end
                                     end,
                                     FieldsPositions_Acc1,
                                     FieldsRanges),
                       case is_map(FieldsPositions_Acc1_updated) of
                           true -> ok;
                           false -> exit(foo)
                       end,
                       {FieldsPositions_Acc1_updated, Index + 1}
                    end,
                    {FieldsPositions, 1},
                    Ticket),
    FieldsPositions2.

reduce(Map, Acc_map) when Map == #{} ->
    Acc_map;
% reduce(Map, Acc_map) when size(Map) == 20 ->
%     io:fwrite("Map = ~p~n", [Map]),
%     io:fwrite("Acc_map = ~p~n", [Acc_map]),
%     ok;
reduce(Map, Acc_map) ->
    io:fwrite("Map = ~p~n", [Map]),
    io:fwrite("Acc_map = ~p~n", [Acc_map]),
    case maps:size(Map) of
    2 -> exit(foo);
_ -> ok
    end,
    case lists:all(fun({_Field, Positions}) -> length(Positions) == 1 end, maps:to_list(Map))
    of
        true ->
            io:fwrite("Here = ~p~n", [""]),
            Acc_map;
        false ->
            {Field, [Index]} =
                maps:fold(fun (F, [V], _Acc) ->
                                  {F, [V]};
                              (_, _, Acc) ->
                                  Acc
                          end,
                          {"-1", [-1]},
                          Map),
            Acc_map2 = maps:put(Field, [Index], Acc_map),
            Map2 =
                maps:fold(fun (F, _, Acc) when F == Field ->
                                  Acc;
                              (F, P, Acc) ->
                                  maps:put(F, P -- [Index], Acc)
                          end,
                          #{},
                          Map),
            io:fwrite("Map2 = ~p~n", [Map2]),
            io:fwrite("Acc_map2 = ~p~n", [Acc_map2]),
            reduce(Map2, Acc_map2)
    end.

p1_test() ->
    Example =
        ["class: 1-3 or 5-7",
         "row: 6-11 or 33-44",
         "seat: 13-40 or 45-50",
         "",
         "your ticket:",
         "7,1,14",
         "",
         "nearby tickets:",
         "7,3,47",
         "40,4,50",
         "55,2,20",
         "38,6,12"],
    ?assertEqual({#{"class" => [{1, 3}, {5, 7}],
                    "row" => [{6, 11}, {33, 44}],
                    "seat" => [{13, 40}, {45, 50}]},
                  [7, 1, 14],
                  [[38, 6, 12], [55, 2, 20], [40, 4, 50], [7, 3, 47]]},
                 parse_lines(Example)),
    ?assertEqual(71, do_p1(Example)),
    ?assertEqual(23115, p1()),
    ok.

p2_test() ->
    Example =
        ["class: 0-1 or 4-19",
         "row: 0-5 or 8-19",
         "seat: 0-13 or 16-19",
         "your ticket:",
         "11,12,13",
         "",
         "nearby tickets:",
         "3,9,18",
         "15,1,5",
         "5,14,9"],
    % Res = do_p2(Example),
    %io:fwrite("Res = ~p~n", [Res]),
    %exit(foo),
    p2(),
    ok.
