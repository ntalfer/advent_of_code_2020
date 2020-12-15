-module(d11).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    [S || S <- string:tokens(Str, "\n")].

p1(File) ->
    Lines = input(File),
    do_p1(Lines).

do_p1(Lines) ->
    Width = length(hd(Lines)),
    Height = length(Lines),
    do_p1(Lines, Width, Height).

do_p1(Lines, Width, Height) ->
    NewLines = do_p1(1, 1, Lines, Width, Height, Lines),
    case lists:flatten(Lines) == lists:flatten(NewLines) of
        true ->
            Occupied = lists:filter(fun(C) -> C == $# end, lists:flatten(NewLines)),
            length(Occupied);
        false ->
            do_p1(NewLines, Width, Height)
    end.

do_p1(_X, Y, _Lines, _Width, Height, Acc) when Y > Height ->
    Acc;
do_p1(X, Y, Lines, Width, Height, Acc1) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    Adjacent =
        [_UL, _U, _UR, _L, _R, _BL, _B, _BR] =
            case {X, Y} of
                {1, 1} ->
                    %% left-up
                    NextLine = lists:nth(Y + 1, Lines),
                    [undefined,
                     undefined,
                     undefined,
                     undefined,
                     lists:nth(X + 1, Line),
                     undefined,
                     lists:nth(X, NextLine),
                     lists:nth(X + 1, NextLine)];
                {1, Height} ->
                    %% left-bottom
                    PrevLine = lists:nth(Y - 1, Lines),
                    [undefined,
                     lists:nth(X, PrevLine),
                     lists:nth(X + 1, PrevLine),
                     undefined,
                     lists:nth(X + 1, Line),
                     undefined,
                     undefined,
                     undefined];
                {1, _} ->
                    %% left-middle
                    PrevLine = lists:nth(Y - 1, Lines),
                    NextLine = lists:nth(Y + 1, Lines),
                    [undefined,
                     lists:nth(X, PrevLine),
                     lists:nth(X + 1, PrevLine),
                     undefined,
                     lists:nth(X + 1, Line),
                     undefined,
                     lists:nth(X, NextLine),
                     lists:nth(X + 1, NextLine)];
                {Width, 1} ->
                    %% right-up
                    NextLine = lists:nth(Y + 1, Lines),
                    [undefined,
                     undefined,
                     undefined,
                     lists:nth(X - 1, Line),
                     undefined,
                     lists:nth(X - 1, NextLine),
                     lists:nth(X, NextLine),
                     undefined];
                {X, 1} ->
                    %% middle-up
                    NextLine = lists:nth(Y + 1, Lines),
                    [undefined,
                     undefined,
                     undefined,
                     lists:nth(X - 1, Line),
                     lists:nth(X + 1, Line),
                     lists:nth(X - 1, NextLine),
                     lists:nth(X, NextLine),
                     lists:nth(X + 1, NextLine)];
                {Width, Height} ->
                    %% right-bottom
                    PrevLine = lists:nth(Y - 1, Lines),
                    [lists:nth(X - 1, PrevLine),
                     lists:nth(X, PrevLine),
                     undefined,
                     lists:nth(X - 1, Line),
                     undefined,
                     undefined,
                     undefined,
                     undefined];
                {Width, _} ->
                    %% right-middle
                    PrevLine = lists:nth(Y - 1, Lines),
                    NextLine = lists:nth(Y + 1, Lines),
                    [lists:nth(X - 1, PrevLine),
                     lists:nth(X, PrevLine),
                     undefined,
                     lists:nth(X - 1, Line),
                     undefined,
                     lists:nth(X - 1, NextLine),
                     lists:nth(X, NextLine),
                     undefined];
                {_, Height} ->
                    %% bottom-middle
                    PrevLine = lists:nth(Y - 1, Lines),
                    [lists:nth(X - 1, PrevLine),
                     lists:nth(X, PrevLine),
                     lists:nth(X + 1, PrevLine),
                     lists:nth(X - 1, Line),
                     lists:nth(X + 1, Line),
                     undefined,
                     undefined,
                     undefined];
                _ ->
                    NextLine = lists:nth(Y + 1, Lines),
                    PrevLine = lists:nth(Y - 1, Lines),
                    [lists:nth(X - 1, PrevLine),
                     lists:nth(X, PrevLine),
                     lists:nth(X + 1, PrevLine),
                     lists:nth(X - 1, Line),
                     lists:nth(X + 1, Line),
                     lists:nth(X - 1, NextLine),
                     lists:nth(X, NextLine),
                     lists:nth(X + 1, NextLine)]
            end,
    NewChar =
        case Char of
            $. ->
                $.;
            $L ->
                case lists:member($#, Adjacent) of
                    true ->
                        $L;
                    false ->
                        $#
                end;
            $# ->
                AdjOccupied = length(lists:filter(fun(C) -> C == $# end, Adjacent)),
                case AdjOccupied > 3 of
                    true ->
                        $L;
                    false ->
                        $#
                end
        end,
    CurrLine = lists:nth(Y, Acc1),
    NewLine =
        case X of
            1 ->
                [NewChar] ++ tl(CurrLine);
            Width ->
                lists:reverse(tl(lists:reverse(CurrLine))) ++ [NewChar];
            _ ->
                {L1, L2} = lists:split(X - 1, CurrLine),
                L1 ++ [NewChar] ++ tl(L2)
        end,
    Acc2 =
        case Y of
            1 ->
                [NewLine] ++ tl(Acc1);
            Height ->
                lists:reverse(tl(lists:reverse(Acc1))) ++ [NewLine];
            _ ->
                {L11, L22} = lists:split(Y - 1, Acc1),
                L11 ++ [NewLine] ++ tl(L22)
        end,
    {NewX, NewY} =
        case X of
            Width ->
                {1, Y + 1};
            _ ->
                {X + 1, Y}
        end,
    do_p1(NewX, NewY, Lines, Width, Height, Acc2).

% p1_test() ->
%     %?assertEqual(37, p1("src/d11_input_example.txt")),
%     ?assertEqual(2275, p1("src/d11_input.txt")).

p2(File) ->
    Lines = input(File),
    do_p2(Lines).

do_p2(Lines) ->
    Width = length(hd(Lines)),
    Height = length(Lines),
    do_p2(Lines, Width, Height).

do_p2(Lines, Width, Height) ->
    NewLines = do_p2(1, 1, Lines, Width, Height, Lines),
    case lists:flatten(Lines) == lists:flatten(NewLines) of
        true ->
            Occupied = lists:filter(fun(C) -> C == $# end, lists:flatten(NewLines)),
            length(Occupied);
        false ->
            do_p2(NewLines, Width, Height)
    end.

do_p2(_X, Y, _Lines, _Width, Height, Acc) when Y > Height ->
    Acc;
do_p2(X, Y, Lines, Width, Height, Acc1) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    AdjOccupied = count_adjacent_occupied(X, Y, Lines, Width, Height),
    NewChar =
        case Char of
            $. ->
                $.;
            $L ->
                case AdjOccupied == 0 of
                    true ->
                        $#;
                    false ->
                        $L
                end;
            $# ->
                case AdjOccupied > 4 of
                    true ->
                        $L;
                    false ->
                        $#
                end
        end,
    CurrLine = lists:nth(Y, Acc1),
    NewLine =
        case X of
            1 ->
                [NewChar] ++ tl(CurrLine);
            Width ->
                lists:reverse(tl(lists:reverse(CurrLine))) ++ [NewChar];
            _ ->
                {L1, L2} = lists:split(X - 1, CurrLine),
                L1 ++ [NewChar] ++ tl(L2)
        end,
    Acc2 =
        case Y of
            1 ->
                [NewLine] ++ tl(Acc1);
            Height ->
                lists:reverse(tl(lists:reverse(Acc1))) ++ [NewLine];
            _ ->
                {L11, L22} = lists:split(Y - 1, Acc1),
                L11 ++ [NewLine] ++ tl(L22)
        end,
    {NewX, NewY} =
        case X of
            Width ->
                {1, Y + 1};
            _ ->
                {X + 1, Y}
        end,
    do_p2(NewX, NewY, Lines, Width, Height, Acc2).

count_adjacent_occupied(X, Y, Lines, Width, Height) ->
    count_adjacent_occupied_ul(X - 1, Y - 1, Lines, Width, Height) +
        count_adjacent_occupied_u(X, Y - 1, Lines, Width, Height)
        + count_adjacent_occupied_ur(X + 1, Y - 1, Lines, Width, Height)
        + count_adjacent_occupied_l(X - 1, Y, Lines, Width, Height)
        + count_adjacent_occupied_r(X + 1, Y, Lines, Width, Height)
        + count_adjacent_occupied_bl(X - 1, Y + 1, Lines, Width, Height)
        + count_adjacent_occupied_b(X, Y + 1, Lines, Width, Height)
        + count_adjacent_occupied_br(X + 1, Y + 1, Lines, Width, Height).

count_adjacent_occupied_ul(X, Y, Lines, _Width, _Height) when X < 1; Y < 1 ->
    0;
count_adjacent_occupied_ul(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_ul(X - 1, Y - 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_u(X, Y, Lines, _Width, _Height) when Y < 1 ->
    0;
count_adjacent_occupied_u(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_u(X, Y - 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_ur(X, Y, Lines, Width, Height) when X > Width; Y < 1 ->
    0;
count_adjacent_occupied_ur(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_ur(X + 1, Y - 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_l(X, Y, Lines, _Width, _Height) when X < 1 ->
    0;
count_adjacent_occupied_l(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_l(X - 1, Y, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_r(X, Y, Lines, Width, Height) when X > Width ->
    0;
count_adjacent_occupied_r(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_r(X + 1, Y, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_bl(X, Y, Lines, Width, Height) when X < 1; Y > Height ->
    0;
count_adjacent_occupied_bl(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_bl(X - 1, Y + 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_b(X, Y, Lines, Width, Height) when Y > Height ->
    0;
count_adjacent_occupied_b(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_b(X, Y + 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

count_adjacent_occupied_br(X, Y, Lines, Width, Height) when Y > Height; X > Width ->
    0;
count_adjacent_occupied_br(X, Y, Lines, Width, Height) ->
    Line = lists:nth(Y, Lines),
    Char = lists:nth(X, Line),
    case Char of
        $. ->
            count_adjacent_occupied_br(X + 1, Y + 1, Lines, Width, Height);
        $L ->
            0;
        $# ->
            1
    end.

p2_test() ->
    T1 = [".......#.",
          "...#.....",
          ".#.......",
          ".........",
          "..#L....#",
          "....#....",
          ".........",
          "#........",
          "...#....."],
    ?assertEqual(8, count_adjacent_occupied(4, 5, T1, 9, 9)),
    T2 = [".............", ".L.L.#.#.#.#.", "............."],
    ?assertEqual(0, count_adjacent_occupied(2, 2, T2, 13, 3)),
    T3 = [".##.##.", "#.#.#.#", "##...##", "...L...", "##...##", "#.#.#.#", ".##.##."],
    ?assertEqual(0, count_adjacent_occupied(4, 4, T3, 7, 7)),
    L1 = ["L.LL.LL.LL",
          "LLLLLLL.LL",
          "L.L.L..L..",
          "LLLL.LL.LL",
          "L.LL.LL.LL",
          "L.LLLLL.LL",
          "..L.L.....",
          "LLLLLLLLLL",
          "L.LLLLLL.L",
          "L.LLLLL.LL"],
    L2 = ["#.##.##.##",
          "#######.##",
          "#.#.#..#..",
          "####.##.##",
          "#.##.##.##",
          "#.#####.##",
          "..#.#.....",
          "##########",
          "#.######.#",
          "#.#####.##"],
    ?assertEqual(L2, do_p2(1, 1, L1, 10, 10, L1)),
    L3 = ["#.LL.LL.L#",
          "#LLLLLL.LL",
          "L.L.L..L..",
          "LLLL.LL.LL",
          "L.LL.LL.LL",
          "L.LLLLL.LL",
          "..L.L.....",
          "LLLLLLLLL#",
          "#.LLLLLL.L",
          "#.LLLLL.L#"],
    ?assertEqual(L3, do_p2(1, 1, L2, 10, 10, L2)),
    L4 = ["#.L#.##.L#",
          "#L#####.LL",
          "L.#.#..#..",
          "##L#.##.##",
          "#.##.#L.##",
          "#.#####.#L",
          "..#.#.....",
          "LLL####LL#",
          "#.L#####.L",
          "#.L####.L#"],
    ?assertEqual(L4, do_p2(1, 1, L3, 10, 10, L3)),
    L5 = ["#.L#.L#.L#",
          "#LLLLLL.LL",
          "L.L.L..#..",
          "##LL.LL.L#",
          "L.LL.LL.L#",
          "#.LLLLL.LL",
          "..L.L.....",
          "LLLLLLLLL#",
          "#.LLLLL#.L",
          "#.L#LL#.L#"],
    ?assertEqual(L5, do_p2(1, 1, L4, 10, 10, L4)),
    L6 = ["#.L#.L#.L#",
          "#LLLLLL.LL",
          "L.L.L..#..",
          "##L#.#L.L#",
          "L.L#.#L.L#",
          "#.L####.LL",
          "..#.#.....",
          "LLL###LLL#",
          "#.LLLLL#.L",
          "#.L#LL#.L#"],
    ?assertEqual(L6, do_p2(1, 1, L5, 10, 10, L5)),
    ?assertEqual(2121, p2("src/d11_input.txt")).
