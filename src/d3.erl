-module(d3).

-include_lib("eunit/include/eunit.hrl").

p1(Map) ->
    DX = 3,
    DY = 1,
    p1(Map, DX, DY).

p1(Map, DX, DY) ->
    X = 0,
    Y = 0,
    Trees = 0,
    X_max = length(hd(Map)) - 1,
    Y_max = length(Map) - 1,
    do_p1(Map, X, Y, DX, DY, X_max, Y_max, Trees).

do_p1(Map, X, Y, DX, DY, X_max, Y_max, Trees) when X == 0, Y == 0 ->
    %% start
    do_p1(Map, X + DX, Y + DY, DX, DY, X_max, Y_max, Trees);
do_p1(_Map, _X, Y, _DX, DY, _X_max, Y_max, Trees) when Y == Y_max + DY ->
    %% at the bottom-right corner
    Trees;
do_p1(Map, X, Y, DX, DY, X_max, Y_max, Trees) when Y > Y_max + DY ->
    %% go to the last line
    %%io:fwrite("goto~n"),
    do_p1(Map, X, Y_max, DX, DY, X_max, Y_max, Trees);
do_p1(Map, X, Y, DX, DY, X_max, Y_max, Trees) when X > X_max ->
    %% out of the map, shift the map right
    %%io:fwrite("shift~n"),
    do_p1(Map, X - (X_max + 1), Y, DX, DY, X_max, Y_max, Trees);
do_p1(Map, X, Y, DX, DY, X_max, Y_max, Trees) ->
    Line = lists:nth(Y + 1, Map),
    %%io:fwrite("Line = ~p, X = ~p, Y = ~p - ", [Line, X, Y]),
    Trees1 =
    case lists:nth(X + 1, Line) of
        $# ->
            %% a tree
            %%io:fwrite("yes~n"),
            Trees + 1;
        _ ->
            %% not a tree
            %%io:fwrite("no~n"),
            Trees
    end,
    do_p1(Map, X + DX, Y + DY, DX, DY, X_max, Y_max, Trees1).

map() ->
    {ok, Bin} = file:read_file("src/d3_input.txt"),
    Str = binary_to_list(Bin),
    string:tokens(Str, "\n").

p1_example_test() ->
    Map = ["..##.......",
           "#...#...#..",
           ".#....#..#.",
           "..#.#...#.#",
           ".#...##..#.",
           "..#.##.....",
           ".#.#.#....#",
           ".#........#",
           "#.##...#...",
           "#...##....#",
           ".#..#...#.#"],
    ?assertEqual(7, p1(Map)).

p1_test() ->
    Map = map(),
    ?assertEqual(156, p1(Map)).

p2(Map) ->
    Slopes = [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}],
    lists:foldl(fun({DX, DY}, Acc) ->
                   Count = p1(Map, DX, DY),
                   %%io:fwrite("Slope = ~p, Count = ~p~n", [{DX, DY}, Count]),
                   Acc * Count
                end,
                1,
                Slopes).

p2_example_test() ->
    Map = ["..##.......",
           "#...#...#..",
           ".#....#..#.",
           "..#.#...#.#",
           ".#...##..#.",
           "..#.##.....",
           ".#.#.#....#",
           ".#........#",
           "#.##...#...",
           "#...##....#",
           ".#..#...#.#"],
    ?assertEqual(336, p2(Map)).

p2_test() ->
    Map = map(),
    ?assertEqual(3521829480, p2(Map)).
