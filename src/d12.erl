-module(d12).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    [{Action, list_to_integer(Count)} || [Action|Count] <- string:tokens(Str, "\n")].

p1(File) ->
    Lines = input(File),
    do_p1(Lines).

do_p1(Lines) ->
    {X, Y, _Degrees} = do_p1(Lines, 0, 0, 0),
    abs(X) + abs(Y).

do_p1([], X, Y, Degrees) ->
    {X, Y, Degrees};
do_p1([{Action, Count}|Lines], X, Y, Degrees) ->
    case Action of
        $F ->
            case Degrees of
                0 -> do_p1(Lines, X+Count, Y, Degrees);
                90 -> do_p1(Lines, X, Y+Count, Degrees);
                180 -> do_p1(Lines, X-Count, Y, Degrees);
                270 -> do_p1(Lines, X, Y-Count, Degrees)
            end;
        $N -> do_p1(Lines, X, Y+Count, Degrees);
        $E -> do_p1(Lines, X+Count, Y, Degrees);
        $W -> do_p1(Lines, X-Count, Y, Degrees);
        $S -> do_p1(Lines, X, Y-Count, Degrees);
        $R ->
            case Degrees - Count of
                D when D >= 0 ->
                    do_p1(Lines, X, Y, D);
                D ->
                    do_p1(Lines, X, Y, 360+D)
            end;
        $L ->
            do_p1(Lines, X, Y, (Degrees+Count) rem 360)
    end.

p1_test() ->
    ?assertEqual(858, p1("src/d12_input.txt")).

p2(File) ->
    Lines = input(File),
    do_p2(Lines).

do_p2(Lines) ->
    {X, Y, _, _} = do_p2(Lines, 0, 0, 10, 1),
    abs(X) + abs(Y).

do_p2([], X, Y, WX, WY) ->
    {X, Y, WX, WY};
do_p2([{Action, Count}|Lines], X, Y, WX, WY) ->
    case Action of
        $F -> do_p2(Lines, X+Count*WX, Y+Count*WY, WX, WY);
        $N -> do_p2(Lines, X, Y, WX, WY+Count);
        $E -> do_p2(Lines, X, Y, WX+Count, WY);
        $W -> do_p2(Lines, X, Y, WX-Count, WY);
        $S -> do_p2(Lines, X, Y, WX, WY-Count);
        $R -> do_p2([{$L, 360-Count}|Lines], X, Y, WX, WY);
        $L ->
            case Count of
                0 -> do_p2(Lines, X, Y, WX, WY);
                90 -> do_p2(Lines, X, Y, -WY, WX);
                180 -> do_p2(Lines, X, Y, -WX, -WY);
                270 -> do_p2(Lines, X, Y, WY, -WX)
            end
    end.

p2_test() ->
    ?assertEqual(39140, p2("src/d12_input.txt")).