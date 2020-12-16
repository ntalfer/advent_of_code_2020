-module(d13).

-include_lib("eunit/include/eunit.hrl").

input() ->
    EarliestTimestamp = 1002576,
    S = "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x"
        ",19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17",
    IDs = lists:foldl(fun ("x", Acc) ->
                              Acc;
                          (Str, Acc) ->
                              [list_to_integer(Str) | Acc]
                      end,
                      [],
                      string:tokens(S, ",")),
    {EarliestTimestamp, IDs}.

p1() ->
    {EarliestTimestamp, IDs} = input(),
    do_p1(EarliestTimestamp, IDs).

do_p1(EarliestTimestamp, IDs) ->
    do_p1(EarliestTimestamp, IDs, {-1, -1}).

do_p1(EarliestTimestamp, [], {IDMax, TimestampMax}) ->
    (TimestampMax - EarliestTimestamp) * IDMax;
do_p1(EarliestTimestamp, [ID | IDs], {IDMax, TimestampMax}) ->
    case EarliestTimestamp div ID of
        0 ->
            0;
        Rem ->
            Timestamp = Rem * ID + ID,
            {NewIDMax, NewTimestampMax} =
                case TimestampMax of
                    -1 ->
                        {ID, Timestamp};
                    _ when Timestamp < TimestampMax ->
                        {ID, Timestamp};
                    _ ->
                        {IDMax, TimestampMax}
                end,
            case NewTimestampMax of
                EarliestTimestamp ->
                    0;
                _ ->
                    do_p1(EarliestTimestamp, IDs, {NewIDMax, NewTimestampMax})
            end
    end.

p1_test() ->
    ?assertEqual(295, do_p1(939, [7, 13, 59, 31, 19])),
    ?assertEqual(3865, p1()),
    ok.

p2() ->
    S = "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x"
        ",19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17",
    p2(S).

p2(S) ->
    {_, IDOffsets} =
        lists:foldl(fun ("x", {Count, Acc}) ->
                            {Count + 1, Acc};
                        (Str, {Count, Acc}) ->
                            {Count + 1, Acc ++ [{list_to_integer(Str), Count}]}
                    end,
                    {0, []},
                    string:tokens(S, ",")),
    do_p2(IDOffsets, 0, 1).

do_p2([], T, _Step) -> T;
do_p2([{ID,Offset}|IDs], T, Step) ->
    NewT = incr_t(ID, Offset, T, Step),
    do_p2(IDs, NewT, Step * ID).

incr_t(ID, Offset, T, Step) ->
    case ((T + Offset) rem ID) of
        0 ->
            T;
        _ ->
            incr_t(ID, Offset, T+Step, Step)
    end.

p2_test() ->
    ?assertEqual(3417, p2("17,x,13,19")),
    ?assertEqual(415579909629976, p2()),
    ok.