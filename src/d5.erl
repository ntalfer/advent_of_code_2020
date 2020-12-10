-module(d5).

-include_lib("eunit/include/eunit.hrl").

p1(BPs) ->
    lists:max([seat_id(BP) || BP <- BPs]).

seat_id(BP) ->
    {RowLetters, ColLetters} = lists:split(7, BP),
    Row = position(0, 127, RowLetters),
    Col = position(0, 7, ColLetters),
    Row * 8 + Col.

position(Pos, Pos, []) ->
    Pos;
position(PosMin, PosMax, [Letter | Letters]) ->
    %io:fwrite("Letter=~p, PosMin=~p, PosMax=~p~n", [Letter, PosMin, PosMax]),
    case Letter of
        L when L == $F; L == $L ->
            NewPosMax = PosMin + (PosMax - PosMin) div 2,
            %io:fwrite("NewPosMin=~p, NewPosMax=~p~n", [PosMin, NewPosMax]),
            position(PosMin, NewPosMax, Letters);
        L when L == $B; L == $R ->
            NewPosMin = PosMax - (PosMax - PosMin) div 2,
            %io:fwrite("NewPosMin=~p, NewPosMax=~p~n", [NewPosMin, PosMax]),
            position(NewPosMin, PosMax, Letters)
    end.

p1_test() ->
    ?assertEqual(44, position(0, 127, "FBFBBFF")),
    ?assertEqual(5, position(0, 7, "RLR")),
    ?assertEqual(357, seat_id("FBFBBFFRLR")),
    BPs = input("src/d5_input.txt"),
    ?assertEqual(963, p1(BPs)).

input(File) ->
    {ok, Bin} = file:read_file(File),
    re:split(Bin, "\n", [{return, list}]).

p2(BPs) ->
    SeatIDs = [seat_id(BP) || BP <- BPs],
    MaxSeatID = p1(BPs),
    AllSeatIDs = lists:seq(0, MaxSeatID),
    Candidates = remove_front_rows(AllSeatIDs -- SeatIDs),
    io:fwrite("Candidates = ~p~n", [Candidates]),
    find_seat(Candidates).

remove_front_rows(IDs) ->
    remove_front_rows(IDs, 0).

remove_front_rows(IDs, ID) ->
    case lists:member(ID, IDs) of
        true ->
            remove_front_rows(IDs -- [ID], ID + 1);
        false ->
            IDs
    end.

find_seat([ID]) ->
    ID;
find_seat([ID | IDs]) ->
    case ID < 8 of
        true ->
            %% front row
            find_seat(IDs);
        false ->
            case ID div 8 of
                127 ->
                    %% back row
                    find_seat(IDs);
                _ ->
                    ID
            end
    end.

p2_test() ->
    BPs = input("src/d5_input.txt"),
    ?assertEqual(592, p2(BPs)).
