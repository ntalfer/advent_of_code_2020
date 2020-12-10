-module(d8).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    string:tokens(Str, "\n").

p1(File) ->
    Lines = input(File),
    catch execute(Lines).

execute(Lines) ->
    execute(Lines, 1, 0, [1]).

execute(Lines, LineNb, Acc, _VisitedLines) when LineNb > length(Lines) ->
    throw({ok, Acc});
execute(Lines, LineNb, Acc, VisitedLines) ->
    Line = lists:nth(LineNb, Lines),
    {NewLineNb, NewAcc} =
        case string:tokens(Line, " ") of
            ["nop", _] -> {LineNb+1, Acc};
            ["jmp", Arg] -> {LineNb+list_to_integer(Arg), Acc};
            ["acc", Arg] -> {LineNb+1, Acc+list_to_integer(Arg)}
        end,
    case lists:member(NewLineNb, VisitedLines) of
        true ->
            throw({infinite_loop, Acc});
        false ->
            execute(Lines, NewLineNb, NewAcc, [NewLineNb|VisitedLines])
    end.

p1_example_test() ->
    ?assertEqual({infinite_loop, 5}, p1("src/d8_input_example.txt")).

p1_test() ->
    ?assertEqual({infinite_loop, 1331}, p1("src/d8_input.txt")).

p2(File) ->
    Program = input(File),
    do_p2(Program, Program, 1).

do_p2(OriginalProgram, CandidateProgram, Index) ->
    case catch execute(CandidateProgram) of
        {infinite_loop, _} ->
            {NewCandidateProgram, NewIndex} = alter_program(OriginalProgram, Index+1),
            do_p2(OriginalProgram, NewCandidateProgram, NewIndex);
        {ok, Acc} ->
            {ok, Acc}
    end.

alter_program(OriginalProgram, Index) ->
    Line = lists:nth(Index, OriginalProgram),
    [Cmd, Arg] = string:tokens(Line, " "),
    case Cmd of
        "acc" ->
            alter_program(OriginalProgram, Index+1);
        "jmp" ->
            {Head, Tail} = lists:split(Index, OriginalProgram),
            CutHead = lists:sublist(Head, length(Head)-1),
            NewProgram = CutHead ++ ["nop " ++ Arg] ++ Tail,
            {NewProgram, Index};
        "nop" ->
            {Head, Tail} = lists:split(Index, OriginalProgram),
            CutHead = lists:sublist(Head, length(Head)-1),
            NewProgram = CutHead ++ ["jmp " ++ Arg] ++ Tail,
            {NewProgram, Index}
    end.

p2_example_test() ->
    ?assertEqual({ok, 8}, p2("src/d8_input_example.txt")).

p2_test() ->
    ?assertEqual({ok, 1121}, p2("src/d8_input.txt")).