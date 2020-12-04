-module(d4).

-include_lib("eunit/include/eunit.hrl").

p1(Passports) ->
    lists:foldl(fun(P, Acc) ->
                   case is_valid_passport_p1(P) of
                       true -> Acc + 1;
                       false -> Acc
                   end
                end,
                0,
                Passports).

p1_test() ->
    Passports = input("src/d4_input.txt"),
    ?assertEqual(210, p1(Passports)).

input(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    Passports = re:split(Str, "\n\n", [{return, list}]),
    lists:map(fun(Passport) ->
                 Tokens = string:tokens(Passport, "\n :"),
                 build_passport(Tokens, #{})
              end,
              Passports).

build_passport([], Passport) ->
    Passport;
build_passport([Key, Value | Tokens], Passport) ->
    build_passport(Tokens, maps:put(Key, Value, Passport)).

is_valid_passport_p1(Passport) ->
    lists:all(fun(Key) -> maps:is_key(Key, Passport) end,
              ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]).

is_valid_passport_p2(Passport) ->
    lists:all(fun(Fun) ->
            Fun(Passport)
            end,
              [fun is_valid_byr/1,
               fun is_valid_iyr/1,
               fun is_valid_eyr/1,
               fun is_valid_hgt/1,
               fun is_valid_hcl/1,
               fun is_valid_ecl/1,
               fun is_valid_pid/1]).

is_valid_byr(#{"byr" := S}) ->
    case catch list_to_integer(S) of
        {'EXIT', _} ->
            false;
        Y ->
            Y >= 1920 andalso Y =< 2002
    end;
is_valid_byr(_) ->
    false.

is_valid_iyr(#{"iyr" := S}) ->
    case catch list_to_integer(S) of
        {'EXIT', _} ->
            false;
        Y ->
            Y >= 2010 andalso Y =< 2020
    end;
is_valid_iyr(_) ->
    false.

is_valid_eyr(#{"eyr" := S}) ->
    case catch list_to_integer(S) of
        {'EXIT', _} ->
            false;
        Y ->
            (Y >= 2020) andalso (Y =< 2030)
    end;
is_valid_eyr(_) ->
    false.

is_valid_hgt(#{"hgt" := H}) ->
    case re:run(H, "^[0-9]*cm$") of
        {match, _} ->
            [S | _] = re:split(H, "cm", [{return, list}]),
            I = list_to_integer(S),
            I >= 150 andalso I =< 193;
        _ ->
            case re:run(H, "^[0-9]*in$") of
                {match, _} ->
                    [S | _] = re:split(H, "in", [{return, list}]),
                    I = list_to_integer(S),
                    I >= 59 andalso I =< 76;
                _ ->
                    false
            end
    end;
is_valid_hgt(_) ->
    false.

is_valid_hcl(#{"hcl" := S}) ->
    case re:run(S, "^#[0-9a-f]{6}$") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_valid_hcl(_) ->
    false.

is_valid_ecl(#{"ecl" := S}) ->
    lists:member(S, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]);
is_valid_ecl(_) ->
    false.

is_valid_pid(#{"pid" := S}) ->
    case re:run(S, "^[0-9]{9}$") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_valid_pid(_) ->
    false.

p2(Passports) ->
    lists:foldl(fun(P, Acc) ->
                   case is_valid_passport_p2(P) of
                       true -> Acc + 1;
                       false -> Acc
                   end
                end,
                0,
                Passports).

p2_example_1_test() ->
    Passports = input("src/d4_input_example_1.txt"),
    ?assertEqual(0, p2(Passports)).

p2_example_2_test() ->
    Passports = input("src/d4_input_example_2.txt"),
    ?assertEqual(4, p2(Passports)).

p2_test() ->
    Passports = input("src/d4_input.txt"),
    ?assertEqual(131, p2(Passports)).