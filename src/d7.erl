-module(d7).

-include_lib("eunit/include/eunit.hrl").

input(File) ->
    {ok, Bin} = file:read_file(File),%"src/d7_input.txt"),
    Str = binary_to_list(Bin),
    Lines = string:tokens(Str, ".\n"),
    [parse(L) || L <- Lines].

parse(Line) ->
    ColorSep = " bags contain ",
    ColorSepIndex = string:str(Line, ColorSep),
    Color = string:slice(Line, 0, ColorSepIndex - 1),
    S1 = string:slice(Line,
                      ColorSepIndex + length(ColorSep) -
                          1), % "5 light maroon bags, 4 pale tomato bags, 5 clear blue bags"
    case S1 of
        "no other bags" ->
            {Color, []};
        _ ->
            S2 = re:replace(S1,
                            " bag(s)?",
                            "",
                            [{return, list},
                             global]), % "5 light maroon, 4 pale tomato, 5 clear blue"
            S3 = re:replace(S2,
                            ", ",
                            ",",
                            [{return, list}, global]), % "5 light maroon,4 pale tomato,5 clear blue"
            L1 = string:tokens(S3, ","), % ["5 light maroon","4 pale tomato","5 clear blue"]
            L2 = lists:foldl(fun(S, Acc) ->
                                [CountStr | Rest] = string:tokens(S, " "),
                                Count = list_to_integer(CountStr),
                                C = string:join(Rest, " "),
                                lists:keystore(Color, 1, Acc, {C, Count})
                             end,
                             [],
                             L1),
            {Color, L2}
    end.

build_digraph(Rules) ->
    build_digraph(Rules, digraph:new()).

build_digraph([], G) ->
    G;
build_digraph([{Type1, List1} | Rules], G) ->
    digraph:add_vertex(G, Type1, Type1),
    lists:foreach(fun({Type2, Count}) ->
                     digraph:add_vertex(G, Type2, Type2),
                     digraph:add_edge(G, Type1, Type2, Count)
                  end,
                  List1),
    build_digraph(Rules, G).

p1(Bag, Digraph) ->
    Vertices = digraph:vertices(Digraph) -- [Bag],
    lists:foldl(fun(V, Acc) ->
                    case digraph:get_path(Digraph, V, Bag) of
                        false -> Acc;
                        _ -> Acc + 1
                    end
                end,
                0,
                Vertices).

p1_example_test() ->
    Rules = input("src/d7_input_example.txt"),
    Digraph = build_digraph(Rules),
    Bag = "shiny gold",
    ?assertEqual(4, p1(Bag, Digraph)),
    ok.

p1_test() ->
    ?assertEqual({"dark olive", [{"plaid black", 5}, {"dim plum", 2}, {"light cyan", 2}]},
                 parse("dark olive bags contain 5 plaid black bags, 2 dim plum bags, "
                       "2 light cyan bags")),
    ?assertEqual({"mirrored gold", [{"light teal", 3}]},
                 parse("mirrored gold bags contain 3 light teal bags")),
    Rules = input("src/d7_input.txt"),
    Digraph = build_digraph(Rules),
    Bag = "shiny gold",
    ?assertEqual(278, p1(Bag, Digraph)),
    ok.

p2(Bag, Rules) ->
    count_bags_of_type(Bag, Rules, 0).

count_bags_of_type(Type, Rules, Acc) ->
    case proplists:get_value(Type, Rules, []) of
        [] ->
            Acc;
        List ->
            lists:foldl(fun({T, C}, Acc1) ->
                Acc1 + C * count_bags_of_type(T, Rules, 1)
            end,
            0,
            List) + Acc
    end.

p2_example_test() ->
    Rules = input("src/d7_input_example.txt"),
    Bag = "shiny gold",
    ?assertEqual(32, p2(Bag, Rules)).

p2_test() ->
    Rules = input("src/d7_input.txt"),
    Bag = "shiny gold",
    ?assertEqual(45157, p2(Bag, Rules)).