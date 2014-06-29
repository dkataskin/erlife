-module(erlife_protocol).
-author("Dmitry Kataskin").

%% API
-export([parse_initial_input/1, gen_delta_to_json/2]).

parse_initial_input(Nodes) ->
        array_to_state(Nodes, []).

array_to_state([], Acc) ->
        Acc;

array_to_state([X, Y | T], Acc) ->
        array_to_state(T, [{X, Y} | Acc]).

gen_delta_to_json(GenNum, Delta) ->
        jsx:encode([{<<"event">>, <<"nextGen">>},
                    {<<"data">>, [{<<"num">>, GenNum},
                                  {<<"nodeCount">>, 0},
                                  {<<"delta">>, prepare_delta(Delta)}]}]).

prepare_delta(Delta) ->
        Fun = fun({X, Y, Action}) ->
                case Action of
                  die ->
                    [false, X, Y];
                  arise ->
                    [true, X, Y]
                end
              end,
        lists:map(Fun, Delta).