-module(erlife_protocol).
-author("Dmitry Kataskin").

%% API
-export([parse_initial_input/1, prepare_delta/1]).

parse_initial_input(Nodes) ->
        array_to_initialstate(Nodes, []).

array_to_initialstate([], Acc) ->
        Acc;

array_to_initialstate([X, Y | T], Acc) ->
        array_to_initialstate(T, [{X, Y} | Acc]).

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