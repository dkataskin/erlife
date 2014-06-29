-module(erlife_protocol).
-author("Dmitry Kataskin").

%% API
-export([parse_nextgen_input/1, parse_initial_input/1, parse_save_input/1, parse_load_input/1]).
-export([dump_list_to_json/1, gen_delta_to_json/2]).

parse_nextgen_input([{<<"viewport">>, [MinX, MinY, MaxX, MaxY]}, {<<"invalidate">>, Invalidate}]) ->
        Viewport = {{MinX, MinY}, {MaxX, MaxY}},
        case Invalidate of
          true ->
            {ok, Viewport, [invalidate]};
          false ->
            {ok, Viewport, []}
        end.

parse_initial_input(Nodes) ->
        array_to_state(Nodes, []).

parse_save_input([{<<"name">>, Name}]) ->
        Name.

parse_load_input([{<<"id">>, Id}, {<<"viewport">>, [MinX, MinY, MaxX, MaxY]}]) ->
        Viewport = {{MinX, MinY}, {MaxX, MaxY}},
        {Id, Viewport}.

array_to_state([], Acc) ->
        Acc;

array_to_state([X, Y | T], Acc) ->
        array_to_state(T, [{X, Y} | Acc]).

dump_list_to_json(Dumps) ->
      Fun = fun({Id, Name}, Acc) ->
              Json = case is_list(Name) of
                      true ->
                        [{<<"id">>, Id}, {<<"name">>, list_to_binary(Name)}];
                      false ->
                        [{<<"id">>, Id}, {<<"name">>, Name}]
                     end,
              [Json | Acc]
            end,
      DumpList = lists:foldl(Fun, [], Dumps),
      jsx:encode([{<<"event">>, <<"savedstates">>},
                  {<<"data">>, DumpList}]).

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