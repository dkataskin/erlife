-module(erlife_protocol).
-author("Dmitry Kataskin").

-define(viewport_prop, <<"viewport">>).
-define(state_changes_prop, <<"statechanges">>).
-define(invalidate_prop, <<"invalidate">>).
-define(name_prop, <<"name">>).
-define(id_prop, <<"id">>).

%% API
-export([parse_nextgen_input/1, parse_save_input/1, parse_load_input/1]).
-export([dump_list_to_json/1, gen_delta_to_json/2]).

parse_nextgen_input([{?viewport_prop, [MinX, MinY, MaxX, MaxY]},
                     {?state_changes_prop, StateChanges},
                     {?invalidate_prop, Invalidate}]) ->
        Viewport = {{MinX, MinY}, {MaxX, MaxY}},
        StateChanges1 = parse_state_changes(StateChanges),
        case Invalidate of
          true ->
            {ok, Viewport, StateChanges1, [invalidate]};
          false ->
            {ok, Viewport, StateChanges1, []}
        end.

parse_state_changes(Array) ->
        array_to_state_changes(Array, []).

parse_save_input([{?id_prop, Id},
                  {?name_prop, Name},
                  {?state_changes_prop, StateChanges}]) ->
        case Id of
          null ->
            {undefined, Name, parse_state_changes(StateChanges)};
          Id ->
            {Id, Name, parse_state_changes(StateChanges)}
        end.

parse_load_input([{?id_prop, Id}, {?viewport_prop, [MinX, MinY, MaxX, MaxY]}]) ->
        Viewport = {{MinX, MinY}, {MaxX, MaxY}},
        {Id, Viewport}.

array_to_state_changes([], Acc) ->
        Acc;

array_to_state_changes([X, Y, Alive | T], Acc) ->
        array_to_state_changes(T, [{X, Y, Alive} | Acc]).

dump_list_to_json(Dumps) ->
        Fun = fun({Id, Name}, Acc) ->
                Json = case is_list(Name) of
                        true ->
                          [{?id_prop, Id}, {?name_prop, list_to_binary(Name)}];
                        false ->
                          [{?id_prop, Id}, {?name_prop, Name}]
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