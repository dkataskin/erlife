-module(erlife_engine).
-author("Dmitry Kataskin").

-behaviour(gen_server).

-define(node_table, erlife_nodes).
-define(center, trunc(math:pow(2, 64) / 2)).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([next_gen/1, print/1]).

-record(state, { gen = 0 }).

% api
start_link(InitialState) ->
  gen_server:start_link(?MODULE, [InitialState], []).

next_gen(Pid) ->
  gen_server:call(Pid, next_gen).

print(Pid) ->
  gen_server:call(Pid, print).

% gen_server callbacks
% [{1,1},{2,2]
init([InitialState]) ->
        ets:new(?node_table, [set, {keypos, 1}, {read_concurrency, true}, named_table]),
        fill_initial(InitialState),
        {ok, #state{ gen = 0 }}.

handle_call(print, _From, State) ->
        ets:foldl(fun({Key, _}, []) ->
                      {X, Y} = to_point(Key),
                      io:format("X:~p Y:~p~n", [X, Y]), []
                  end, [], ?node_table),
        {reply, ok, State};

handle_call(next_gen, _From, State=#state{ gen = Gen }) ->
        Delta = traverse_univ(),
        lists:foreach(fun({Action, Key}) ->
                        {X, Y} = to_point(Key),
                        io:format("~p {~p,~p}~n", [Action, X, Y])
                      end, Delta),

        execute_actions(Delta),

        NewState = State#state { gen = Gen + 1 },
        {reply, {ok, {NewState#state.gen, Delta}}, NewState};

handle_call(_Request, _From, State) ->
        {reply, ok, State}.

handle_cast(_Request, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

fill_initial(InitialState) ->
        lists:foreach(fun({X, Y}) -> ets:insert(?node_table, {to_key(X, Y), alive}) end, InitialState),
        ok.

traverse_univ() ->
        TabId = ets:new(lookup_table, [set, {keypos, 1}]),
        Fun = fun(Cell, Acc) -> traverse_cell(Cell, Acc, TabId) end,
        ets:foldl(Fun, [], ?node_table).

traverse_cell(Cell={Key, alive}, Actions, LookupTabId) ->
        Neig = get_neig(Key),
        Count = alive_count(Neig),
        Actions1 = add_action(Cell, Count, Actions),

        Fun = fun(CellElem={_, State}, Acc) ->
                case State of
                  alive ->
                    Acc;
                  empty ->
                    traverse_cell(CellElem, Acc)
                end
              end,

        lists:foldl(Fun, Actions1, Neig);

traverse_cell(Cell={Key, empty}, Actions) ->
        Neig = get_neig(Key),
        Count = alive_count(Neig),
        add_action(Cell, Count, Actions).

add_action({Key, empty}, Count, Actions) when Count =:= 3 ->
        [{arise, Key} | Actions];

add_action({_, empty}, _, Actions) ->
        Actions;

add_action({Key, alive}, Count, Actions) when Count < 2 orelse Count > 3 ->
        [{die, Key} | Actions];

add_action({_, alive}, _, Actions) ->
        Actions.

execute_actions(Actions) ->
        lists:foreach(fun exec_action/1, Actions).

exec_action({die, Key}) ->
        ets:delete(?node_table, Key);

exec_action({arise, Key}) ->
        ets:insert(?node_table, {Key, alive}).

alive_count(Cells) ->
        Fun = fun({_, alive}, Sum) -> Sum + 1;
                 ({_, empty}, Sum) -> Sum
              end,
        lists:foldl(Fun, 0, Cells).

get_neig(<<X:64, Y:64>>) ->
        NW = get_state(<<(X-1):64, (Y-1):64>>),
        NC = get_state(<<X:64, (Y-1):64>>),
        NE = get_state(<<(X + 1):64, (Y-1):64>>),

        CW = get_state(<<(X-1):64, Y:64>>),
        CE = get_state(<<(X+1):64, Y:64>>),

        SW = get_state(<<(X-1):64, (Y+1):64>>),
        SC = get_state(<<X:64, (Y+1):64>>),
        SE = get_state(<<(X + 1):64, (Y+1):64>>),
        [NW, NC, NE, CW, CE, SW, SC, SE].

get_state(Key) ->
        case ets:lookup(?node_table, Key) of
          [] ->
            {Key, empty};
          [_] ->
            {Key, alive}
        end.

to_point(<<X:64, Y:64>>) ->
        {X - ?center, Y - ?center}.

to_key(X, Y) ->
        X1 = X + ?center,
        Y1 = Y + ?center,
        <<X1:64, Y1:64>>.