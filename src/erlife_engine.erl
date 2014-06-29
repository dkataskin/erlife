-module(erlife_engine).
-author("Dmitry Kataskin").

-behaviour(gen_server).

-define(node_table, erlife_nodes).
-define(center, trunc(math:pow(2, 64) / 2)).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([next_gen/2, print/1]).

-record(state, { gen = 0 }).

-type cellkey() :: binary().
-type point() :: {int(), int()}.
-type viewport() :: {point(), point()}.
-type cellaction() :: die | arise.
-type celldelta() :: {cellkey(), cellaction()}.

% api
start_link(InitialState) ->
        gen_server:start_link(?MODULE, [InitialState], []).

-spec next_gen(Pid::pid(), Viewport::viewport()) -> {GenNum::int(), CellDelta::[celldelta()]}.
next_gen(Pid, Viewport) ->
        gen_server:call(Pid, {next_gen, Viewport}).

-spec print(Pid::pid()) -> ok.
print(Pid) ->
        gen_server:call(Pid, print).

% gen_server callbacks
-spec init(InitialState::[point()]) -> {ok, pid()}.
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

handle_call({next_gen, Viewport}, _From, State=#state{ gen = Gen }) ->
        {ok, Delta} = next_gen(Viewport),
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

next_gen(Viewport) ->
        TabId = ets:new(lookup_table, [set, {keypos, 1}]),
        Delta = calc_next_gen(Viewport, TabId),

        execute_actions(Delta),
        ets:delete(TabId),

        lists:foreach(fun({Key, Action, _, _}) ->
                        {X, Y} = to_point(Key),
                        io:format("~p {~p,~p}~n", [Action, X, Y])
                      end, Delta),
        {ok, Delta}.

calc_next_gen(Viewport, LookupTabId) ->
        Fun = fun(Cell, Acc) -> traverse_cell(Cell, Acc, LookupTabId, Viewport) end,
        ets:foldl(Fun, [], ?node_table).

traverse_cell(Cell={Key, alive}, ViewportDelta, LookupTabId, Viewport) ->
        Neig = get_neig(Key),
        Count = alive_count(Neig),
        {ok, Action} = add_action(Cell, Count, LookupTabId),
        ViewportDelta1 = add_to_viewport(Action, Viewport, ViewportDelta),

        Fun = fun(CellElem={CellKey, State}, ViewportDelta2) ->
                case State of
                  alive ->
                    ViewportDelta2;

                  empty ->
                    case ets:lookup(LookupTabId, CellKey) of
                      [] -> traverse_cell(CellElem, ViewportDelta2, LookupTabId, Viewport);
                      [_] -> ViewportDelta2
                    end
                end
              end,

        lists:foldl(Fun, ViewportDelta1, Neig);

traverse_cell(Cell={Key, empty}, ViewportDelta, LookupTabId, Viewport) ->
        Neig = get_neig(Key),
        Count = alive_count(Neig),
        {ok, Action} = add_action(Cell, Count, LookupTabId),
        ViewportDelta1 = add_to_viewport(Action, Viewport, ViewportDelta),
        ViewportDelta1.

add_action({Key, empty}, Count, LookupTabId) when Count =:= 3 ->
        <<X:64, Y:64>> = Key,
        Action = {Key, arise, X, Y},
        ets:insert(LookupTabId, Action),
        {ok, Action};

add_action({_, empty}, _, _) ->
        {ok, no_action};

add_action({Key, alive}, Count, LookupTabId) when Count < 2 orelse Count > 3 ->
        <<X:64, Y:64>> = Key,
        Action = {Key, die, X, Y},
        ets:insert(LookupTabId, Action),
        {ok, Action};

add_action({_, alive}, _, _) ->
        {ok, no_action}.

add_to_viewport(no_action, _, ViewportDelta) ->
        ViewportDelta;

add_to_viewport({_, State, X, Y}, {{MinX, MinY}, {MaxX, MaxY}}, ViewportDelta)
    when X >= MinX andalso X =< MaxX andalso Y >= MinY andalso Y =< MaxY ->
        [{X, Y, State} | ViewportDelta];

add_to_viewport(_, _, ViewportDelta) ->
        ViewportDelta.

execute_actions(LookupTabId) ->
        Fun = fun(Action, _) ->
                exec_action(Action),
                no_acc
              end,
        ets:foldl(Fun, no_acc, LookupTabId).

exec_action({Key, die, _, _}) ->
        ets:delete(?node_table, Key);

exec_action({Key, arise, _, _}) ->
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