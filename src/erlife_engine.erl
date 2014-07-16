-module(erlife_engine).
-author("Dmitry Kataskin").

-behaviour(gen_server).

-define(center, trunc(math:pow(2, 64) / 2)).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([next_gen/1]).
-export([apply_changes/2, clear/1, get_viewport/2, live_count/1]).
-export([dump_state/1, restore_from_dump/2]).

-type cellkey() :: binary().
-type point() :: {integer(), integer()}.
-type rect() :: {point(), point()}.
-type cellaction() :: die | arise.
-type celldelta() :: {cellkey(), cellaction()}.
-type cellchange() :: {integer(), integer(), cellaction()}.
-type viewportdata() :: [celldelta()].
-type gendata() :: {non_neg_integer(), viewportdata()}.

-record(qtree, { rect = undefined :: rect(), is_leaf = false :: boolean(), nodes = [] :: [qtree()] }).
-type qtree() :: #qtree{}.

-record(state, { gen = 0,
                 liveCount = 0,
                 genTime = 0,
                 univ = #qtree{} }).

% api
start_link(Id) ->
        gen_server:start_link(?MODULE, [Id], []).

stop(Pid) ->
        gen_server:call(Pid, stop).

next_gen(Pid) ->
        gen_server:call(Pid, {next_gen}).

apply_changes(Pid, ChangesToState) ->
        gen_server:call(Pid, {apply_changes, ChangesToState}).

clear(Pid) ->
        gen_server:call(Pid, clear).

get_viewport(Pid, Viewport) ->
        gen_server:call(Pid, {get_viewport, Viewport}).

live_count(Pid) ->
        gen_server:call(Pid, live_count).

dump_state(Pid) ->
        gen_server:call(Pid, dump_state).

restore_from_dump(Pid, DumpTabId) ->
        gen_server:call(Pid, {restore_from_dump, DumpTabId}).

% gen_server callbacks
-spec init(Id::binary()) -> {ok, pid()}.
init([Id]) ->
        gproc:add_local_name(Id),
        {ok, #state{ gen = 0, tab_id = TabId }}.

handle_call({next_gen, {Min, Max}, ChangesToState, Options}, _From, State=#state{ gen = Gen, tab_id = TabId }) ->
        Viewport1 = {translate(to_server, Min), translate(to_server, Max)},
        Actions = changes_to_action_list(ChangesToState),
        {ok, _} = apply_changes_to_state(Actions, TabId),
        {ok, {Delta, Count}} = calc_next_gen(Viewport1, TabId),
        Resp = case proplists:lookup(invalidate, Options) of
                 {invalidate, true} ->
                   invalidate_viewport(Viewport1, TabId);
                 none ->
                   Delta
               end,

        NewState = State#state { gen = Gen + 1, liveCount = Count },
        {reply, {ok, {NewState#state.gen, NewState#state.liveCount, Resp}}, NewState};

handle_call({apply_changes, ChangesToState}, _From, State=#state { tab_id = TabId, liveCount = Count }) ->
        Actions = changes_to_action_list(ChangesToState),
        {ok, {_, CountChange}} = apply_changes_to_state(Actions, TabId),
        NewState = State#state { liveCount = Count + CountChange },
        {reply, {ok, applied}, NewState};

handle_call({get_viewport, {Min, Max}}, _From, State=#state { tab_id = TabId }) ->
        Viewport1 = {translate(to_server, Min), translate(to_server, Max)},
        ViewportData = invalidate_viewport(Viewport1, TabId),
        {reply, {ok, ViewportData}, State};

handle_call(dump_state, _From, State=#state { tab_id = TabId }) ->
        {reply, {dumped, TabId}, State};

handle_call(live_count, _From, State=#state { liveCount = Count }) ->
        {reply, {ok, Count}, State};

handle_call({restore_from_dump, DumpTabId}, _From, #state { tab_id = TabId }) ->
        true = ets:delete(TabId),
        LiveCount = ets:foldl(fun(_, Count) -> Count + 1 end, 0, DumpTabId),
        NewState = #state { gen = 0, tab_id = DumpTabId, liveCount = LiveCount },

        {reply, {ok, restored}, NewState};

handle_call(clear, _From, State=#state { tab_id = TabId }) ->
        true = ets:delete_all_objects(TabId),
        NewState = State#state { gen = 0, liveCount = 0 },
        {reply, {ok, cleared}, NewState};

handle_call(stop, _From, State) ->
        {stop, normal, ok, State};

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

calc_next_gen(Viewport, WorldTabId) ->
        LookupTabId = ets:new(lookup_table, [set, {keypos, 1}]),
        {ok, LiveCount} = traverse_cells(WorldTabId, LookupTabId),
        {ok, {ViewportDelta, CountChange}} = apply_changes_to_state(WorldTabId, LookupTabId, Viewport),
        ets:delete(LookupTabId),
        {ok, {ViewportDelta, LiveCount + CountChange}}.

traverse_cells(WorldTabId, LookupTabId) ->
        Fun = fun(Cell, LiveCount) ->
                traverse_cell(Cell, WorldTabId, LookupTabId),
                LiveCount + 1
              end,
        LiveCount = ets:foldl(Fun, 0, WorldTabId),
        {ok, LiveCount}.

traverse_cell(Cell={Key, alive}, WorldTabId, LookupTabId) ->
        Neig = get_neig(Key, WorldTabId),
        Count = alive_count(Neig),
        ok = decide_on_cell(Cell, Count, LookupTabId),

        Fun = fun(CellElem={CellKey, State}) ->
                case State of
                  alive ->
                    ok;

                  empty ->
                    case ets:lookup(LookupTabId, CellKey) of
                      [] -> traverse_cell(CellElem, WorldTabId, LookupTabId);
                      [_] -> ok
                    end
                end
              end,

        lists:foreach(Fun, Neig);

traverse_cell(Cell={Key, empty}, WorldTabId, LookupTabId) ->
        Neig = get_neig(Key, WorldTabId),
        Count = alive_count(Neig),
        ok = decide_on_cell(Cell, Count, LookupTabId),
        ok.

decide_on_cell({Key, empty}, Count, LookupTabId) when Count =:= 3 ->
        ets:insert(LookupTabId, {Key, arise}),
        ok;

decide_on_cell({_, empty}, _, _) ->
        ok;

decide_on_cell({Key, alive}, Count, LookupTabId) when Count < 2 orelse Count > 3 ->
        ets:insert(LookupTabId, {Key, die}),
        ok;

decide_on_cell({_, alive}, _, _) ->
        ok.

add_to_viewport({<<X:64, Y:64>>, State}, {{MinX, MinY}, {MaxX, MaxY}}, ViewportDelta)
    when X >= MinX andalso X =< MaxX andalso Y >= MinY andalso Y =< MaxY ->
        {X1, Y1} = translate(to_client, {X, Y}),
        [{X1, Y1, State} | ViewportDelta];

add_to_viewport(_, _, ViewportDelta) ->
        ViewportDelta.

changes_to_action_list(ChangesToState) ->
        Fun = fun({X, Y, Alive}) ->
          {X1, Y1} = translate(to_server, {X, Y}),
          case Alive of
            true ->
              {<<X1:64, Y1:64>>, arise};
            false ->
              {<<X1:64, Y1:64>>, die}
          end
        end,
        lists:map(Fun, ChangesToState).

apply_changes_to_state(Actions, WorldTabId) ->
        Fun = fun(Action, Count) -> exec_action(Action, Count, WorldTabId) end,
        CountChange = lists:foldl(Fun, 0, Actions),
        {ok, {[], CountChange}}.

apply_changes_to_state(WorldTabId, LookupTabId, Viewport) ->
        Fun = fun(Action, {ViewportDelta, Count}) ->
                Count1 = exec_action(Action, Count, WorldTabId),
                ViewPortDelta1 = add_to_viewport(Action, Viewport, ViewportDelta),
                {ViewPortDelta1, Count1}
              end,
        Acc = ets:foldl(Fun, {[], 0}, LookupTabId),
        {ok, Acc}.

exec_action(Action, Count, WorldTabId) ->
        case exec_action(Action, WorldTabId) of
          {ok, died} -> Count - 1;
          {ok, born} -> Count + 1
        end.

exec_action({Key, die}, TabId) ->
        ets:delete(TabId, Key),
        {ok, died};

exec_action({Key, arise}, TabId) ->
        ets:insert(TabId, {Key, alive}),
        {ok, born}.

alive_count(Cells) ->
        Fun = fun({_, alive}, Sum) -> Sum + 1;
                 ({_, empty}, Sum) -> Sum
              end,
        lists:foldl(Fun, 0, Cells).

get_neig(<<X:64, Y:64>>, TabId) ->
        NW = get_state(<<(X-1):64, (Y-1):64>>, TabId),
        NC = get_state(<<X:64, (Y-1):64>>, TabId),
        NE = get_state(<<(X + 1):64, (Y-1):64>>, TabId),

        CW = get_state(<<(X-1):64, Y:64>>, TabId),
        CE = get_state(<<(X+1):64, Y:64>>, TabId),

        SW = get_state(<<(X-1):64, (Y+1):64>>, TabId),
        SC = get_state(<<X:64, (Y+1):64>>, TabId),
        SE = get_state(<<(X + 1):64, (Y+1):64>>, TabId),
        [NW, NC, NE, CW, CE, SW, SC, SE].

invalidate_viewport({{MinX, MinY}, {MaxX, MaxY}}, TabId) ->
        invalidate_viewport(MinY, MaxY, MinX, MaxX, TabId, []).

invalidate_viewport(MaxY, MaxY, X, MaxX, TabId, Acc) ->
        Acc1 = fill_row(X, MaxX, MaxY, TabId, Acc),
        Acc1;

invalidate_viewport(Y, MaxY, X, MaxX, TabId, Acc) ->
        Acc1 = fill_row(X, MaxX, Y, TabId, Acc),
        invalidate_viewport(Y + 1, MaxY, X, MaxX, TabId, Acc1).

fill_row(MaxX, MaxX, Y, TabId, Acc) ->
        fill_by_key(MaxX, Y, TabId, Acc),
        Acc;

fill_row(X, MaxX, Y, TabId, Acc) ->
        Acc1 = fill_by_key(X, Y, TabId, Acc),
        fill_row(X + 1, MaxX, Y, TabId, Acc1).

fill_by_key(X, Y, TabId, Acc) ->
        case ets:lookup(TabId, <<X:64, Y:64>>) of
          [] ->
            Acc;
          [_] ->
            {X1, Y1} = translate(to_client, {X, Y}),
            [{X1, Y1, arise} | Acc]
        end.

get_state(Key, TabId) ->
        case ets:lookup(TabId, Key) of
          [] ->
            {Key, empty};
          [_] ->
            {Key, alive}
        end.

translate(to_server, {X, Y}) ->
        {X + ?center, Y + ?center};

translate(to_client, {X, Y}) ->
        {X - ?center, Y - ?center}.