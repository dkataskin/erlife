-module(erlife_engine).
-author("Dmitry Kataskin").

-behaviour(gen_server).

-define(center, trunc(math:pow(2, 64) / 2)).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([next_gen/3, next_gen/4]).
-export([dump_state/1, restore_from_dump/2]).
-export([apply_changes/2, clear/1, get_viewport/2]).

-record(state, { gen = 0,
                 liveCount = 0,
                 genTime = 0,
                 tab_id = undefined }).

-type cellkey() :: binary().
-type point() :: {integer(), integer()}.
-type viewport() :: {point(), point()}.
-type cellaction() :: die | arise.
-type celldelta() :: {cellkey(), cellaction()}.
-type cellchange() :: {integer(), integer(), cellaction()}.
-type viewportdata() :: [celldelta()].
-type gendata() :: {non_neg_integer(), viewportdata()}.

% api
start_link(Id) ->
        gen_server:start_link(?MODULE, [Id], []).

stop(Pid) ->
        gen_server:call(Pid, stop).

-spec next_gen(Pid::pid(), Viewport::viewport(), ChangesToState::[cellchange()]) ->
            {ok, gendata()}.
next_gen(Pid, Viewport, ChangesToState) ->
        gen_server:call(Pid, {next_gen, Viewport, ChangesToState, []}).

next_gen(Pid, Viewport, ChangesToState, Options) ->
        gen_server:call(Pid, {next_gen, Viewport, ChangesToState, Options}).

-spec apply_changes(Pid::pid(), ChangesToState::[cellchange()]) -> {ok, applied}.
apply_changes(Pid, ChangesToState) ->
        gen_server:call(Pid, {apply_changes, ChangesToState}).

-spec clear(Pid::pid()) -> {ok, cleared}.
clear(Pid) ->
        gen_server:call(Pid, clear).

-spec get_viewport(Pid::pid(), Viewport::viewport()) -> {ok, viewportdata()}.
get_viewport(Pid, Viewport) ->
        gen_server:call(Pid, {get_viewport, Viewport}).

-spec dump_state(Pid::pid()) -> {dumped, ets:tid()}.
dump_state(Pid) ->
        gen_server:call(Pid, dump_state).

-spec restore_from_dump(Pid::pid(), DumpTabId::ets:tid()) -> {ok, restored}.
restore_from_dump(Pid, DumpTabId) ->
        gen_server:call(Pid, {restore_from_dump, DumpTabId}).

% gen_server callbacks
-spec init(Id::binary()) -> {ok, pid()}.
init([Id]) ->
        TabId = ets:new(node_table, [set, {keypos, 1}]),
        gproc:add_local_name(Id),
        {ok, #state{ gen = 0, tab_id = TabId }}.

handle_call({next_gen, {Min, Max}, ChangesToState, Options}, _From, State=#state{ gen = Gen, tab_id = TabId }) ->
        Viewport1 = {translate(to_server, Min), translate(to_server, Max)},
        Actions = changes_to_action_list(ChangesToState),
        ok = apply_changes_to_state(Actions, TabId),
        {ok, Delta} = calc_next_gen(Viewport1, TabId),

        Resp = case proplists:lookup(invalidate, Options) of
                 {invalidate, true} ->
                   invalidate_viewport(Viewport1, TabId);
                 none ->
                   Delta
               end,

        NewState = State#state { gen = Gen + 1 },
        {reply, {ok, {NewState#state.gen, Resp}}, NewState};

handle_call({apply_changes, ChangesToState}, _From, State=#state { tab_id = TabId }) ->
        Actions = changes_to_action_list(ChangesToState),
        ok = apply_changes_to_state(Actions, TabId),
        {reply, {ok, applied}, State};

handle_call({get_viewport, {Min, Max}}, _From, State=#state { tab_id = TabId }) ->
        Viewport1 = {translate(to_server, Min), translate(to_server, Max)},
        ViewportData = invalidate_viewport(Viewport1, TabId),
        {reply, {ok, ViewportData}, State};

handle_call(dump_state, _From, State=#state { tab_id = TabId }) ->
        {reply, {dumped, TabId}, State};

handle_call({restore_from_dump, DumpTabId}, _From, #state { tab_id = TabId }) ->
        true = ets:delete(TabId),
        NewState = #state { gen = 0, tab_id = DumpTabId },
        {reply, {ok, restored}, NewState};

handle_call(clear, _From, State=#state { tab_id = TabId }) ->
        true = ets:delete_all_objects(TabId),
        NewState = State#state { gen = 0 },
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
        ok = traverse_cells(WorldTabId, LookupTabId),

        ViewportDelta = apply_changes_to_state(WorldTabId, LookupTabId, Viewport),
        ets:delete(LookupTabId),
        {ok, ViewportDelta}.

traverse_cells(WorldTabId, LookupTabId) ->
        Fun = fun(Cell, Acc) ->
                traverse_cell(Cell, WorldTabId, LookupTabId),
                Acc
              end,
        ets:foldl(Fun, [], WorldTabId),
        ok.

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
        Fun = fun({Key, Action}) -> exec_action({Key, Action}, WorldTabId) end,
        lists:foreach(Fun, Actions),
        ok.

apply_changes_to_state(WorldTabId, LookupTabId, Viewport) ->
        Fun = fun(Action, ViewportDelta) ->
                ok = exec_action(Action, WorldTabId),
                add_to_viewport(Action, Viewport, ViewportDelta)
              end,
        ets:foldl(Fun, [], LookupTabId).

exec_action({Key, die}, TabId) ->
        ets:delete(TabId, Key),
        ok;

exec_action({Key, arise}, TabId) ->
        ets:insert(TabId, {Key, alive}),
        ok.

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