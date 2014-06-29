-module(erlife_store).
-author("Dmitry Kataskin").

-include("erlife.hrl").

-define(dumps_table, erlife_dumps).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([save/2, update/3, load/2, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
        gen_server:start_link({local, ?erlife_store}, ?MODULE, [], []).

save(Name, TabId) ->
        gen_server:call(?erlife_store, {save, {Name, TabId}}).

update(Id, Name, TabId) ->
        gen_server:call(?erlife_store, {update, Id, Name, TabId}).

load(Id, Pid) ->
        gen_server:call(?erlife_store, {load, Id, Pid}).

list() ->
        gen_server:call(?erlife_store, list).

%% gen_server callbacks
init([]) ->
        FileName = filename:join(erlife_utils:priv_dir(), "dumps.tbl"),
        {ok, _} = dets:open_file(?dumps_table, [{file, FileName}]),
        {ok, no_state}.

handle_call({save, Name, TabId}, _From, State) ->
        Id = erlife_utils:generate_uuid(),
        ets:tab2file(TabId, get_filename(Id)),
        dets:insert(?dumps_table, {Id, Name}),
        {reply, ok, State};

handle_call({update, Id, Name, TabId}, _From, State) ->
        ets:tab2file(TabId, get_filename(Id)),
        dets:insert(?dumps_table, {Id, Name}),
        {reply, ok, State};

handle_call({load, Id, Pid}, _From, State) ->
        {ok, TabId} = ets:file2tab(get_filename(Id)),
        true = ets:give_away(TabId, Pid, enjoy),
        {reply, {ok, TabId}, State};

handle_call(list, _From, State) ->
        List = dets:foldl(fun(Elem, Acc) -> [Elem | Acc] end, [], ?dumps_table),
        {reply, {ok, List}, State};

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

get_filename(Id) when is_binary(Id) ->
        filename:join(erlife_utils:dumps_dir(), binary_to_list(Id) ++ ".dmp").