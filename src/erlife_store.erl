-module(erlife_store).
-author("Dmitry Kataskin").

-include("erlife.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
        gen_server:start_link({local, ?erlife_store}, ?MODULE, [], []).

init([]) ->
        {ok, no_state}.

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