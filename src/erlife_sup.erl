-module(erlife_sup).
-author("Dmitry Kataskin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supvervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Store = {erlife_store, {erlife_store, start_link, []}, 5000, worker, [erlife_store]},
    {ok, {{one_for_one, 5, 10}, [Store]}}.

