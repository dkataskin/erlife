-module(erlife_sup).
-author("Dmitry Kataskin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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

