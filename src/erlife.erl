-module(erlife).
-author("Dmitry Kataskin").

-define(apps, [crypto, inets, ranch, cowlib, cowboy, gproc, sync, erlife]).
-author("Dmitry Kataskin").

-export([start/0, stop/0]).

start() ->
        all_started = ensure_started(?apps),
        ok.

stop() ->
        all_stopped = ensure_stopped(lists:reverse(?apps)),
        ok.

ensure_started([]) ->
        all_started;

ensure_started([App|RestOfAppList]) ->
        case application:start(App) of
          ok -> started;
          {error, {already_started, App}} -> started
        end,
        ensure_started(RestOfAppList).

ensure_stopped([]) ->
        all_stopped;

ensure_stopped([App|RestOfAppList]) ->
        case application:stop(App) of
          ok -> stopped;
          {error, {not_started, App}} -> stopped
        end,
        ensure_stopped(RestOfAppList).