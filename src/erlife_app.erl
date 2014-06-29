-module(erlife_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
        Dispatch = cowboy_router:compile([
            {'_', [
              static_files("js"),
              static_files("css"),
              static_files("img"),
              {"/", toppage_handler, []},
              {"/erlife", bullet_handler, [{handler, stream_handler}]}
            ]}
          ]),
          {ok, _} = cowboy:start_http(http, 100,
            [{port, 8085}], [{env, [{dispatch, Dispatch}]}]
          ),
          erlife_sup:start_link().

stop(_State) ->
        ok.

static_files(FileType) ->
        {lists:append(["/", FileType, "/[...]"]), cowboy_static,
          {dir, static_content_dir(FileType), [{mimetypes, cow_mimetypes, web}]}}.

static_content_dir(FileType) ->
        filename:join(erlife_utils:priv_dir(), FileType).