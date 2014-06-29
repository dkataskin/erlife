-module(toppage_handler).
-author("Dmitry Kataskin").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
        {ok, HTML} = main_dtl:render([]),
        {ok, Req1} = cowboy_req:reply(200, [], HTML, Req),
        {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
        ok.