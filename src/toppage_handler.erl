-module(toppage_handler).
-author("Dmitry Kataskin").

-include("erlife.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
        {ok, HTML} = main_dtl:render([]),

        SessionId = erlife_utils:generate_uuid(),
        Req1 = cowboy_req:set_resp_cookie(?session_id_cookie, SessionId, [], Req),

        {ok, Req2} = cowboy_req:reply(200, [], HTML, Req1),
        {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
        ok.