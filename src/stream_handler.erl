-module(stream_handler).

-author("Dmitry Kataskin").

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

init(_Transport, Req, _Opts, Active) ->
        {ok, Req, no_state}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
        io:format("ping ~p received~n", [Name]),
        {reply, <<"pong">>, Req, State};

stream(Data, Req, State) ->
        case jsx:is_json(Data) of
          true ->
            io:format("stream received valid json ~s~n", [Data]),
            {ok, Req, State};
          false ->
            io:format("stream received something ~s~n", [Data]),
            {ok, Req, State}
  end.

info(Info, Req, State) ->
        io:format("info received ~p~n", [Info]),
        {ok, Req, State}.

terminate(_Req, _State) ->
        io:format("erlchat handler terminate~n"),
        ok.