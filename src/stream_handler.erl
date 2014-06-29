-module(stream_handler).

-author("Dmitry Kataskin").

-include("erlife.hrl").

-record(stream_state, { running = false,
                        sessionId = undefined }).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
        {SessionId, Req1} = cowboy_req:cookie(?session_id_cookie, Req),
        {ok, Req1, #stream_state{ sessionId = SessionId }}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
        io:format("ping ~p received~n", [Name]),
        {reply, <<"pong">>, Req, State};

stream(Data, Req, State) ->
        case jsx:is_json(Data) of
          true ->
            %io:format("stream received valid json ~s~n", [Data]),
            Json = jsx:decode(Data),
            execute_command(Json, Req, State);

          false ->
            io:format("stream received something ~s~n", [Data]),
            {ok, Req, State}
        end.

info(Info, Req, State) ->
        io:format("info received ~p~n", [Info]),
        {ok, Req, State}.

terminate(_Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("erlife handler terminate~n"),
        ok.

execute_command([{<<"command">>, Command} | T], Req, State) ->
        execute_command(Command, T, Req, State).

execute_command(<<"nextGen">>, _, Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: nextGen~n"),
        Fun = fun(Pid) ->
                {ok, GenNum, Delta} = erlife_engine:next_gen(Pid),
                get_delta_json(GenNum, Delta)
              end,
        Resp = execute_on_server(SessionId, Fun),
        reply(Resp, Req, State);

execute_command(<<"start">>, Data, Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: start; data:~p~n", [Data]),
        {ok, _Pid} = start_engine(Data, SessionId),
        {ok, Req, State};

execute_command(<<"stop">>, _, Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: stop~n"),
        ok = stop_engine(SessionId),
        {ok, Req, State};

execute_command(Command, _, Req, State) ->
        io:format("unknown command ~p received ~n", [Command]),
        {ok, Req, State}.

get_delta_json(GenNum, Delta) ->
        jsx:encode([{<<"gen">>, GenNum}, {<<"delta">>, compact_delta(Delta)}]).

compact_delta(Delta) ->
        Fun = fun({Action, <<X:64, Y:64>>}) ->
                case Action of
                  die ->
                    [false, X, Y];
                  arise ->
                    [true, X, Y]
                end
              end,
        lists:map(Fun, Delta).

execute_on_server(SessionId, Fun) ->
        case gproc:lookup_local_name(SessionId) of
          undefined ->
            {ok, no_reply};
          Pid ->
            Fun(Pid)
        end.

reply({ok, no_reply}, Req, State) ->
        {ok, Req, State};

reply(Response, Req, State) ->
        {reply, Response, Req, State}.

start_engine(SessionId, InitialState) ->
        EnginePid = case gproc:lookup_local_name(SessionId) of
                      undefined ->
                        {ok, Pid1} = erlife_engine:start_link(SessionId, InitialState),
                        Pid1;

                      ProcessPid ->
                        ProcessPid
                    end,
        {ok, EnginePid}.

stop_engine(SessionId) ->
        case gproc:lookup_local_name(SessionId) of
          undefined ->
            ok;

          Pid ->
            erlife_engine:stop(Pid),
            ok
        end.