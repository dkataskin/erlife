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
            execute_command(jsx:decode(Data), Req, State);

          false ->
            io:format("stream received something ~s~n", [Data]),
            {ok, Req, State}
        end.

info(Info, Req, State) ->
        io:format("info received ~p~n", [Info]),
        {ok, Req, State}.

terminate(_Req, #stream_state{ sessionId = SessionId }) ->
        io:format("erlife handler terminate~n"),
        ok = stop_engine(SessionId),
        ok.

execute_command([{<<"command">>, Command} | T], Req, State) ->
        execute_command(Command, T, Req, State).

execute_command(<<"nextGen">>, [{<<"data">>, Data}], Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: nextGen, data:~p~n", [Data]),
        {ok, Viewport, Options} = get_nextgen_input(Data),
        Fun = fun(Pid) ->
                {ok, GenNum, Delta} = erlife_engine:next_gen(Pid, Viewport, Options),
                get_delta_json(GenNum, Delta)
              end,
        Resp = execute_on_server(SessionId, Fun),
        reply(Resp, Req, State);

execute_command(<<"start">>, [{<<"data">>, Data}], Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: start; data:~p~n", [Data]),
        InitialState = erlife_protocol:parse_initial_input(Data),
        {ok, _Pid} = start_engine(SessionId, InitialState),
        {ok, Req, State};

execute_command(<<"stop">>, _, Req, State=#stream_state{ sessionId = SessionId }) ->
        io:format("user commanded: stop~n"),
        ok = stop_engine(SessionId),
        {ok, Req, State};

execute_command(Command, _, Req, State) ->
        io:format("unknown command ~p received ~n", [Command]),
        {ok, Req, State}.

get_delta_json(GenNum, Delta) ->
        jsx:encode([{<<"gen">>, GenNum}, {<<"delta">>, erlang_protocol:prepare_delta(Delta)}]).

get_nextgen_input([{<<"viewport">>, [MinX, MinY, MaxX, MaxY]}, {<<"invalidate">>, Invalidate}]) ->
        Viewport = {{MinX, MinY}, {MaxX, MaxY}},
        case Invalidate of
          true ->
            {ok, Viewport, [invalidate]};
          false ->
            {ok, Viewport, []}
        end;

get_nextgen_input(_) ->
        {ok, {{-50, -50}, {50, 50}}, []}.

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