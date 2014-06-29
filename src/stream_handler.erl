-module(stream_handler).
-author("Dmitry Kataskin").

-include("erlife.hrl").

-record(stream_state, { sessionId = undefined }).

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
        %io:format("user commanded: nextGen, data:~p~n", [Data]),
        {ok, Viewport, Options} = erlife_protocol:parse_nextgen_input(Data),
        Fun = fun(Pid) ->
                {ok, {GenNum, Delta}} = erlife_engine:next_gen(Pid, Viewport, Options),
                erlife_protocol:gen_delta_to_json(GenNum, Delta)
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

execute_command(<<"save">>, [{<<"data">>, Data}], Req, State=#stream_state{ sessionId = SessionId }) ->
        Name = erlife_protocol:parse_save_input(Data),
        Fun = fun(Pid) ->
                {dumped, TabId} = erlife_engine:dump_state(Pid),
                {saved, _Id} = erlife_store:save(Name, TabId),
                {ok, DumpList} = erlife_store:list(),
                erlife_protocol:dump_list_to_json(DumpList)
              end,
        Resp = execute_on_server(SessionId, Fun),
        reply(Resp, Req, State);

execute_command(<<"load">>, [{<<"data">>, Data}], Req, State=#stream_state{ sessionId = SessionId }) ->
        {Id, Viewport} = erlife_protocol:parse_load_input(Data),
        Fun = fun(Pid) ->
                {loaded, TabId} = erlife_store:load(Id, Pid),
                {ok, restored} = erlife_engine:restore_from_dump(Pid, TabId),
                {ok, ViewportData} = erlife_engine:get_viewport(Pid, Viewport),
                erlife_protocol:gen_delta_to_json(0, ViewportData)
              end,
        Resp = execute_on_server(SessionId, Fun),
        reply(Resp, Req, State);

execute_command(Command, _, Req, State) ->
        io:format("unknown command ~p received ~n", [Command]),
        {ok, Req, State}.

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
        ok = case gproc:lookup_local_name(SessionId) of
              undefined ->
                ok;

              ProcessPid ->
                erlife_engine:stop(ProcessPid),
                ok
             end,
        erlife_engine:start_link(SessionId, InitialState).

stop_engine(SessionId) ->
        case gproc:lookup_local_name(SessionId) of
          undefined ->
            ok;

          Pid ->
            erlife_engine:stop(Pid),
            ok
        end.