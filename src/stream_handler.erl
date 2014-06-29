-module(stream_handler).

-author("Dmitry Kataskin").

-record(stream_state, { running = false,
                        pid = undefined,
                        sessionId = undefined }).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
        {ok, Req, #stream_state{}}.

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

terminate(_Req, _State) ->
        io:format("erlife handler terminate~n"),
        ok.

execute_command([{<<"command">>, Command} | T], Req, State) ->
        execute_command(Command, T, Req, State).

execute_command(<<"nextGen">>, _, Req, State=#stream_state{ pid = Pid }) ->
        io:format("user commanded: nextGen~n"),
        {ok, GenNum, Delta} = erlife_engine:next_gen(Pid),
        {reply, get_delta_json(GenNum, Delta), Req, State};

execute_command(<<"start">>, Data, Req, State=#stream_state{}) ->
        io:format("user commanded: start; data:~p~n", [Data]),
        {ok, Req, State};

execute_command(<<"stop">>, _, Req, State) ->
        io:format("user commanded: stop~n"),
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

