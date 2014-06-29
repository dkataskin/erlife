%%%-------------------------------------------------------------------
%%% @author kotoff
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2014 19:27
%%%-------------------------------------------------------------------
-module(erlife_utils).
-author("kotoff").

%% API
-export([priv_dir/0]).

priv_dir() ->
        Ebin = filename:dirname(code:which(erlife)),
        filename:join(filename:dirname(Ebin), "priv").