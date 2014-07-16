-module(erlife_qtree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type rect() :: {point(), point()}.

-record(qtree, { rect = undefined :: rect(), is_leaf = false :: boolean(), nodes = [] :: [qtree()] }).
-type qtree() :: #qtree{}.

-export_type([point/0, rect/0, qtree/0]).

%% API
-export([new/1]).

-spec new(Rect::rect()) -> {ok, qtree()}.
new(Rect) -> ok.