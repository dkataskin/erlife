-module(erlife_qtree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type rect() :: {point(), point()}.

-record(qtree, { rect = undefined :: rect(), is_leaf = false :: boolean(), nodes = [] :: [qtree()] }).
-type qtree() :: #qtree{}.

-export_type([point/0, rect/0, qtree/0]).

%% API
-export([new/1, add_point/2]).

-spec new(Rect::rect()) -> {ok, qtree()}.
new(Rect) ->
        {ok, #qtree { rect = Rect }}.

-spec add_point(QTree::qtree(), Point::point()) -> {ok, qtree()}.
add_point(QTree = #qtree{}, Point) ->
        {ok, QTree}.
