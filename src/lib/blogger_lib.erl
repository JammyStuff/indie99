-module(blogger_lib).
-export([is_logged_in/1]).

is_logged_in(RequestContext) ->
    proplists:get_value(blogger, RequestContext) =/= undefined.
