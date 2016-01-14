-module(require_login_filter).
-export([before_filter/2]).

before_filter(_FilterConfig, RequestContext) ->
    case blogger_lib:is_logged_in(RequestContext) of
        true ->
            {ok, RequestContext};
        _ ->
            {redirect, "/login"}
    end.
