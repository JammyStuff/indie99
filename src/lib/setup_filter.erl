-module(setup_filter).
-export([before_filter/2]).

before_filter(_FilterConfig, RequestContext) ->
    case boss_db:count(blogger) of
        0 ->
            {redirect, "/setup/step1"};
        _ ->
            {ok, RequestContext}
    end.
