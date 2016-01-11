-module(blogger_filter).
-export([before_filter/2]).

before_filter(_FilterConfig, RequestContext) ->
    SessionID = proplists:get_value(session_id, RequestContext),
    BloggerID = boss_session:get_session_data(SessionID, blogger),
    Blogger = boss_db:find(BloggerID),
    case lists:keymember(blogger, 1, [Blogger]) of
        true ->
            {ok, [{blogger, Blogger} | RequestContext]};
        _ ->
            {ok, RequestContext}
    end.
