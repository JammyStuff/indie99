-module(blogger_filter).
-export([before_filter/2, middle_filter/3]).

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

middle_filter({render, Variables, Headers}, _FilterConfig, RequestContext) ->
    {render, middle_filter_variables(Variables, RequestContext), Headers};
middle_filter({render_other, Location, Variables, Headers},
              _FilterConfig,
              RequestContext) ->
    {render_other,
     Location,
     middle_filter_variables(Variables, RequestContext),
     Headers};
middle_filter(Other, _FilterConfig, _RequestContext) ->
    Other.

middle_filter_variables(Variables, RequestContext) ->
    case proplists:get_value(blogger, RequestContext) of
        undefined ->
            Variables;
        Blogger ->
            NewVaribles = proplists:delete(blogger, Variables),
            [{blogger, Blogger} | NewVaribles]
    end.

