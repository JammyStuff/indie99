-module(rel_me_filter).
-export([middle_filter/3]).

middle_filter({render, Variables, Headers}, _FilterConfig, _RequestContext) ->
    {render, middle_filter_variables(Variables), Headers};
middle_filter({render_other, Location, Variables, Headers},
              _FilterConfig,
              _RequestContext) ->
    {render_other,
     Location,
     middle_filter_variables(Variables),
     Headers};
middle_filter(Other, _FilterConfig, _RequestContext) ->
    Other.

middle_filter_variables(Variables) ->
    case boss_db:find_first(blogger) of
        undefined ->
            Variables;
        Blogger ->
            NewVariables1 = proplists:delete(site_email_address, Variables),
            NewVariables2 = proplists:delete(site_twitter_username,
                                             NewVariables1),
            NewVariables3 = proplists:delete(site_username, NewVariables2),
            NewVariables4 = proplists:delete(site_full_name, NewVariables3),
            [
                {site_email_address, Blogger:email_address()},
                {site_twitter_username, Blogger:twitter_username()},
                {site_username, Blogger:username()},
                {site_full_name, Blogger:full_name()}
            ] ++ NewVariables4
    end.
