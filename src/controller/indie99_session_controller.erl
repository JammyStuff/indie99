-module(indie99_session_controller, [Req, SessionID]).
-export([login/3, logout/2]).

login('GET', [], RequestContext) ->
    case blogger_lib:is_logged_in(RequestContext) of
        true ->
            {redirect, "/"};
        _ ->
        {ok, [{title, "Login"}]}
    end;

login('POST', [], RequestContext) ->
    Username = Req:post_param("username"),
    Password = Req:post_param("password"),
    case boss_db:find_first(blogger, [username, equals, Username]) of
        undefined ->
            {ok, [{title, "Login"},
                  {login_error, "Invalid username or password"}]};
        Blogger ->
            case Blogger:check_password(Password) of
                true ->
                    boss_session:set_session_data(SessionID, blogger, Blogger:id()),
                    {redirect, "/"};
                _ ->
                    {ok, [{title, "Login"},
                          {login_error, "Invalid username or password"}]}
            end
    end.

logout('POST', []) ->
    boss_session:remove_session_data(SessionID, blogger),
    {redirect, "/"}.
