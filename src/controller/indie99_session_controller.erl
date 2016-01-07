-module(indie99_session_controller, [Req, SessionID]).
-export([login/2, logout/2]).

login('GET', []) ->
    case blogger_lib:is_logged_in(SessionID) of
        true ->
            {redirect, "/"};
        _ ->
        {ok, [{title, "Login"}]}
    end;

login('POST', []) ->
    Username = Req:post_param("username"),
    Password = Req:post_param("password"),
    case boss_db:find_first(blogger, [username, equals, Username]) of
        undefined ->
            {ok, [{title, "Login"},
                  {login_error, "Invalid username or password"}]};
        User ->
            case User:check_password(Password) of
                true ->
                    boss_session:set_session_data(SessionID, user, User:id()),
                    {redirect, "/"};
                _ ->
                    {ok, [{title, "Login"},
                          {login_error, "Invalid username or password"}]}
            end
    end.

logout('POST', []) ->
    boss_session:remove_session_data(SessionID, user),
    {redirect, "/"}.
