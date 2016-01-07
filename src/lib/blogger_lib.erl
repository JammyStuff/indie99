-module(blogger_lib).
-export([is_logged_in/1, register/2]).

is_logged_in(SessionID) ->
    UserID = boss_session:get_session_data(SessionID, user),
    case boss_db:find(UserID) of
        {error, _} ->
            false;
        User ->
            User =/= undefined
    end.

register(Username, Password) ->
    Time = erlang:universaltime(),
    Blogger = blogger:new(id, Username, "", Time, Time),
    BloggerHashed = Blogger:set_password(Password),
    BloggerHashed:save().
