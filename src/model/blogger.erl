-module(blogger, [Id, Username::string(), PasswordHash::string(),
                  CreatedAt::datetime(), UpdatedAt::datetime()]).
-export([before_create/0, before_update/0, check_password/1, set_password/1,
         validation_tests/0]).

-has({posts, many}).
-has({twitter_token, 1}).

check_password(Password) ->
    {ok, PasswordHash} =:= bcrypt:hashpw(Password, PasswordHash).

set_password(Password) ->
    if
        length(Password) < 6 ->
            {error, ["Password must be at least 6 characters long"]};
        true ->
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, Hash} = bcrypt:hashpw(Password, Salt),
            Blogger = set(password_hash, Hash),
            {ok, Blogger}
    end.

before_create() ->
    Time = erlang:universaltime(),
    Blogger = set([{created_at, Time}, {updated_at, Time}]),
    {ok, Blogger}.

before_update() ->
    Blogger = set(updated_at, erlang:universaltime()),
    {ok, Blogger}.

validation_tests() ->
    [
        {fun() ->
            (length(Username) >= 1) and (length(Username) =< 30)
         end,
         "Username must be between 1 and 30 characters long"},
        {fun() ->
             boss_db:count(blogger, [username, equals, Username]) =:= 0
         end,
         "Username already taken"}
    ].
