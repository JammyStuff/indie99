-module(blogger, [Id, Username::string(), PasswordHash::string(),
                  EmailAddress::string(), TwitterUsername::string(),
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
            if
                is_list(EmailAddress) ->
                    length(EmailAddress) =< 254;
                true ->
                    true
            end
         end,
         "Email address can be up to 254 characters long"},
        {fun() ->
            if
                is_list(TwitterUsername) ->
                    length(TwitterUsername) =< 15;
                true ->
                    true
            end
         end,
         "Twitter Username can be up to 15 characters long"},
        {fun() ->
            (length(Username) >= 1) and (length(Username) =< 30)
         end,
         "Username must be between 1 and 30 characters long"},
        {fun() ->
            case Id of
                id ->
                    boss_db:count(blogger, [username, equals, Username]) =:= 0;
                _ ->
                    true
            end
         end,
         "Username already taken"}
    ].
