-module(blogger, [Id, Username::string(), PasswordHash::string(),
                  CreatedAt::datetime(), UpdatedAt::datetime()]).
-export([before_update/0, check_password/1, set_password/1]).

check_password(Password) ->
    {ok, PasswordHash} =:= bcrypt:hashpw(Password, PasswordHash).

set_password(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    set(password_hash, Hash).

before_update() ->
    Blogger = set(updated_at, erlang:universaltime()),
    {ok, Blogger}.
