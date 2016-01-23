-module(twitter_token, [Id, Token::string(), TokenSecret::string(), BloggerId,
                        CreatedAt::datetime(), UpdatedAt::datetime()]).
-export([before_create/0, before_update/0]).

-belongs_to(blogger).

before_create() ->
    Time = erlang:universaltime(),
    TwitterToken = set([{created_at, Time}, {updated_at, Time}]),
    {ok, TwitterToken}.

before_update() ->
    TwitterToken = set(updated_at, erlang:universaltime()),
    {ok, TwitterToken}.
