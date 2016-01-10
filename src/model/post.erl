-module(post, [Id, Title::string(), Content::string(), Slug::string(),
               BloggerId, CreatedAt::datetime(), UpdatedAt::datetime()]).
-export([before_create/0, before_update/0]).

-belongs_to(blogger).

generate_slug() ->
    SlugLower = string:to_lower(Title),
    SlugNoSpace = re:replace(SlugLower, "\s+", "-", [global, {return, list}]),
    re:replace(SlugNoSpace, "[^-a-z0-9]", "", [global, {return, list}]).

before_create() ->
    Time = erlang:universaltime(),
    Post = set([{slug, generate_slug()}, {created_at, Time}, {updated_at, Time}]),
    {ok, Post}.

before_update() ->
    Post = set(updated_at, erlang:universaltime()),
    {ok, Post}.
