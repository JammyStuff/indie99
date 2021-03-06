-module(post, [Id, Title::string(), Content::string(), Slug::string(),
               Latitude::float(), Longitude::float(), TwitterStatusId::string(),
               TwitterUsername::string(), BloggerId, ImageId,
               PublishedAt::datetime(), CreatedAt::datetime(),
               UpdatedAt::datetime()]).
-export([before_create/0, before_update/0, generate_slug/0,
         validation_tests/0]).

-belongs_to(blogger).
-belongs_to_uploaded_file(image).

generate_slug() ->
    SlugLower = string:to_lower(Title),
    SlugNoSpace = re:replace(SlugLower, "\s+", "-", [global, {return, list}]),
    SlugPrefix = re:replace(SlugNoSpace, "[^-a-z0-9]", "", [global, {return, list}]),
    generate_slug(SlugPrefix, 0).

generate_slug(SlugPrefix, N) ->
    CandidateSlug = case N of
        0 ->
            SlugPrefix;
        _ ->
            SlugPrefix ++ "-" ++ integer_to_list(N)
    end,
    case boss_db:count(post, [slug, equals, CandidateSlug]) of
        0 ->
            CandidateSlug;
        _ ->
            generate_slug(SlugPrefix, N + 1)
    end.

before_create() ->
    Time = erlang:universaltime(),
    Post = set([{slug, generate_slug()}, {created_at, Time}, {updated_at, Time}]),
    {ok, Post}.

before_update() ->
    Post = set(updated_at, erlang:universaltime()),
    {ok, Post}.

validation_tests() ->
    [
        {fun() ->
            (length(Title) >= 1) and (length(Title) =< 116)
         end,
         "Title must be between 1 and 116 characters long"},
        {fun() ->
            if
                is_list(TwitterUsername) ->
                    length(TwitterUsername) =< 15;
                true ->
                    true
            end
         end,
         "Twitter Username can be up to 15 characters long"}
    ].
