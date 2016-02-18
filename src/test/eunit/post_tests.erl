-module(post_tests).
-include_lib("eunit/include/eunit.hrl").

post_test_() ->
    [
        {"generate_slug function",
         ?_test(fun_generate_slug())},
        {"Post without content",
         ?_test(no_content())},
        {"Validations for title",
         ?_test(val_title())},
        {"Validations for twitter username",
         ?_test(val_twitter_username())}
    ].

fun_generate_slug() ->
    Post = create_post(undefined, [{title, "Title Test To Slug"}]),
    ?assertEqual("title-test-to-slug", Post:generate_slug()),
    PunctuationPost = create_post(undefined, [{title, "Don't fail 123"}]),
    ?assertEqual("dont-fail-123", PunctuationPost:generate_slug()),
    DuplicateTitle = "Duplicate post title",
    Posts = boss_db:find(post, [title, equals, DuplicateTitle], []),
    lists:foreach(fun(P) -> boss_db:delete(P:id()) end, Posts),
    DuplicatePost1 = create_post_with_blogger([{title, DuplicateTitle}]),
    ?assertMatch({ok, _}, DuplicatePost1:save()),
    ?assertEqual("duplicate-post-title-1", DuplicatePost1:generate_slug()),
    DuplicatePost2 = create_post(DuplicatePost1:blogger(),
                                 [{title, DuplicateTitle}]),
    ?assertMatch({ok, _}, DuplicatePost2:save()),
    ?assertEqual("duplicate-post-title-2", DuplicatePost2:generate_slug()).

no_content() ->
    Post = create_post_with_blogger([{content, undefined}]),
    ?assertMatch({ok, _}, Post:save()).

val_title() ->
    TooShortPost = create_post(undefined, [{title, ""}]),
    ?assertEqual({error, ["Title must be between 1 and 116 characters long"]},
                 TooShortPost:validate()),
    TooLongPost = create_post(undefined, [{title, "This post title is too " ++
        "long to be stored in the database and should cause a validation" ++
        " failure. Should fail if not..."}]),
    ?assertEqual(117, length(TooLongPost:title())),
    ?assertEqual({error, ["Title must be between 1 and 116 characters long"]},
                 TooLongPost:validate()).

val_twitter_username() ->
    OkPost = create_post(undefined, [{twitter_username, "TwitterUsername"}]),
    ?assertEqual(ok, OkPost:validate()),
    TooLongPost = create_post(undefined,
                              [{twitter_username, "TwitterUsername1"}]),
    ?assertEqual({error, ["Twitter Username can be up to 15 characters long"]},
                 TooLongPost:validate()).

create_post(Blogger) ->
    Post = post:new(id, "Test post please ignore", "This is a test post. It " ++
                    "can be used in eunit tests.", undefined, undefined,
                    undefined, undefined, undefined, undefined, undefined,
                    undefined, undefined, undefined),
    case Blogger of
        undefined ->
            Post;
        _ ->
            Post:set(blogger_id, Blogger:id())
    end.

create_post(Blogger, AttrVals) ->
    Post = create_post(Blogger),
    Post:set(AttrVals).

create_post_with_blogger() ->
    Username = "PostTestUser",
    Bloggers = boss_db:find(blogger, [username, equals, Username], []),
    lists:foreach(fun(B) -> boss_db:delete(B:id()) end, Bloggers),
    Blogger = blogger_tests:create_blogger([{username, Username}]),
    {ok, SavedBlogger} = Blogger:save(),
    create_post(SavedBlogger).

create_post_with_blogger(AttrVals) ->
    Post = create_post_with_blogger(),
    Post:set(AttrVals).
