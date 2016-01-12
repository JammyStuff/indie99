-module(blogger_tests).
-include_lib("eunit/include/eunit.hrl").

blogger_test_() ->
    [
        {"check_password function",
         ?_test(fun_check_password())},
        {"set_password function",
         ?_test(fun_set_password())},
        {"Validations for Username",
         ?_test(val_username())}
    ].

fun_check_password() ->
    Blogger = create_blogger(),
    ?assert(Blogger:check_password("testpassword123")),
    ?assertNot(Blogger:check_password("notthepasword")).

fun_set_password() ->
    OrigBlogger = create_blogger(),
    Password = "anewtestpass123",
    {ok, NewBlogger} = OrigBlogger:set_password(Password),
    ?assertNotEqual(OrigBlogger:password_hash(), NewBlogger:password_hash()),
    ?assertNotEqual(undefined, NewBlogger:password_hash()),
    ?assertNotEqual(Password, NewBlogger:password_hash()),
    ?assertEqual(60, length(NewBlogger:password_hash())),
    ?assertEqual({error, ["Password must be at least 6 characters long"]},
                 OrigBlogger:set_password("abcde")).

val_username() ->
    TooShortBlogger = create_blogger([{username, ""}]),
    ?assertEqual({error, ["Username must be between 1 and 30 characters long"]},
                 TooShortBlogger:validate()),
    TooLongBlogger = create_blogger([
        {username, "abcdefghijklmnopqrstuvwxyzabcde"}]),
    ?assertEqual({error, ["Username must be between 1 and 30 characters long"]},
                 TooShortBlogger:validate()),
    Bloggers = boss_db:find(blogger, [{username, equals, "Username"}], []),
    lists:foreach(fun(B) -> boss_db:delete(B:id()) end, Bloggers),
    DuplicateBlogger = create_blogger([{username, "Username"}]),
    ?assertMatch({ok, _}, DuplicateBlogger:save()),
    ?assertEqual({error, ["Username already taken"]},
                 DuplicateBlogger:validate()).

create_blogger() ->
    blogger:new(id, "Username",
                "$2a$12$.Thg9f90gPSFkmTR6JDcVeG0/mg8BYte8q92v6PcEFMCe0cfcR1/C",
                undefined, undefined).

create_blogger(AttrVals) ->
    Blogger = create_blogger(),
    Blogger:set(AttrVals).
