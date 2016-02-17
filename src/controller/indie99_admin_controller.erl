-module(indie99_admin_controller, [Req, SessionID]).
-export([before_filters/2, posts/3, profile/3, services/3]).

-define(POSTS_PER_PAGE, 10).

posts('GET', [], _RequestContext) ->
    {action_other, [{action, "posts"}, {page_number_str, "1"}]};

posts('GET', ["create"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    TwitterIsConnected = (twitter_client:is_enabled()) and
        (Blogger:twitter_token() =/= undefined),
    {render_other, [{action, "posts_create"}],
        [{title, "Create Post"},
         {twitter_is_connected, TwitterIsConnected}]};

posts('POST', ["create"], RequestContext) ->
    Publish = Req:post_param("post") =:= "post",
    Blogger = proplists:get_value(blogger, RequestContext),
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    PublishedAt = case Publish of
        true ->
            erlang:universaltime();
        false ->
            undefined
    end,
    Post = post:new(id, Title, Content, undefined, undefined, undefined,
                    Blogger:id(), undefined, PublishedAt, undefined, undefined),
    case Post:save() of
        {error, Errors} ->
            {render_other, [{action, "posts_create"}],
                           [{title, "Create Post"}, {form_errors, Errors}]};
        {ok, SavedPost} ->
            case Publish of
                true ->
                    PostUrl = io_lib:format("~s://~s/p/~s", [Req:protocol(),
                        Req:header(host), SavedPost:slug()]),
                    case Req:post_param("twitter") of
                        "twitter" ->
                            posse_post(Blogger, SavedPost, PostUrl,
                                       [{twitter, true}]);
                        _ ->
                            ok
                    end,
                    {redirect, io_lib:format("/p/~s", [SavedPost:slug()])};
                false ->
                    {redirect, io_lib:format("/admin/posts/~s/preview",
                        [SavedPost:id()])}
            end
    end;

posts('GET', ["page", PageNumberStr], _RequestContext) ->
    PageNumber = list_to_integer(PageNumberStr),
    Offset = (PageNumber - 1) * ?POSTS_PER_PAGE,
    Posts = boss_db:find(post, [],
                               [{order_by, created_at},
                                {descending, true},
                                {limit, ?POSTS_PER_PAGE},
                                {offset, Offset}]),
    PostsCount = boss_db:count(post),
    PreviousPage = PageNumber - 1,
    NextPage = if
                   PostsCount > (PageNumber * ?POSTS_PER_PAGE) ->
                       PageNumber + 1;
                   true ->
                       undefined
               end,
    {ok, [{title, "Posts Admin"}, {posts, Posts}, {previous_page, PreviousPage},
          {next_page, NextPage}]};

posts('GET', [PostId], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    TwitterIsConnected = (twitter_client:is_enabled()) and
        (Blogger:twitter_token() =/= undefined),
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{action, "posts_edit"}],
                           [{title, "Edit Post"}, {post, Post},
                            {twitter_is_connected, TwitterIsConnected}]};
        _ ->
            not_found
    end;

posts('POST', [PostId], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    Publish = Req:post_param("publish") =:= "publish",
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            UpdatedPost = case Publish of
                true ->
                    Post:set([{title, Title}, {content, Content},
                              {published_at, erlang:universaltime()}]);
                false ->
                    Post:set([{title, Title}, {content, Content}])
            end,
            case UpdatedPost:save() of
                {error, Errors} ->
                    {render_other, [{action, "posts_edit"}],
                                   [{title, "Edit Post"},
                                    {post, Post},
                                    {form_errors, Errors}]};
                {ok, SavedPost} ->
                    case SavedPost:published_at() of
                        undefined ->
                            {redirect, io_lib:format("/admin/posts/~s/preview",
                                [SavedPost:id()])};
                        _ ->
                            PostUrl = io_lib:format("~s://~s/p/~s", [
                                Req:protocol(), Req:header(host),
                                SavedPost:slug()]),
                            posse_post(Blogger, SavedPost, PostUrl,
                                       [{twitter, true}]),
                            {redirect, io_lib:format("/p/~s",
                                [SavedPost:slug()])}
                    end
            end;
        _ ->
            not_found
    end;

posts('GET', [PostId, "delete"], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{action, "posts_delete"}],
                           [{title, "Delete Post"}]};
        _ ->
            not_found
    end;

posts('POST', [PostId, "delete"], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            ok = boss_db:delete(PostId),
            {redirect, "/admin/posts"};
        _ ->
            not_found
    end;

posts('GET', [PostId, "preview"], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{controller, "posts"}, {action, "view"}],
                [{title, "Preview Post"}, {post, Post}]};
        _ ->
            not_found
    end.

profile('GET', [], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    {ok, [{title, "Profile Admin"}, {form_blogger, Blogger}]};

profile('POST', [], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    FullName = Req:post_param("full_name"),
    EmailAddress = Req:post_param("email_address"),
    TwitterUsername = Req:post_param("twitter_username"),
    UpdatedBlogger = Blogger:set([
        {full_name, FullName},
        {email_address, EmailAddress},
        {twitter_username, TwitterUsername}
    ]),
    case UpdatedBlogger:save() of
        {ok, SavedBlogger} ->
            {ok, [{title, "Profile Admin"}, {form_blogger, SavedBlogger},
                  {updated, true}]};
        {error, Errors} ->
            {ok, [{title, "Profile Admin"}, {form_blogger, UpdatedBlogger},
                   {form_errors, Errors}]}
    end.

services('GET', [], _RequestContext) ->
    TwitterEnabled = twitter_client:is_enabled(),
    {ok, [{title, "Services Admin"}, {twitter_enabled, TwitterEnabled}]};

services('GET', ["twitter"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    IsConnected = Blogger:twitter_token() =/= undefined,
    {render_other, [{action, "services_twitter"}],
                   [{title, "Twitter"}, {twitter_connected, IsConnected}]};

services('POST', ["twitter"], _RequestContext) ->
    Credentials = twitter_client:get_credentials(),
    CallbackUrl = io_lib:format("~s://~s/admin/services/twitter/callback",
                                [Req:protocol(), Req:header(host)]),
    {RequestToken, RequestTokenSecret} = twitter_client:get_request_token(
        Credentials, CallbackUrl),
    boss_session:set_session_data(SessionID, twitter_request_token_secret,
        RequestTokenSecret),
    AuthorizeUrl = twitter_client:get_authorize_url(
        {RequestToken, RequestTokenSecret}),
    {redirect, AuthorizeUrl};

services('GET', ["twitter", "callback"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    case Blogger:twitter_token() of
        undefined ->
            Credentials = twitter_client:get_credentials(),
            RequestToken = Req:query_param("oauth_token"),
            RequestTokenSecret = boss_session:get_session_data(SessionID,
                twitter_request_token_secret),
            Verifier = Req:query_param("oauth_verifier"),
            {AccessToken, AccessTokenSecret} = twitter_client:get_access_token(
                Credentials, {RequestToken, RequestTokenSecret}, Verifier),
            TwitterToken = twitter_token:new(id, AccessToken, AccessTokenSecret,
                Blogger:id(), undefined, undefined),
            {ok, SavedTwitterToken} = TwitterToken:save(),
            boss_session:remove_session_data(SessionID,
                twitter_request_token_secret)
    end,
    {redirect, "/admin/services/twitter"};

services('POST', ["twitter", "disconnect"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    case Blogger:twitter_token() of
        TwitterToken when TwitterToken =/= undefined ->
            boss_db:delete(TwitterToken:id())
    end,
    {redirect, "/admin/services/twitter"}.

posse_post(Blogger, Post, PostUrl, Services) ->
    case proplists:get_value(twitter, Services) of
        true ->
            TwitterToken = Blogger:twitter_token(),
            TwitterStatus = io_lib:format("~s ~s", [Post:title(), PostUrl]),
            {TwitterStatusId, TwitterUsername} =
                twitter_client:post_status_update(
                    twitter_client:get_credentials(),
                    {TwitterToken:token(), TwitterToken:token_secret()},
                    TwitterStatus
                ),
            TweetedPost = Post:set([
                {twitter_status_id, TwitterStatusId},
                {twitter_username, TwitterUsername}]),
            {ok, TwitterPost} = TweetedPost:save(),
            TwitterPost;
        _ ->
            Post
    end.

before_filters(DefaultFilters, _RequestContext) ->
    DefaultFilters ++ [require_login_filter].
