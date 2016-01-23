-module(indie99_admin_controller, [Req, SessionID]).
-export([before_filters/2, posts/3, services/3]).

posts('GET', [], _RequestContext) ->
    Posts = boss_db:find(post, [],
                               [{order_by, created_at}, {descending, true}]),
    {ok, [{title, "Posts Admin"}, {posts, Posts}]};

posts('GET', ["create"], _RequestContext) ->
    {render_other, [{action, "posts_create"}], [{title, "Create Post"}]};

posts('POST', ["create"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    Post = post:new(id, Title, Content, undefined, Blogger:id(),
                    erlang:universaltime(), undefined, undefined),
    case Post:validate() of
        {error, Errors} ->
            {render_other, [{action, "posts_create"}],
                           [{title, "Create Post"}, {form_errors, Errors}]};
        ok ->
            {ok, SavedPost} = Post:save(),
            {redirect, io_lib:format("/p/~s", [SavedPost:slug()])}
    end;

posts('GET', [PostId], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{action, "posts_edit"}],
                           [{title, "Edit Post"}, {post, Post}]};
        _ ->
            not_found
    end;

posts('POST', [PostId], _RequestContext) ->
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            UpdatedPost = Post:set([{title, Title}, {content, Content}]),
            case UpdatedPost:validate() of
                {error, Errors} ->
                    {render_other, [{action, "posts_edit"}],
                                   [{title, "Edit Post"},
                                    {post, Post},
                                    {form_errors, Errors}]};
                ok ->
                    {ok, SavedPost} = Post:save(),
                    {redirect, io_lib:format("/p/~s", [SavedPost:slug()])}
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

before_filters(DefaultFilters, _RequestContext) ->
    DefaultFilters ++ [require_login_filter].
