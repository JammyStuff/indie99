-module(twitter_client).
-export([get_access_token/3, get_authorize_url/1, get_credentials/0,
         get_request_token/2, is_enabled/0]).

-define(ACCESS_TOKEN_URL, "https://api.twitter.com/oauth/access_token").
-define(AUTHORIZE_URL, "https://api.twitter.com/oauth/authorize").
-define(REQUEST_TOKEN_URL, "https://api.twitter.com/oauth/request_token").

get_access_token(Credentials, {RequestToken, RequestTokenSecret}, Verifier) ->
    {ok, Response} = oauth:get(?ACCESS_TOKEN_URL,
        [{"oauth_verifier", Verifier}], Credentials, RequestToken,
        RequestTokenSecret),
    Parameters = oauth:params_decode(Response),
    Token = oauth:token(Parameters),
    Secret = oauth:token_secret(Parameters),
    {Token, Secret}.

get_authorize_url({RequestToken, _RequestTokenSecret}) ->
    io_lib:format("~s?oauth_token=~s", [?AUTHORIZE_URL, RequestToken]).

get_credentials() ->
    {ok, {Key, Secret}} = application:get_env(indie99, twitter_credentials),
    {Key, Secret, hmac_sha1}.

get_request_token(Credentials, CallbackUrl) ->
    FlatUrl = lists:flatten(CallbackUrl),
    {ok, Response} = oauth:get(?REQUEST_TOKEN_URL,
        [{"oauth_callback", FlatUrl}], Credentials),
    Parameters = oauth:params_decode(Response),
    Token = oauth:token(Parameters),
    Secret = oauth:token_secret(Parameters),
    {Token, Secret}.

is_enabled() ->
    application:get_env(indie99, twitter_credentials) =/= undefined.
