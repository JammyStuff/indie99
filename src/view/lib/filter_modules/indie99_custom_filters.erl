-module(indie99_custom_filters).
-compile(export_all).

% put custom filters in here, e.g.
%
% my_reverse(Value) ->
%     lists:reverse(binary_to_list(Value)).
%
% "foo"|my_reverse   => "oof"

as_id(Value) ->
    [_, Id] = string:tokens(Value, "-"),
    Id.

markdown(Value) ->
    markdown:conv_utf8(Value).

upload_path_to_url(Value) ->
    re:replace(Value, "^\\./priv", "", [{return, list}]).
