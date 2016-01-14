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
