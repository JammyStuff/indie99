-module(indiecode_filters).
-export([indiecode/1]).

escape_amp(Content) ->
    re:replace(Content, "&", "\\&amp;", [global, {return, list}]).

escape_apos(Content) ->
    re:replace(Content, "'", "\\&#x27;", [global, {return, list}]).

escape_gt(Content) ->
    re:replace(Content, ">", "\\&gt;", [global, {return, list}]).

escape_lt(Content) ->
    re:replace(Content, "<", "\\&lt;", [global, {return, list}]).

escape_quot(Content) ->
    re:replace(Content, "\"", "\\&quot", [global, {return, list}]).

insert_p(Content) ->
    Paragraphs = string:tokens(Content, "\r\n"),
    JoinedParagraphs = string:join(Paragraphs, "</p><p>"),
    io_lib:format("<p>~s</p>", [JoinedParagraphs]).

indiecode(Value) ->
    AmpEscaped = escape_amp(Value),
    LtEscaped = escape_lt(AmpEscaped),
    GtEscaped = escape_gt(LtEscaped),
    QuotEscaped = escape_quot(GtEscaped),
    AposEscaped = escape_apos(QuotEscaped),
    insert_p(AposEscaped).
