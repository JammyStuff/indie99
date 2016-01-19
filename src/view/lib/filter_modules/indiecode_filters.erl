-module(indiecode_filters).
-export([indiecode/1]).

auto_a(Content) ->
    % This is a really bad regex, only works for http(s) and catches things that
    % aren't actually URLs. We should write a proper RFC 3986 compliant regex at
    % some point.
    re:replace(Content, "https?://\\S+", "<a href=\"&\">&</a>",
               [global, {return, list}]).

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
    AutoLinked = auto_a(AposEscaped),
    insert_p(AutoLinked).
