%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% This pass walks over the tree and parses inline elements.
-module(asciideck_inline_pass).
-compile({no_auto_import, [link/2]}).

-export([run/1]).
-export([inline_verse/1]).

-import(asciideck_block_parser, [trim/1, trim/2, while/2]).

%% Verse keeps line breaks. Normal text folds them into a space.
-define(KEEP_NL, asciideck_inline_keep_nl).

-type inline_ast() :: list(). %% @todo
-export_type([inline_ast/0]).

run([]) ->
	[];
%% Verse is the original lines, not the parsed children.
%% A quote is a section body. Its raw lines are no longer needed.
run([{quote_block, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
	case quote_style(Attrs) of
		<<"verse">> ->
			Text = verse_text(maps:get(raw, Ann, <<>>)),
			[{quote_block, Attrs, inline_verse(Text),
				maps:remove(raw, Ann)}|run(Tail)];
		_ ->
			[{quote_block, Attrs, run(Items), maps:remove(raw, Ann)}|run(Tail)]
	end;
run([Data|Tail]) when is_binary(Data) ->
	[inline(Data)|run(Tail)];
%% We do not do any inline formatting for verbatim blocks,
%% for example listing blocks.
%%
%% @todo subs is a list of values.
run([Item={_, #{<<"subs">> := <<"verbatim">>}, _, _}|Tail]) ->
	[Item|run(Tail)];
%% Comment blocks are not processed. The text stays in the AST.
run([Block={comment_block, _, _, _}|Tail]) ->
	[Block|run(Tail)];
%% Labeled lists' labels can also have inline formatting.
run([{Type, Attrs=#{label := Label}, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs#{label => inline(Label)}, run(Items), Ann}|run(Tail)];
run([{Type, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs, run(Items), Ann}|run(Tail)];
run([{Type, Attrs, Data, Ann}|Tail]) ->
	[{Type, Attrs, inline(Data), Ann}|run(Tail)].

%% Positional attribute 1 is the style when that key was written.
%% Otherwise the named style attribute is. The default is quote.
quote_style(Attrs) ->
	case maps:is_key(1, Attrs) of
		true ->
			maps:get(1, Attrs);
		false ->
			maps:get(<<"style">>, Attrs, <<"quote">>)
	end.

%% A line of exactly // comments out the following line too.
%% //text comments out only that line. A blank line remains
%% where the comment was. Blanks at either end are dropped.
%% /// is text. Trailing spaces on a content line are dropped.
verse_text(Raw) ->
	Lines = verse_lines(binary:split(Raw, <<"\n">>, [global]), []),
	join_verse(drop_verse_blanks(Lines)).

verse_lines([], Acc) ->
	lists:reverse(Acc);
verse_lines([Line0|Rest], Acc) ->
	Line = trim(Line0, trailing),
	case Line of
		%% A backslash keeps the comment text. With nothing
		%% after //, the next line is part of that text.
		<<"\\//">> ->
			case Rest of
				[Next|More] ->
					verse_lines(More, [trim(Next, trailing), <<"//">>|Acc]);
				[] ->
					verse_lines([], [<<"//">>|Acc])
			end;
		<<"\\//", C, Text/bits>> when C =/= $/ ->
			verse_lines(Rest, [<<"//", C, Text/bits>>|Acc]);
		_ ->
			case verse_comment(Line) of
				both ->
					verse_lines(drop_verse_line(Rest), [<<>>|Acc]);
				line ->
					verse_lines(Rest, [<<>>|Acc]);
				keep ->
					verse_lines(Rest, [Line|Acc])
			end
	end.

verse_comment(<<"//">>) ->
	both;
verse_comment(<<"//", C, _/bits>>) when C =/= $/ ->
	line;
verse_comment(_) ->
	keep.

drop_verse_line([_|Rest]) ->
	Rest;
drop_verse_line([]) ->
	[].

drop_verse_blanks(Lines) ->
	lists:reverse(drop_verse_leading(lists:reverse(drop_verse_leading(Lines)))).

drop_verse_leading([<<>>|Rest]) ->
	drop_verse_leading(Rest);
drop_verse_leading(Lines) ->
	Lines.

join_verse([]) ->
	<<>>;
join_verse(Lines) ->
	iolist_to_binary(lists:join(<<"\n">>, Lines)).

%% Verse keeps the line breaks that normal text would fold.
inline_verse(Data) ->
	put(?KEEP_NL, true),
	try inline(Data)
	after erase(?KEEP_NL)
	end.

%% We reduce inline content with a single text element
%% with no formatting to a simple binary.
inline(<<>>) ->
	<<>>;
inline(Data) ->
	{Text, Spans} = neutralize(Data),
	inline(Text, start, true, Spans).

%% Mark is the character before this text. 'start' is the
%% real beginning, where ^ is a boundary. 'consumed' means
%% the previous character was a quote mark that already
%% closed and cannot bound another one. Eol is whether the
%% end of this text is the end of the line. Quote bodies
%% are not.
inline(Data, Mark, Eol, Spans) ->
	case inline(Data, <<>>, [], Mark, Eol, Spans) of
		[] -> <<>>;
		[Text] when is_binary(Text) -> Text;
		AST -> AST
	end.

inline(<<>>, <<>>, Acc, _, _, _) ->
	lists:reverse(Acc);
inline(<<>>, BinAcc, Acc, _, _, _) ->
	lists:reverse([BinAcc|Acc]);
inline(Data, BinAcc, Acc, Mark, Eol, Spans) ->
	oneof(Data, BinAcc, Acc, Mark, Eol, Spans, parsers(Mark, Eol, Spans)).

parsers(Mark, Eol, Spans) ->
	[
		fun xref/2,
		fun link/2,
		fun http_link/2,
		fun https_link/2,
		fun mailto_link/2,
		%% Doubled marks are tried before the single marks.
		fun(Bin, _) -> unconstrained_strong(Bin, Mark, Spans) end,
		fun(Bin, _) -> unconstrained_emphasis(Bin, Mark, Spans) end,
		%% +++ is a passthrough and is taken before ++.
		fun passthrough_plus/2,
		fun(Bin, _) -> unconstrained_mono(Bin, Mark, Spans) end,
		fun(Bin, _) -> constrained_mono(Bin, Mark, Spans) end,
		fun(Bin, _) -> superscript(Bin, Mark, Spans) end,
		fun(Bin, _) -> subscript(Bin, Mark, Spans) end,
		fun(Bin, _) -> emphasized_single_quote(Bin, Mark, Spans) end,
		fun(Bin, _) -> emphasized_underline(Bin, Mark, Spans) end,
		fun(Bin, _) -> strong(Bin, Mark, Spans) end,
		fun inline_literal_passthrough/2,
		fun(Bin, _) -> line_break(Bin, Eol) end
	].

nl_char() ->
	case get(?KEEP_NL) of
		true -> <<$\n>>;
		_ -> <<$\s>>
	end.

nl_mark() ->
	case get(?KEEP_NL) of
		true -> $\n;
		_ -> $\s
	end.

%% The inline pass replaces \r\n and \n with a simple space
%% when it occurs within normal text. Verse keeps the break,
%% including inside a quote that spans lines.
oneof(<<$\r, $\n, Rest/bits>>, BinAcc, Acc, _, Eol, Spans, []) ->
	inline(Rest, <<BinAcc/binary, (nl_char())/binary>>, Acc, nl_mark(), Eol,
		shift_spans(Spans, 2));
oneof(<<$\n, Rest/bits>>, BinAcc, Acc, _, Eol, Spans, []) ->
	inline(Rest, <<BinAcc/binary, (nl_char())/binary>>, Acc, nl_mark(), Eol,
		shift_spans(Spans, 1));
oneof(<<C/utf8, Rest/bits>>, BinAcc, Acc, _, Eol, Spans, []) ->
	inline(Rest, <<BinAcc/binary, C/utf8>>, Acc, C, Eol,
		shift_spans(Spans, byte_size(<<C/utf8>>)));
oneof(<<C, Rest/bits>>, BinAcc, Acc, _, Eol, Spans, []) ->
	inline(Rest, <<BinAcc/binary, C>>, Acc, C, Eol,
		shift_spans(Spans, 1));
oneof(Data, BinAcc, Acc, Mark, Eol, Spans, [Parse|Tail]) ->
	Prev = case BinAcc of
		<<>> -> undefined;
		_ -> binary:last(BinAcc)
	end,
	try Parse(Data, Prev) of
		{ok, Inline, Rest, NextMark} when BinAcc =:= <<>> ->
			inline(Rest, <<>>, [Inline|Acc], NextMark, Eol,
				drop_spans(Data, Rest, Spans));
		{ok, Inline, Rest, NextMark} ->
			inline(Rest, <<>>, [Inline, BinAcc|Acc], NextMark, Eol,
				drop_spans(Data, Rest, Spans));
		{ok, Inline, Rest} when BinAcc =:= <<>> ->
			inline(Rest, <<>>, [Inline|Acc], prev_after(Inline), Eol,
				drop_spans(Data, Rest, Spans));
		{ok, Inline, Rest} ->
			inline(Rest, <<>>, [Inline, BinAcc|Acc], prev_after(Inline),
				Eol, drop_spans(Data, Rest, Spans));
		%% The backslash before a matched quote is removed.
		%% The opener is ordinary text and the rest is parsed again.
		%% The same mark following that opener must not open.
		{literal, Text, Rest} ->
			inline(Rest, unescaped(BinAcc, Text), Acc,
				quote_resume(Rest, binary:last(Text)), Eol,
				drop_spans(Data, Rest, Spans));
		{skip, Text, Rest} ->
			oneof(Rest, <<BinAcc/binary, Text/binary>>, Acc, Mark, Eol,
				drop_spans(Data, Rest, Spans), Tail)
	catch _:_ ->
		oneof(Data, BinAcc, Acc, Mark, Eol, Spans, Tail)
	end.

drop_spans(Data, Rest, Spans) ->
	shift_spans(Spans, byte_size(Data) - byte_size(Rest)).

%% Delete a backslash only when a doubled-quote match
%% starts on it. The opener is recorded and the search
%% resumes one byte into it, so the closer can open the
%% next pair. A backslash that is only the body of an
%% earlier pair stays. Passes run in conf order.
%% "+++" is a passthrough and is not a "++" quote.
neutralize(Bin) ->
	{Bin1, S1} = escape_pass(Bin, $*, []),
	{Bin2, S2} = escape_pass(Bin1, $+, S1),
	{Bin3, S3} = escape_pass(Bin2, $_, S2),
	{Bin3, lists:sort(S3)}.

escape_pass(Bin, C, Spans) ->
	escape_pass(Bin, C, 0, Spans).

escape_pass(Bin, C, From, Spans) ->
	SearchFrom = case From of
		0 -> 0;
		_ -> From + 1
	end,
	case next_open(Bin, C, SearchFrom) of
		nomatch ->
			{Bin, Spans};
		{Open, Close} ->
			case Open > 0 andalso binary:at(Bin, Open - 1) =:= $\\ of
				true ->
					PreLen = Open - 1,
					<<Pre:PreLen/binary, $\\, Post/binary>> = Bin,
					escape_pass(<<Pre/binary, Post/binary>>, C, PreLen + 1,
						[{PreLen, PreLen + 2}|shift_deleted(Spans, PreLen)]);
				false ->
					escape_pass(Bin, C, Close + 2, Spans)
			end
	end.

next_open(Bin, C, SearchFrom) ->
	next_open(Bin, C, SearchFrom, plus_passthroughs(Bin)).

next_open(Bin, C, SearchFrom, Passes) ->
	case find_mark(Bin, C, SearchFrom, Passes, []) of
		nomatch ->
			nomatch;
		Open ->
			case find_closer(Bin, C, Open, Passes, []) of
				nomatch ->
					next_open(Bin, C, Open + 1, Passes);
				Close ->
					{Open, Close}
			end
	end.

shift_deleted([{O, E}|T], Index) ->
	[{shift_del(O, Index), shift_del(E, Index)}|shift_deleted(T, Index)];
shift_deleted([], _) ->
	[].

shift_del(P, Index) when P > Index ->
	P - 1;
shift_del(P, _) ->
	P.

shift_spans([{_, E}|T], N) when E =< N ->
	shift_spans(T, N);
shift_spans([{O, E}|T], N) when O < N ->
	[{0, E - N}|shift_spans(T, N)];
shift_spans([{O, E}|T], N) ->
	[{O - N, E - N}|shift_spans(T, N)];
shift_spans([], _) ->
	[].

%% A doubled mark covers [Pos, Pos + 2). It overlaps an
%% escaped opener when it is that opener or starts one
%% byte earlier. Both are invisible once the search has
%% resumed inside the opener.
overlaps_escaped(Pos, [{O, E}|_]) when Pos < E, Pos + 2 > O ->
	true;
overlaps_escaped(Pos, [_|T]) ->
	overlaps_escaped(Pos, T);
overlaps_escaped(_, []) ->
	false.

%% Spans that overlap [From, From + Len), relative to From.
subspans(Spans, From, Len) ->
	To = From + Len,
	[subspan(O, E, From, To) || {O, E} <- Spans, E > From, O < To].

subspan(O, E, From, To) ->
	{max(0, O - From), min(E, To) - From}.

%% The closer stays visible to the next quote. A constrained
%% quote that ends on the first of a doubled mark returns
%% 'consumed' itself, via quote_resume/2.
prev_after({_, #{right := C}, _, _}) when is_integer(C) ->
	C;
prev_after(_) ->
	start.

%% Drop the backslash that suppressed this quote.
unescaped(BinAcc, Text) ->
	Size = byte_size(BinAcc) - 1,
	<<Keep:Size/binary, $\\>> = BinAcc,
	<<Keep/binary, Text/binary>>.

-ifdef(TEST).
text_test() ->
	<<>> = inline(<<>>),
	<<"Hello, Robert">> = inline(<<"Hello, Robert">>),
	ok.
-endif.

-define(IS_BOUNDARY(C), C =:= undefined; C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n; C =:= $().

%% Asciidoc User Guide 21.2.1
%%
%% We currently do not implement the <<...>> form.
xref(<<"xref:", IDAndCaption/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	%% ID must not contain whitespace characters.
	{ID, <<"[", Caption0/bits>>} = while(fun(C) ->
		(C =/= $[) andalso (C =/= $\s) andalso (C =/= $\t)
	end, IDAndCaption),
	%% It is followed by a caption.
	{Caption1, <<"]", Rest/bits>>} = while(fun(C) ->
		C =/= $]
	end, Caption0),
	Caption = trim(Caption1),
	{ok, {xref, #{
		id => ID
	}, Caption, inline}, Rest}.

-ifdef(TEST).
xref_test() ->
	[{xref, #{
		id := <<"tiger_image">>
	}, <<"face of a tiger">>, _}] = inline(<<"xref:tiger_image[face of a tiger]">>),
	ok.
-endif.

%% Asciidoc User Guide 21.1.3
link(<<"link:", TargetAndCaption/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	%% Target must not contain whitespace characters.
	{Target, <<"[", Caption0/bits>>} = while(fun(C) ->
		(C =/= $[) andalso (C =/= $\s) andalso (C =/= $\t)
			andalso (C =/= $\r) andalso (C =/= $\n)
	end, TargetAndCaption),
	%% It is followed by a caption.
	{Caption1, <<"]", Rest/bits>>} = while(fun(C) ->
		C =/= $]
	end, Caption0),
	Caption = trim(Caption1),
	{ok, {link, #{
		target => Target
	}, Caption, inline}, Rest}.

-ifdef(TEST).
link_test() ->
	[{link, #{
		target := <<"downloads/foo.zip">>
	}, <<"download foo.zip">>, _}] = inline(<<"link:downloads/foo.zip[download foo.zip]">>),
	[{link, #{
		target := <<"chapter1.asciidoc#fragment">>
	}, <<"Chapter 1.">>, _}] = inline(<<"link:chapter1.asciidoc#fragment[Chapter 1.]">>),
	[
		{link, #{target := <<"first.zip">>}, <<"first">>, _},
		<<", ">>,
		{link, #{target := <<"second.zip">>}, <<"second">>, _}
	] = inline(<<"link:first.zip[first],\nlink:second.zip[second]">>),
	ok.
-endif.

%% Asciidoc User Guide 21.1.1
http_link(<<"http:", Rest/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	direct_link(Rest, <<"http:">>).

direct_link(Data, Prefix) ->
	%% Target must not contain whitespace characters.
	{Target0, Rest0} = while(fun(C) ->
		(C =/= $[) andalso (C =/= $\s) andalso (C =/= $\t)
			andalso (C =/= $\r) andalso (C =/= $\n)
			andalso (C =/= $,)
	end, Data),
	%% The link must be more than just the prefix.
	false = Target0 =:= <<>>,
	Target = <<Prefix/binary, Target0/binary>>,
	%% It is optionally followed by a caption. Otherwise
	%% the link itself is the caption.
	case Rest0 of
		<<"[", Caption0/bits>> ->
			{Caption1, <<"]", Rest/bits>>} = while(fun(C) ->
				C =/= $]
			end, Caption0),
			Caption = trim(Caption1),
			case Caption of
				<<>> ->
					{ok, {link, #{
						target => Target
					}, Target, inline}, Rest};
				_ ->
					{ok, {link, #{
						target => Target
					}, Caption, inline}, Rest}
			end;
		_ ->
			{ok, {link, #{
				target => Target
			}, Target, inline}, Rest0}
	end.

-ifdef(TEST).
http_link_test() ->
	<<"Incomplete http: link">> = inline(<<"Incomplete http: link">>),
	[
		{link, #{
			target := <<"http://example.org:8080">>
		}, <<"http://example.org:8080">>, _},
		<<", continued">>
	] = inline(<<"http://example.org:8080, continued">>),
	[
		<<"If you have ">>,
		{link, #{
			target := <<"http://example.org/hello#fragment">>
		}, <<"http://example.org/hello#fragment">>, _},
		<<" then:">>
	] = inline(<<"If you have http://example.org/hello#fragment then:">>),
	[
		<<"If you have ">>,
		{link, #{
			target := <<"http://example.org/hello#fragment">>
		}, <<"http://example.org/hello#fragment">>, _},
		<<" then:">>
	] = inline(<<"If you have http://example.org/hello#fragment\nthen:">>),
	[
		<<"Oh, ">>,
		{link, #{
			target := <<"http://example.org/hello#fragment">>
		}, <<"hello there">>, _},
		<<", young lad.">>
	] = inline(<<"Oh, http://example.org/hello#fragment[hello there], young lad.">>),
	ok.
-endif.

%% Asciidoc User Guide 21.1.1
https_link(<<"https:", Rest/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	direct_link(Rest, <<"https:">>).

-ifdef(TEST).
https_link_test() ->
	<<"Incomplete https: link">> = inline(<<"Incomplete https: link">>),
	[
		{link, #{
			target := <<"https://example.org:8080">>
		}, <<"https://example.org:8080">>, _},
		<<", continued">>
	] = inline(<<"https://example.org:8080, continued">>),
	[
		<<"If you have ">>,
		{link, #{
			target := <<"https://example.org/hello#fragment">>
		}, <<"https://example.org/hello#fragment">>, _},
		<<" then:">>
	] = inline(<<"If you have https://example.org/hello#fragment then:">>),
	[
		<<"If you have ">>,
		{link, #{
			target := <<"https://example.org/hello#fragment">>
		}, <<"https://example.org/hello#fragment">>, _},
		<<" then:">>
	] = inline(<<"If you have https://example.org/hello#fragment\nthen:">>),
	[
		<<"Oh, ">>,
		{link, #{
			target := <<"https://example.org/hello#fragment">>
		}, <<"hello there">>, _},
		<<", young lad.">>
	] = inline(<<"Oh, https://example.org/hello#fragment[hello there], young lad.">>),
	ok.
-endif.

%% Asciidoc User Guide 21.1.1
mailto_link(<<"mailto:", Rest0/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	{ok, {link, Attrs, Caption0, Ann}, Rest} = direct_link(Rest0, <<"mailto:">>),
	Caption = case Caption0 of
		<<"mailto:", Caption1/bits>> -> Caption1;
		_ -> Caption0
	end,
	{ok, {link, Attrs, Caption, Ann}, Rest}.

-ifdef(TEST).
mailto_link_test() ->
	[
		{link, #{
			target := <<"mailto:joe.bloggs@foobar.com">>
		}, <<"email Joe Bloggs">>, _}
	] = inline(<<"mailto:joe.bloggs@foobar.com[email Joe Bloggs]">>),
	[
		{link, #{
			target := <<"mailto:srackham@gmail.com">>
		}, <<"srackham@gmail.com">>, _}
	] = inline(<<"mailto:srackham@gmail.com[]">>),
	ok.
-endif.

%% Asciidoc User Guide 10.1.2
%%
%% The mark is doubled and may appear anywhere, including
%% inside a word. The content has to be non-empty.
%% A backslash immediately before the opening mark is
%% removed by neutralize/1. That pair is not matched
%% here, so a constrained quote can still use it.
unconstrained_strong(Data, Mark, Spans) ->
	unconstrained(Data, Mark, strong, $*, Spans).
unconstrained_emphasis(Data, Mark, Spans) ->
	unconstrained(Data, Mark, emphasized, $_, Spans).
%% User Guide passthrough macros. +++text+++ is copied
%% through with no substitutions. It is not a ++ quote.
%% +++foo++ has no closing triple, so ++ may match it.
passthrough_plus(<<"+++", Rest0/bits>>, _) ->
	{Pos, _} = binary:match(Rest0, <<"+++">>),
	<<Content:Pos/binary, _:3/binary, Rest/bits>> = Rest0,
	{ok, {passthrough, #{}, Content, inline}, Rest}.

unconstrained_mono(Data, Mark, Spans) ->
	unconstrained(Data, Mark, monospaced, $+, Spans).

%% Asciidoc User Guide 10.1
%%
%% The content cannot start or end with whitespace. The
%% closer must be followed by a non-word character or the
%% end of the text. A finished "++" pair and a +++ span
%% are not closers. The left side is ^ or a character
%% other than a word character, ";", ":", or "}".
%% "++" is tried first, so ++x++y is unconstrained.
constrained_mono(<<$+, Rest/bits>>, Mark, Spans) ->
	case plus_left(Mark) of
		false ->
			error(nomatch);
		true ->
			Passes = plus_passthroughs(Rest),
			Esc = shift_spans(Spans, 1),
			Doubled = quote_spans(Rest, $+, 0, Passes, Esc),
			{Content, Tail} = plus_content(Rest,
				lists:sort(Doubled ++ Passes), 0),
			case Mark of
				$\\ ->
					{literal, <<$+>>, Rest};
				_ ->
					{ok, {monospaced, #{
						left => $+,
						right => $+
					}, inline(Content, $+, false,
						subspans(Spans, 1, byte_size(Content))), inline},
						Tail, quote_resume(Tail, $+)}
			end
	end.

%% Closing on the first mark of a doubled pair must not
%% let the second mark open a new quote. A "+" after a
%% finished "++" pair still opens.
quote_resume(<<C, _/bits>>, C) ->
	consumed;
quote_resume(_, C) ->
	C.

%% ^ is only the real start of the text. A consumed closer
%% is not a boundary. ";" ":" "}" and word characters,
%% including "_", are not either.
plus_left(start) ->
	true;
plus_left(consumed) ->
	false;
%% "__" was replaced by a tag. "*" and "+" already ran
%% against the "_" and must not see the new boundary.
plus_left(us_tag) ->
	false;
plus_left(C) ->
	not plus_word(C) andalso C =/= $; andalso C =/= $: andalso C =/= $}.

plus_word(C) when C >= $a, C =< $z -> true;
plus_word(C) when C >= $A, C =< $Z -> true;
plus_word(C) when C >= $0, C =< $9 -> true;
plus_word($_) -> true;
plus_word(C) when is_integer(C), C > 127 ->
	match =:= re:run(<<C/utf8>>, <<"^\\w$">>, [unicode, ucp, {capture, none}]);
plus_word(_) ->
	false.

plus_content(Bin, _, Pos) when Pos >= byte_size(Bin) ->
	error(nomatch);
plus_content(<<C, _/bits>>, _, 0) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
	error(nomatch);
plus_content(Bin, Skip, Pos) ->
	case plus_closer(Bin, Skip, Pos) of
		true ->
			<<Content:Pos/binary, _:1/binary, Tail/bits>> = Bin,
			{Content, Tail};
		false ->
			plus_content(Bin, Skip, Pos + 1)
	end.

plus_closer(Bin, Skip, Pos) ->
	case binary:at(Bin, Pos) =:= $+ andalso not in_span(Pos, Skip) of
		false ->
			false;
		true when Pos =:= 0 ->
			false;
		true ->
			not plus_space(binary:at(Bin, Pos - 1))
				andalso plus_right_boundary(Bin, Pos + 1)
	end.

plus_space($\s) -> true;
plus_space($\t) -> true;
plus_space($\n) -> true;
plus_space($\r) -> true;
plus_space(_) -> false.

%% Python re \w, including letters such as "é". The ucp
%% flag is what makes \w use Unicode properties.
plus_right_boundary(Bin, Pos) when Pos >= byte_size(Bin) ->
	true;
plus_right_boundary(Bin, Pos) ->
	<<_:Pos/binary, Rest/binary>> = Bin,
	nomatch =:= re:run(Rest, <<"^\\w">>, [unicode, ucp, {capture, none}]).

unconstrained(<<C, C, _/bits>> = Data, _, Type, C, Spans) ->
	case overlaps_escaped(0, Spans) of
		true ->
			error(nomatch);
		false ->
			[{0, End}|_] = quote_spans(Data, C, Spans),
			ContentLen = End - 4,
			<<_:2/binary, Content:ContentLen/binary, _:2/binary, Rest/bits>> = Data,
			{ok, {Type, #{
				left => C,
				right => C
			}, inline(Content, C, false,
				subspans(Spans, 2, ContentLen)), inline}, Rest,
				unconstrained_mark(C)}
	end.

%% The "_" closer is a word character. After the tag is
%% written the next "_" quote sees ">", not that closer.
unconstrained_mark($_) ->
	us_tag;
unconstrained_mark(C) ->
	C.

%% Doubled marks are applied to the whole paragraph before
%% the single mark of the same character. A constrained
%% quote therefore does not close on either half of "**"
%% or "__". "2 * 3 = **six**." leaves the single * literal.
%%
%% +++text+++ is removed before any of those marks are
%% paired. The pluses and the bytes inside are not marks.
%% A doubled mark that overlaps an escaped opener is not
%% a span. The closer and the marks inside still are.
quote_spans(Bin, C, Esc) ->
	quote_spans(Bin, C, 0, plus_passthroughs(Bin), Esc).

quote_spans(Bin, C, From, Skip, Esc) ->
	case find_mark(Bin, C, From, Skip, Esc) of
		nomatch ->
			[];
		Open ->
			case find_closer(Bin, C, Open, Skip, Esc) of
				nomatch ->
					quote_spans(Bin, C, Open + 1, Skip, Esc);
				Close ->
					[{Open, Close + 2}
						|quote_spans(Bin, C, Close + 2, Skip, Esc)]
			end
	end.

%% The closer is the first doubled mark that leaves at least
%% one byte of content. "*****" is strong of a single "*".
%% "****" has nothing between the pairs, so it is not an
%% unconstrained quote. The constrained form may still match.
find_closer(Bin, C, Open, Skip, Esc) ->
	find_closer(Bin, C, Open, Open + 2, Skip, Esc).

find_closer(Bin, C, Open, From, Skip, Esc) ->
	case find_mark(Bin, C, From, Skip, Esc) of
		nomatch ->
			nomatch;
		Close when Close > Open + 2 ->
			Close;
		Close ->
			find_closer(Bin, C, Open, Close + 1, Skip, Esc)
	end.

find_mark(Bin, C, From, Skip, Esc) ->
	Size = byte_size(Bin),
	case From > Size - 2 of
		true ->
			nomatch;
		false ->
			case binary:match(Bin, <<C, C>>, [{scope, {From, Size - From}}]) of
				nomatch ->
					nomatch;
				{Pos, 2} ->
					case overlaps_escaped(Pos, Esc) of
						true ->
							find_mark(Bin, C, Pos + 1, Skip, Esc);
						false ->
							Inside = case in_span(Pos, Skip) of
								true -> Pos;
								false -> Pos + 1
							end,
							case in_span(Inside, Skip) of
								true ->
									find_mark(Bin, C, skip_end(Inside, Skip),
										Skip, Esc);
								false ->
									Pos
							end
					end
			end
	end.

%% +++text+++ spans, including the delimiters. The first
%% closer wins. A triple with no closer is not a span.
plus_passthroughs(Bin) ->
	plus_passthroughs(Bin, 0).

plus_passthroughs(Bin, From) ->
	Size = byte_size(Bin),
	case From > Size - 6 of
		true ->
			[];
		false ->
			case binary:match(Bin, <<"+++">>, [{scope, {From, Size - From}}]) of
				nomatch ->
					[];
				{Open, 3} ->
					After = Open + 3,
					case After > Size - 3 of
						true ->
							[];
						false ->
							case binary:match(Bin, <<"+++">>,
									[{scope, {After, Size - After}}]) of
								nomatch ->
									[];
								{Close, 3} ->
									[{Open, Close + 3}
										|plus_passthroughs(Bin, Close + 3)]
							end
					end
			end
	end.

skip_end(Pos, [{_, End}|Rest]) when Pos >= End ->
	skip_end(Pos, Rest);
skip_end(Pos, [{Open, End}|_]) when Pos >= Open ->
	End;
skip_end(Pos, _) ->
	Pos + 2.

%% Backtick text is not scanned for quotes. A +++ span
%% inside it is still a passthrough: the delimiters are
%% removed and the interior is kept as-is.
splice_passthroughs(Bin) ->
	case plus_passthroughs(Bin) of
		[] ->
			Bin;
		Spans ->
			splice_spans(Bin, Spans, 0)
	end.

splice_spans(Bin, [], From) ->
	<<_:From/binary, Tail/binary>> = Bin,
	case Tail of
		<<>> -> [];
		_ -> [Tail]
	end;
splice_spans(Bin, [{Open, End}|Spans], From) ->
	BeforeLen = Open - From,
	InnerLen = End - Open - 6,
	<<_:From/binary, Before:BeforeLen/binary, "+++",
		Inner:InnerLen/binary, "+++", _/binary>> = Bin,
	BeforePiece = case Before of
		<<>> -> [];
		_ -> [Before]
	end,
	BeforePiece ++ [{passthrough, #{}, Inner, inline}
		|splice_spans(Bin, Spans, End)].

%% Asciidoc User Guide 10.2
%%
%% Superscript and subscript are unconstrained. The content
%% has to be non-empty. "^^^" is a superscript of "^".
%% "^^" is not a superscript.
superscript(<<$^, Rest/bits>>, Mark, Spans) ->
	single_mark(Rest, $^, superscript, Mark, Spans).
subscript(<<$~, Rest/bits>>, Mark, Spans) ->
	single_mark(Rest, $~, subscript, Mark, Spans).

%% 'consumed' is the mark just emitted as literal text.
%% It must not open again, so \^^b^ stays ^^b^ rather than
%% a superscript of b. A backslash still suppresses only
%% the matched opener: \^sup^ is ^sup^, and \^a^b^ is ^a
%% plus a superscript of b.
single_mark(_, _, _, consumed, _) ->
	error(nomatch);
single_mark(Rest, C, _, $\\, _) ->
	_ = single_closer(Rest, C, 0, plus_passthroughs(Rest)),
	{literal, <<C>>, Rest};
single_mark(Rest, C, Type, _, Spans) ->
	Pos = single_closer(Rest, C, 0, plus_passthroughs(Rest)),
	<<Content:Pos/binary, _:1/binary, Tail/bits>> = Rest,
	{ok, {Type, #{}, inline(Content, C, false,
		subspans(Spans, 1, byte_size(Content))), inline}, Tail}.

single_closer(Bin, C, From, Skip) ->
	Size = byte_size(Bin),
	case From >= Size of
		true ->
			error(nomatch);
		false ->
			case binary:match(Bin, <<C>>, [{scope, {From, Size - From}}]) of
				nomatch ->
					error(nomatch);
				{Pos, 1} ->
					case in_span(Pos, Skip) of
						true ->
							single_closer(Bin, C, skip_end(Pos, Skip), Skip);
						false when Pos =:= 0 ->
							single_closer(Bin, C, 1, Skip);
						false ->
							Pos
					end
			end
	end.

%% Asciidoc User Guide 10.1
emphasized_single_quote(Data, Mark, Spans) ->
	quoted_text(Data, Mark, emphasized, $', $', Spans).
emphasized_underline(Data, Mark, Spans) ->
	quoted_text(Data, Mark, emphasized, $_, $_, Spans).
strong(Data, Mark, Spans) ->
	quoted_text(Data, Mark, strong, $*, $*, Spans).

quoted_text(<<Left, Rest0/bits>>, Mark, Type, Left, Right, Spans) ->
	case opens_constrained(Mark, Left) of
		false ->
			error(nomatch);
		true ->
			{Content, Rest} = constrained_content(Rest0, Left,
				shift_spans(Spans, 1)),
			%% The content has to be non-empty.
			true = Content =/= <<>>,
			case Mark of
				$\\ ->
					{literal, <<Left>>, Rest0};
				_ ->
					{ok, {Type, #{
						left => Left,
						right => Right
					}, inline(Content, Left, false,
						subspans(Spans, 1, byte_size(Content))), inline},
						Rest, quote_resume(Rest, Right)}
			end
	end.

%% "_" after an unconstrained "__" sees the tag boundary.
%% "*" and "+" do not: those passes already ran.
opens_constrained(us_tag, $_) ->
	true;
opens_constrained(Mark, _) ->
	plus_left(Mark).

%% * and _ have a doubled form. Skip those spans when
%% looking for the closing mark. +++text+++ is not text
%% for any quote, so those spans are skipped too.
%% An escaped doubled pair is not skipped: its characters
%% can still close a constrained quote.
%% The closer must be followed by a non-word character,
%% and the content cannot start or end with whitespace.
constrained_content(Bin, C, Esc) when C =:= $*; C =:= $_ ->
	Passes = plus_passthroughs(Bin),
	Doubled = quote_spans(Bin, C, 0, Passes, Esc),
	take_bounded(Bin, C, lists:sort(Doubled ++ Passes));
constrained_content(Bin, C, _) ->
	take_bounded(Bin, C, plus_passthroughs(Bin)).

take_bounded(<<C, _/bits>>, _, _) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
	error(nomatch);
take_bounded(Bin, C, Spans) ->
	take_bounded(Bin, C, Spans, 0).

take_bounded(Bin, _, _, Pos) when Pos >= byte_size(Bin) ->
	error(nomatch);
take_bounded(Bin, C, Spans, Pos) ->
	case binary:at(Bin, Pos) =:= C andalso not in_span(Pos, Spans)
			andalso Pos > 0
			andalso not plus_space(binary:at(Bin, Pos - 1))
			andalso plus_right_boundary(Bin, Pos + 1) of
		true ->
			<<Content:Pos/binary, _:1/binary, Rest/bits>> = Bin,
			{Content, Rest};
		false ->
			take_bounded(Bin, C, Spans, Pos + 1)
	end.

take_constrained(Bin, _, _, Pos) when Pos >= byte_size(Bin) ->
	error(nomatch);
take_constrained(Bin, C, Spans, Pos) ->
	case binary:at(Bin, Pos) =:= C andalso not in_span(Pos, Spans) of
		true ->
			<<Content:Pos/binary, _:1/binary, Rest/bits>> = Bin,
			{Content, Rest};
		false ->
			take_constrained(Bin, C, Spans, Pos + 1)
	end.

in_span(_, []) ->
	false;
in_span(Pos, [{_, End}|Rest]) when Pos >= End ->
	in_span(Pos, Rest);
in_span(Pos, [{Open, _}|_]) when Pos >= Open ->
	true;
in_span(_, _) ->
	false.

-ifdef(TEST).
emphasized_test() ->
	[
		<<"Word phrases ">>,
		{emphasized, #{left := $', right := $'},
			<<"enclosed in single quote characters">>, _},
		<<" (acute accents) or ">>,
		{emphasized, #{left := $_, right := $_},
			<<"underline characters">>, _},
		<<" are emphasized.">>
	] = inline(<<
		"Word phrases 'enclosed in single quote characters' (acute accents) "
		"or _underline characters_ are emphasized."
	>>),
	<<"'word'next">> = inline(<<"'word'next">>),
	[{emphasized, _, <<"word">>, _}, <<" next">>]
		= inline(<<"'word' next">>),
	<<"See ' not ' here.">> = inline(<<"See ' not ' here.">>),
	[{emphasized, _, <<"em">>, _}, <<" next">>]
		= inline(<<"_em_ next">>),
	[{emphasized, _, <<"'text">>, _}, <<"'">>]
		= inline(<<"''text''">>),
	ok.

strong_test() ->
	[
		<<"Word phrases ">>,
		{strong, #{left := $*, right := $*},
			<<"enclosed in asterisk characters">>, _},
		<<" are rendered in a strong font (usually bold).">>
	] = inline(<<
		"Word phrases *enclosed in asterisk characters* "
		"are rendered in a strong font (usually bold)."
	>>),
	<<"** Two.">> = inline(<<"** Two.">>),
	%% "***" is a strong of one "*", not an empty quote.
	[{strong, _, <<"*">>, _}, <<" Three.">>]
		= inline(<<"*** Three.">>),
	<<"*bold*word">> = inline(<<"*bold*word">>),
	[{strong, _, <<"bold">>, _}, <<" word">>]
		= inline(<<"*bold* word">>),
	[{strong, _, <<"bold*word">>, _}] = inline(<<"*bold*word*">>),
	[{strong, _, <<"bold">>, _}, <<"*more*">>]
		= inline(<<"*bold**more*">>),
	<<"See * not * here.">> = inline(<<"See * not * here.">>),
	[<<"end.">>, {strong, _, <<"bold">>, _}]
		= inline(<<"end.*bold*">>),
	<<"2*bold*">> = inline(<<"2*bold*">>),
	<<"_em_next">> = inline(<<"_em_next">>),
	[{strong, _, <<"bold">>, _}, <<".">>] = inline(<<"*bold*.">>),
	[{strong, _, <<"bold*more">>, _}] = inline(<<"*bold*more*">>),
	[{strong, _, <<"bold">>, _}, <<" ">>, {strong, _, <<"more">>, _}]
		= inline(<<"*bold* *more*">>),
	[{strong, _, <<"foo bar">>, _}] = inline(<<"*foo\nbar*">>),
	<<"*bold*é"/utf8>> = inline(<<"*bold*é"/utf8>>),
	<<"é*bold*"/utf8>> = inline(<<"é*bold*"/utf8>>),
	ok.

%% A backslash before a quote that matches suppresses it
%% and is removed. The text inside is still parsed. A
%% backslash before a mark that does not match stays.
quote_escape_test() ->
	<<"*bold*">> = inline(<<"\\*bold*">>),
	<<"_em_">> = inline(<<"\\_em_">>),
	<<"'em'">> = inline(<<"\\'em'">>),
	<<"pre*bold*">> = inline(<<"pre\\*bold*">>),
	<<"a\\*b">> = inline(<<"a\\*b">>),
	<<"\\*bold*">> = inline(<<"\\\\*bold*">>),
	<<"+code+">> = inline(<<"\\+code+">>),
	<<"see +code+ here">> = inline(<<"see \\+code+ here">>),
	[
		<<"*bold ">>,
		{emphasized, _, <<"em">>, _},
		<<" more*">>
	] = inline(<<"\\*bold _em_ more*">>),
	[
		<<"*bold* ">>,
		{strong, _, <<"more">>, _}
	] = inline(<<"\\*bold* *more*">>),
	[
		<<"*a ">>,
		{strong, _, <<"b">>, _},
		<<" c*">>
	] = inline(<<"\\*a **b** c*">>),
	[
		<<"\\+code ">>,
		{emphasized, _, <<"em">>, _},
		<<" + here">>
	] = inline(<<"\\+code _em_ + here">>),
	<<"****">> = inline(<<"\\****">>),
	<<"a****">> = inline(<<"a\\****">>),
	<<"pre****">> = inline(<<"pre\\****">>),
	<<"\\****">> = inline(<<"\\\\****">>),
	<<"**a*">> = inline(<<"\\**a*">>),
	<<"''a'">> = inline(<<"\\''a'">>),
	[{emphasized, _, <<"a">>, _}, <<"''b'">>]
		= inline(<<"'a'\\''b'">>),
	<<"++++">> = inline(<<"\\++++">>),
	ok.

%% A backslash before a doubled quote removes the
%% backslash and does not format that pattern. A
%% constrained quote may still match the same marks.
unconstrained_escape_test() ->
	[{monospaced, #{left := $+, right := $+}, <<"+code">>, _}, <<"+">>]
		= inline(<<"\\++code++">>),
	[{monospaced, #{left := $+, right := $+}, <<"+ not +">>, _}]
		= inline(<<"\\++ not ++">>),
	[<<"a+">>, {monospaced, #{left := $+, right := $+}, <<"b">>, _},
		<<"+c">>] = inline(<<"a\\++b++c">>),
	<<"++hidden++">> = inline(<<"\\\\++hidden++">>),
	[{strong, #{left := $*, right := $*}, <<"*bold">>, _}, <<"*">>]
		= inline(<<"\\**bold**">>),
	[<<"a*">>, {strong, #{left := $*, right := $*}, <<"b">>, _}, <<"*">>]
		= inline(<<"a\\**b**">>),
	[{strong, #{left := $*, right := $*}, <<"*F">>, _}, <<"*ile">>]
		= inline(<<"\\**F**ile">>),
	<<"**bold**">> = inline(<<"\\\\**bold**">>),
	[{emphasized, #{left := $_, right := $_}, <<"_em_">>, _}]
		= inline(<<"\\__em__">>),
	<<"__File__name">> = inline(<<"\\__File__name">>),
	[{strong, _, <<"*">>, _}, <<"**">>] = inline(<<"\\*****">>),
	[{strong, _, <<"*see *bold">>, _}, <<" here**">>]
		= inline(<<"\\**see *bold* here**">>),
	[{strong, _, [
		<<"*bold ">>,
		{emphasized, _, <<"em">>, _},
		<<" more">>
	], _}, <<"*">>] = inline(<<"\\**bold _em_ more**">>),
	<<"\\**">> = inline(<<"\\**">>),
	<<"^sup^">> = inline(<<"\\^sup^">>),
	<<"~sub~">> = inline(<<"\\~sub~">>),
	<<"e^pi^">> = inline(<<"e\\^pi^">>),
	[<<"^a">>, {superscript, _, <<"b">>, _}]
		= inline(<<"\\^a^b^">>),
	%% The escaped opener is text. The next same mark does
	%% not open, so the whole run stays literal.
	<<"^^b^">> = inline(<<"\\^^b^">>),
	<<"e^^pi^">> = inline(<<"e\\^^pi^">>),
	<<"~~b~">> = inline(<<"\\~~b~">>),
	[{emphasized, _, <<"em">>, _}, <<"^^x^">>]
		= inline(<<"_em_\\^^x^">>),
	%% Deleting the backslash joins the marks before the
	%% doubled pass. The underscore before the slash is not
	%% part of the escaped pair, and it is not a closer.
	[{emphasized, _, <<"foo___bar_">>, _}]
		= inline(<<"_foo_\\__bar__">>),
	[{monospaced, _, <<"code">>, _}, <<"+">>,
		{monospaced, _, <<"more">>, _}, <<"+">>]
		= inline(<<"+code+\\++more++">>),
	[{emphasized, _, <<"foo">>, _}, <<"+">>,
		{monospaced, _, <<"bar">>, _}, <<"+">>]
		= inline(<<"_foo_\\++bar++">>),
	[<<"é+"/utf8>>, {monospaced, _, <<"code">>, _}, <<"+">>]
		= inline(<<"é\\++code++"/utf8>>),
	[{emphasized, _, <<"foo">>, _}, <<"+">>,
		{monospaced, _, <<"bar">>, _}, <<"+">>]
		= inline(<<"__foo__\\++bar++">>),
	[{emphasized, _, <<"foo">>, _}, <<"*">>,
		{strong, _, <<"bar">>, _}, <<"*">>]
		= inline(<<"_foo_\\**bar**">>),
	%% The closer of the escaped pair can open the next one.
	[<<"**a">>, {strong, _, <<"b">>, _}]
		= inline(<<"\\**a**b**">>),
	[<<"++a">>, {monospaced, _, <<"b">>, _}]
		= inline(<<"\\++a++b++">>),
	[<<"__a">>, {emphasized, _, <<"b">>, _}]
		= inline(<<"\\__a__b__">>),
	[{strong, _, <<"keep">>, _}, <<" **a">>, {strong, _, <<"b">>, _}]
		= inline(<<"**keep** \\**a**b**">>),
	[{strong, _, [
		<<"*see ">>,
		{strong, _, <<"bold">>, _},
		<<" here">>
	], _}, <<"*">>] = inline(<<"\\**see **bold** here**">>),
	%% A different doubled mark inside the quote still matches.
	[{strong, _, [
		<<"*bold ">>,
		{monospaced, _, <<"code">>, _},
		<<" more">>
	], _}, <<"*">>] = inline(<<"\\**bold ++code++ more**">>),
	[{strong, _, [
		<<"*bold ">>,
		{emphasized, _, <<"em">>, _},
		<<" more">>
	], _}, <<"*">>] = inline(<<"\\**bold __em__ more**">>),
	[{monospaced, _, [
		<<"+see ">>,
		{strong, _, <<"bold">>, _},
		<<" here">>
	], _}, <<"+">>] = inline(<<"\\++see **bold** here++">>),
	[<<"a*">>, {strong, _, [
		<<"bold ">>,
		{monospaced, _, <<"code">>, _},
		<<" more">>
	], _}, <<"*">>] = inline(<<"a\\**bold ++code++ more**">>),
	%% A backslash inside an earlier pair is the body.
	[{strong, _, <<"\\">>, _}, <<"bar**">>]
		= inline(<<"**\\**bar**">>),
	[<<"a">>, {strong, _, <<"\\">>, _}, <<"b**">>]
		= inline(<<"a**\\**b**">>),
	[{strong, _, <<"a">>, _}, <<"b">>, {strong, _, <<"\\">>, _}, <<"c**">>]
		= inline(<<"**a**b**\\**c**">>),
	[{monospaced, _, <<"\\">>, _}, <<"bar++">>]
		= inline(<<"++\\++bar++">>),
	[{emphasized, _, <<"\\">>, _}, <<"bar__">>]
		= inline(<<"__\\__bar__">>),
	[<<"a">>, {monospaced, _, <<"\\">>, _}, <<"b++">>]
		= inline(<<"a++\\++b++">>),
	[<<"a">>, {emphasized, _, <<"\\">>, _}, <<"b__">>]
		= inline(<<"a__\\__b__">>),
	%% The closer is replaced by a tag before "_" runs.
	[{emphasized, _, <<"a">>, _}, {emphasized, _, <<"_b_">>, _}]
		= inline(<<"__a__\\__b__">>),
	[<<"x">>, {emphasized, _, <<"a">>, _}, {emphasized, _, <<"_b_">>, _}]
		= inline(<<"x__a__\\__b__">>),
	[{emphasized, _, <<"foo">>, _}, {emphasized, _, <<"_bar_">>, _}]
		= inline(<<"__foo__\\__bar__">>),
	[{emphasized, _, <<"a">>, _}, {emphasized, _, <<"b">>, _}]
		= inline(<<"__a___b_">>),
	[{emphasized, _, <<"a">>, _}, {emphasized, _, <<"b_">>, _}]
		= inline(<<"__a___b__">>),
	[{emphasized, _, <<"a">>, _}, <<"*b*">>]
		= inline(<<"__a__*b*">>),
	[{emphasized, _, <<"a">>, _}, <<"+code+">>]
		= inline(<<"__a__+code+">>),
	<<"word___b__">> = inline(<<"word_\\__b__">>),
	[{emphasized, _, <<"em___x_">>, _}]
		= inline(<<"_em_\\__x__">>),
	[{strong, _, <<"a">>, _}, {strong, _, <<"*b">>, _}, <<"*">>]
		= inline(<<"**a**\\**b**">>),
	[{monospaced, _, <<"a">>, _}, {monospaced, _, <<"+b">>, _}, <<"+">>]
		= inline(<<"++a++\\++b++">>),
	[{strong, _, <<"*">>, _}, <<"*bar**">>]
		= inline(<<"\\**\\**bar**">>),
	ok.

%% A closer on the next line ends the quote. The newline
%% becomes a space, as it does in other inline text.
unconstrained_test() ->
	[{strong, #{left := $*, right := $*}, <<"DEPRECATED">>, _}]
		= inline(<<"**DEPRECATED**">>),
	[
		{strong, #{left := $*, right := $*}, <<"F">>, _},
		<<"ile Open">>
	] = inline(<<"**F**ile Open">>),
	[
		<<"a">>,
		{strong, _, <<"b">>, _},
		<<"c">>
	] = inline(<<"a**b**c">>),
	[
		{strong, _, [
			<<"bold ">>,
			{emphasized, _, <<"em">>, _},
			<<" more">>
		], _}
	] = inline(<<"**bold _em_ more**">>),
	[
		{strong, _, <<"a ">>, _},
		<<"b">>,
		{strong, _, <<" c">>, _}
	] = inline(<<"**a **b** c**">>),
	[{strong, _, <<"multi line">>, _}] = inline(<<"**multi\nline**">>),
	[
		{strong, _, <<" Two. ">>, _},
		<<" Three.">>
	] = inline(<<"** Two.\n** Three.">>),
	[{emphasized, #{left := $_, right := $_}, <<"File">>, _}, <<"name">>]
		= inline(<<"__File__name">>),
	%% A single mark does not swallow a later doubled mark.
	[
		<<"use * as a wildcard then ">>,
		{strong, _, <<"F">>, _},
		<<"ile.">>
	] = inline(<<"use * as a wildcard then **F**ile.">>),
	[
		<<"2 * 3 = ">>,
		{strong, _, <<"six">>, _},
		<<".">>
	] = inline(<<"2 * 3 = **six**.">>),
	[
		<<"a _ b and ">>,
		{emphasized, _, <<"call">>, _},
		<<".">>
	] = inline(<<"a _ b and __call__.">>),
	[
		<<"*a ">>,
		{strong, _, <<"b">>, _}
	] = inline(<<"*a **b**">>),
	[{strong, _, [
		<<"a ">>,
		{strong, _, <<"b">>, _},
		<<" c">>
	], _}] = inline(<<"*a **b** c*">>),
	<<"**">> = inline(<<"**">>),
	%% Nothing sits between the pairs, so this is not
	%% unconstrained. Constrained quotes take one mark.
	[{strong, _, <<"*">>, _}, <<"*">>] = inline(<<"****">>),
	<<"__">> = inline(<<"__">>),
	%% "_" is a word character, so the closer is the last one.
	[{emphasized, _, <<"__">>, _}] = inline(<<"____">>),
	%% An unclosed doubled mark must not drop earlier quotes,
	%% and must not pair with the closer of a finished quote.
	[{strong, _, <<"bold">>, _}, <<" and then **">>]
		= inline(<<"*bold* and then **">>),
	[{strong, _, <<"six">>, _}, <<" and **">>]
		= inline(<<"**six** and **">>),
	[{emphasized, _, <<"em">>, _}, <<" and __">>]
		= inline(<<"_em_ and __">>),
	[{strong, _, <<"*">>, _}] = inline(<<"*****">>),
	[
		<<"x ">>,
		{strong, _, <<"**y">>, _},
		<<" z">>
	] = inline(<<"x ****y** z">>),
	ok.
-endif.

-ifdef(TEST).
super_sub_test() ->
	[
		<<"e">>,
		{superscript, #{}, <<"pi">>, _},
		<<"+1">>
	] = inline(<<"e^pi^+1">>),
	[
		<<"H">>,
		{subscript, #{}, <<"2">>, _},
		<<"O">>
	] = inline(<<"H~2~O">>),
	[
		<<"x">>,
		{superscript, #{}, <<"10">>, _}
	] = inline(<<"x^10^">>),
	[{superscript, #{}, <<"super text">>, _}]
		= inline(<<"^super text^">>),
	[{superscript, #{}, <<" not ">>, _}] = inline(<<"^ not ^">>),
	[{subscript, #{}, <<" not ">>, _}] = inline(<<"~ not ~">>),
	<<"^^">> = inline(<<"^^">>),
	<<"~~">> = inline(<<"~~">>),
	[{superscript, #{}, <<"^">>, _}] = inline(<<"^^^">>),
	[
		<<"2">>,
		{superscript, #{}, <<"3">>, _},
		<<" and ^">>
	] = inline(<<"2^3^ and ^">>),
	[{strong, #{}, [
		<<"x">>,
		{superscript, #{}, <<"2">>, _}
	], _}] = inline(<<"*x^2^*">>),
	ok.
-endif.

-ifdef(TEST).
unconstrained_mono_test() ->
	[{monospaced, #{}, <<"mono">>, _}] = inline(<<"++mono++">>),
	[
		<<"a">>,
		{monospaced, #{}, <<"b">>, _},
		<<"c">>
	] = inline(<<"a++b++c">>),
	[{monospaced, #{}, <<" not ">>, _}] = inline(<<"++ not ++">>),
	%% Four pluses are not an unconstrained quote.
	[{monospaced, #{}, <<"+">>, _}, <<"+">>] = inline(<<"++++">>),
	[{monospaced, #{}, <<"+">>, _}] = inline(<<"+++++">>),
	[{monospaced, #{}, [
		<<"code ">>,
		{strong, #{}, <<"bold">>, _}
	], _}] = inline(<<"++code *bold*++">>),
	[{passthrough, #{}, <<"<b>raw</b>">>, _}]
		= inline(<<"+++<b>raw</b>+++">>),
	[
		<<"a ">>,
		{passthrough, #{}, <<"b">>, _},
		<<" c">>
	] = inline(<<"a +++b+++ c">>),
	[
		{monospaced, #{}, <<"a">>, _},
		<<" ">>,
		{passthrough, #{}, <<"b">>, _}
	] = inline(<<"++a++ +++b+++">>),
	[{monospaced, #{}, <<"+foo">>, _}] = inline(<<"+++foo++">>),
	[{monospaced, #{}, <<"foo">>, _}, <<"+">>] = inline(<<"++foo+++">>),
	%% A +++ span is removed before ++ is paired, even when
	%% the triple sits inside the doubled marks.
	[
		<<"see ">>,
		{monospaced, #{}, [
			<<"code ">>,
			{passthrough, #{}, <<"<b>raw</b>">>, _},
			<<" more">>
		], _},
		<<" please">>
	] = inline(<<"see ++code +++<b>raw</b>+++ more++ please">>),
	[<<"++foo ">>, {passthrough, #{}, <<"bar">>, _}]
		= inline(<<"++foo +++bar+++">>),
	[{monospaced, #{}, [
		<<" ">>,
		{passthrough, #{}, <<"bar">>, _},
		<<" ">>
	], _}] = inline(<<"++ +++bar+++ ++">>),
	%% A line break is a single + at the end of a line.
	[
		<<"Stay">>,
		{line_break, #{}, <<>>, _},
		<<"put">>
	] = inline(<<"Stay +\nput">>),
	ok.

%% +++text+++ is removed before any quote runs. Marks inside
%% it stay literal, including when the span sits in a quote.
passthrough_shield_test() ->
	[{strong, _, [
		<<"bold ">>,
		{passthrough, #{}, <<"*not*">>, _},
		<<" more">>
	], _}] = inline(<<"*bold +++*not*+++ more*">>),
	[{strong, _, [
		<<"bold ">>,
		{passthrough, #{}, <<"**not**">>, _},
		<<" more">>
	], _}] = inline(<<"**bold +++**not**+++ more**">>),
	[{inline_literal_passthrough, #{}, [
		<<"tick ">>,
		{passthrough, #{}, <<"`in`">>, _},
		<<" here">>
	], _}] = inline(<<"`tick +++`in`+++ here`">>),
	[{superscript, #{}, [
		<<"out ">>,
		{passthrough, #{}, <<"^in^">>, _},
		<<" tail">>
	], _}] = inline(<<"^out +++^in^+++ tail^">>),
	[{emphasized, _, [
		<<"em ">>,
		{passthrough, #{}, <<"__no__">>, _},
		<<" em">>
	], _}] = inline(<<"__em +++__no__+++ em__">>),
	[{monospaced, #{}, [
		<<" *bold ">>,
		{passthrough, #{}, <<"*x*">>, _},
		<<" more ">>
	], _}] = inline(<<"++ *bold +++*x*+++ more ++">>),
	[{emphasized, _, [
		<<"out ">>,
		{passthrough, #{}, <<"'in'">>, _},
		<<" tail">>
	], _}] = inline(<<"'out +++'in'+++ tail'">>),
	[{subscript, #{}, [
		<<"out ">>,
		{passthrough, #{}, <<"~in~">>, _},
		<<" tail">>
	], _}] = inline(<<"~out +++~in~+++ tail~">>),
	[{emphasized, _, [
		<<"em ">>,
		{passthrough, #{}, <<"_no_">>, _},
		<<" em">>
	], _}] = inline(<<"_em +++_no_+++ em_">>),
	[{strong, _, [{passthrough, #{}, <<"*not*">>, _}], _}]
		= inline(<<"*+++*not*+++*">>),
	ok.

constrained_mono_test() ->
	[{monospaced, #{}, <<"code">>, _}] = inline(<<"+code+">>),
	<<"+x+y">> = inline(<<"+x+y">>),
	<<"+code+_y">> = inline(<<"+code+_y">>),
	<<"+code+2">> = inline(<<"+code+2">>),
	<<"+code+é"/utf8>> = inline(<<"+code+é"/utf8>>),
	[{monospaced, #{}, <<"x+y">>, _}] = inline(<<"+x+y+">>),
	[
		{monospaced, #{}, <<"x+y">>, _},
		<<".">>
	] = inline(<<"+x+y+.">>),
	[{monospaced, #{}, <<"+">>, _}] = inline(<<"+++">>),
	[
		{monospaced, #{}, <<"x">>, _},
		<<"y">>
	] = inline(<<"++x++y">>),
	<<"See + not + here.">> = inline(<<"See + not + here.">>),
	<<"See +not + here.">> = inline(<<"See +not + here.">>),
	<<"See + not+ here.">> = inline(<<"See + not+ here.">>),
	<<"word+code+">> = inline(<<"word+code+">>),
	[
		<<"(">>,
		{monospaced, #{}, <<"code">>, _},
		<<")">>
	] = inline(<<"(+code+)">>),
	[
		{monospaced, #{}, <<"code">>, _},
		<<",">>
	] = inline(<<"+code+,">>),
	[{monospaced, #{}, <<"foo bar">>, _}] = inline(<<"+foo\nbar+">>),
	[{monospaced, #{}, [
		<<"code ">>,
		{strong, #{}, <<"bold">>, _}
	], _}] = inline(<<"+code *bold*+">>),
	[{strong, #{}, [{monospaced, #{}, <<"bold">>, _}], _}]
		= inline(<<"*+bold+*">>),
	[{monospaced, #{}, [
		<<"see ">>,
		{passthrough, #{}, <<"x">>, _},
		<<" more">>
	], _}] = inline(<<"+see +++x+++ more+">>),
	[{monospaced, #{}, [
		<<"foo">>,
		{line_break, #{}, <<>>, _},
		<<"bar">>
	], _}] = inline(<<"+foo +\nbar+">>),
	[
		<<"Stay">>,
		{line_break, #{}, <<>>, _},
		<<"put">>
	] = inline(<<"Stay +\nput">>),
	%% A finished ++ pair is not a constrained closer.
	[
		<<"see ">>,
		{monospaced, #{}, [
			<<"the ">>,
			{monospaced, #{}, <<"big">>, _},
			<<" end">>
		], _}
	] = inline(<<"see +the ++big++ end+">>),
	[
		<<"+a">>,
		{monospaced, #{}, <<"b">>, _},
		<<"c">>
	] = inline(<<"+a++b++c">>),
	[<<"+foo">>, {monospaced, #{}, <<"bar">>, _}]
		= inline(<<"+foo++bar++">>),
	[<<"+C">>, {monospaced, #{}, <<" code">>, _}]
		= inline(<<"+C++ code++">>),
	[{monospaced, #{}, [
		<<"foo">>,
		{monospaced, #{}, <<"bar">>, _},
		<<"baz">>
	], _}] = inline(<<"+foo++bar++baz+">>),
	[{monospaced, #{}, <<"C">>, _}, <<"+ code+">>]
		= inline(<<"+C++ code+">>),
	[{monospaced, #{}, <<"foo">>, _}, <<"+bar+">>]
		= inline(<<"+foo++bar+">>),
	[
		{monospaced, #{}, <<"x">>, _},
		{monospaced, #{}, <<"code">>, _}
	] = inline(<<"++x+++code+">>),
	[<<"x+">>, {monospaced, #{}, <<"code">>, _}]
		= inline(<<"x++code+">>),
	%% Punctuation bounds a constrained quote. ";" ":" "}" do not.
	[<<"\"">>, {monospaced, #{}, <<"code">>, _}, <<"\"">>]
		= inline(<<"\"+code+\"">>),
	[<<"end.">>, {monospaced, #{}, <<"code">>, _}]
		= inline(<<"end.+code+">>),
	[{monospaced, #{}, <<"init">>, _}, <<".">>,
		{monospaced, #{}, <<"loop">>, _}]
		= inline(<<"+init+.+loop+">>),
	<<":+code+:">> = inline(<<":+code+:">>),
	<<"foo:+bar+">> = inline(<<"foo:+bar+">>),
	<<";+code+">> = inline(<<";+code+">>),
	<<"}+code+">> = inline(<<"}+code+">>),
	%% "_" is a word character, so "+" does not open inside it.
	[{emphasized, #{}, <<"+a+">>, _}] = inline(<<"_+a+_">>),
	[{emphasized, #{}, <<"+a+">>, _}] = inline(<<"__+a+__">>),
	[{emphasized, #{}, <<"foo">>, _}, <<"+bar+">>]
		= inline(<<"_foo_+bar+">>),
	[{strong, #{}, [{monospaced, #{}, <<"a">>, _}], _}]
		= inline(<<"**+a+**">>),
	[
		{strong, #{}, <<"bar">>, _},
		{monospaced, #{}, <<"foo">>, _}
	] = inline(<<"*bar*+foo+">>),
	[
		{monospaced, #{}, <<"foo">>, _},
		{strong, #{}, <<"bar">>, _}
	] = inline(<<"+foo+*bar*">>),
	ok.
-endif.

%% Asciidoc User Guide 21.4
%%
%% The closing backtick is not one inside +++text+++.
%% That span is copied through and is not quote text.
inline_literal_passthrough(<<"`", Rest0/bits>>, Prev) when ?IS_BOUNDARY(Prev) ->
	{Content, Rest} = take_constrained(Rest0, $`, plus_passthroughs(Rest0), 0),
	{ok, {inline_literal_passthrough, #{}, splice_passthroughs(Content), inline}, Rest}.

-ifdef(TEST).
inline_literal_passthrough_test() ->
	[
		<<"Word phrases ">>,
		{inline_literal_passthrough, #{}, <<"enclosed in backtick characters">>, _},
		<<" (grave accents)...">>
	] = inline(<<"Word phrases `enclosed in backtick characters` (grave accents)...">>),
	ok.
-endif.

-define(IS_WS(C), (C =:= $\s) or (C =:= $\t)).

%% Asciidoc User Guide 10.3
%% A space and "+" at the end of a line is a break.
%% The end of the outermost text counts, including a
%% trailing CR left from a CRLF line. Quote bodies do
%% not, so a constrained quote may end in " +".
line_break(<<WS, "+", Rest0/bits>>, EolFlag) when ?IS_WS(WS) ->
	{Eol, Break, Rest} = case while(fun(C) ->
		(C =/= $\r) andalso (C =/= $\n)
	end, Rest0) of
		{Eol0, <<"\r\n", Rest1/bits>>} ->
			{Eol0, <<"\n">>, Rest1};
		{Eol0, <<"\n", Rest1/bits>>} ->
			{Eol0, <<"\n">>, Rest1};
		{Eol0, <<"\r">>} when EolFlag ->
			{Eol0, <<>>, <<>>};
		{Eol0, <<>>} when EolFlag ->
			{Eol0, <<>>, <<>>};
		_ ->
			error(nomatch)
	end,
	<<>> = trim(Eol),
	%% Verse is preformatted. The break is an extra <br />,
	%% and the line break itself stays in the text.
	Kept = case get(?KEEP_NL) of
		true -> <<Break/binary, Rest/binary>>;
		_ -> Rest
	end,
	{ok, {line_break, #{}, <<>>, inline}, Kept}.

-ifdef(TEST).
line_break_test() ->
	[
		<<"Plus at the end of the line">>,
		{line_break, #{}, <<>>, inline},
		<<"should work">>
	] = inline(<<"Plus at the end of the line +\nshould work">>),
	[
		<<"Plus at the end of the line   ">>,
		{line_break, #{}, <<>>, inline},
		<<"should work">>
	] = inline(<<"Plus at the end of the line    +\nshould work">>),
	[
		<<"Plus at the end of the line">>,
		{line_break, #{}, <<>>, inline},
		<<"should work">>
	] = inline(<<"Plus at the end of the line +\r\nshould work">>),
	<<"Plus in the middle + should not.">>
		= inline(<<"Plus in the middle + should not.">>),
	[
		<<"ends with">>,
		{line_break, #{}, <<>>, _}
	] = inline(<<"ends with +">>),
	[
		<<"line one line two">>,
		{line_break, #{}, <<>>, _}
	] = inline(<<"line one\nline two +">>),
	[
		<<"line one line two">>,
		{line_break, #{}, <<>>, _}
	] = inline(<<"line one\r\nline two +\r">>),
	ok.

quote_verse_test() ->
	[{strong, _, <<"bold still">>, _}] = inline(<<"*bold\nstill*">>),
	[{strong, _, <<"bold\nstill">>, _}]
		= inline_verse(<<"*bold\nstill*">>),
	<<"*bold\n*">> = inline_verse(<<"*bold\n*">>),
	<<"one\n\ntwo">> = verse_text(<<"\none\n// hidden\ntwo\n\n">>),
	<<"one\n\nthree">> = verse_text(<<"one\n//\ntwo\nthree">>),
	<<"one\n\nthree">> = verse_text(<<"one\n//   \ntwo\nthree">>),
	<<"one\n/// text\ntwo">> = verse_text(<<"one\n/// text\ntwo">>),
	<<"one\n//\n// hidden\nthree">> = verse_text(<<"one\n\\//\n// hidden\nthree">>),
	<<"// hidden">> = verse_text(<<"\\// hidden">>),
	[<<"one">>, {line_break, _, _, _}, <<"\ntwo">>]
		= inline_verse(<<"one +\ntwo">>),
	<<"  indented\ntail">> = verse_text(<<"\n  indented\ntail  \n">>),
	<<"line\n\n.Title">> = verse_text(<<"\nline\n\n.Title\n">>),
	[{quote_block, #{1 := <<"verse">>}, <<"one\n\ntwo">>, Ann}] = run([
		{quote_block, #{1 => <<"verse">>}, [
			{paragraph, #{}, <<"ignored">>, #{}}
		], #{raw => <<"one\n// hidden\ntwo">>}}
	]),
	false = maps:is_key(raw, Ann),
	[{quote_block, #{}, [
		{paragraph, #{}, [{strong, _, <<"world">>, _}], _}
	], NoRaw}] = run([
		{quote_block, #{}, [
			{paragraph, #{}, <<"*world*">>, #{}}
		], #{raw => <<"unused">>}}
	]),
	false = maps:is_key(raw, NoRaw),
	[{quote_block, #{<<"style">> := <<"verse">>}, <<"one\ntwo">>, _}] = run([
		{quote_block, #{<<"style">> => <<"verse">>}, [],
			#{raw => <<"one\ntwo">>}}
	]),
	[{quote_block, #{1 := <<"quote">>}, [
		{paragraph, #{}, <<"words">>, _}
	], NoRaw2}] = run([
		{quote_block, #{1 => <<"quote">>, <<"style">> => <<"verse">>}, [
			{paragraph, #{}, <<"words">>, #{}}
		], #{raw => <<"one\ntwo">>}}
	]),
	false = maps:is_key(raw, NoRaw2),
	ok.

comment_block_test() ->
	[{comment_block, _, <<"hidden *stars*">>, _},
		{paragraph, _, [{strong, _, <<"bold">>, _}], _}] = run([
		{comment_block, #{}, <<"hidden *stars*">>, #{line => 1}},
		{paragraph, #{}, <<"*bold*">>, #{line => 2}}
	]),
	ok.
-endif.
