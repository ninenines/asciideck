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

-import(asciideck_block_parser, [trim/1, while/2]).

-type inline_ast() :: list(). %% @todo
-export_type([inline_ast/0]).

run([]) ->
	[];
run([Data|Tail]) when is_binary(Data) ->
	[inline(Data)|run(Tail)];
%% We do not do any inline formatting for verbatim blocks,
%% for example listing blocks.
%%
%% @todo subs is a list of values.
run([Item={_, #{<<"subs">> := <<"verbatim">>}, _, _}|Tail]) ->
	[Item|run(Tail)];
%% Labeled lists' labels can also have inline formatting.
run([{Type, Attrs=#{label := Label}, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs#{label => inline(Label)}, run(Items), Ann}|run(Tail)];
run([{Type, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs, run(Items), Ann}|run(Tail)];
run([{Type, Attrs, Data, Ann}|Tail]) ->
	[{Type, Attrs, inline(Data), Ann}|run(Tail)].

%% We reduce inline content with a single text element
%% with no formatting to a simple binary.
inline(<<>>) ->
	<<>>;
inline(Data) ->
	case inline(Data, <<>>, []) of
		[] -> <<>>;
		[Text] when is_binary(Text) -> Text;
		AST -> AST
	end.

-spec inline(binary(), binary(), inline_ast()) -> inline_ast().
inline(<<>>, <<>>, Acc) ->
	lists:reverse(Acc);
inline(<<>>, BinAcc, Acc) ->
	lists:reverse([BinAcc|Acc]);
inline(Data, BinAcc, Acc) ->
	oneof(Data, BinAcc, Acc, [
		%% Links.
		fun xref/2,
		fun link/2,
		fun http_link/2,
		fun https_link/2,
		fun mailto_link/2,
		%% Quoted text. Unconstrained forms are doubled marks
		%% and are tried before the constrained forms.
		fun unconstrained_strong/2,
		fun unconstrained_emphasis/2,
		%% +++ is a passthrough and is taken before ++.
		fun passthrough_plus/2,
		fun unconstrained_mono/2,
		fun superscript/2,
		fun subscript/2,
		fun emphasized_single_quote/2,
		fun emphasized_underline/2,
		fun strong/2,
		%% Passthrough macros.
		fun inline_literal_passthrough/2,
		%% Line breaks.
		fun line_break/2
	]).

%% The inline pass replaces \r\n and \n with a simple space
%% when it occurs within normal text.
oneof(<<$\r, $\n, Rest/bits>>, BinAcc, Acc, []) ->
	inline(Rest, <<BinAcc/binary, $\s>>, Acc);
oneof(<<$\n, Rest/bits>>, BinAcc, Acc, []) ->
	inline(Rest, <<BinAcc/binary, $\s>>, Acc);
oneof(<<C, Rest/bits>>, BinAcc, Acc, []) ->
	inline(Rest, <<BinAcc/binary, C>>, Acc);
oneof(Data, BinAcc, Acc, [Parse|Tail]) ->
	Prev = case BinAcc of
		<<>> -> undefined;
		_ -> binary:last(BinAcc)
	end,
	try Parse(Data, Prev) of
		{ok, Inline, Rest} when BinAcc =:= <<>> ->
			inline(Rest, BinAcc, [Inline|Acc]);
		{ok, Inline, Rest} ->
			inline(Rest, <<>>, [Inline, BinAcc|Acc]);
		{skip, Text, Rest} ->
			oneof(Rest, <<BinAcc/binary, Text/binary>>, Acc, Tail)
	catch _:_ ->
		oneof(Data, BinAcc, Acc, Tail)
	end.

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
%% @todo A leading backslash does not yet suppress the quote.
unconstrained_strong(Data, _) ->
	unconstrained(Data, strong, $*).
unconstrained_emphasis(Data, _) ->
	unconstrained(Data, emphasized, $_).
%% User Guide passthrough macros. +++text+++ is copied
%% through with no substitutions. It is not a ++ quote.
%% +++foo++ has no closing triple, so ++ may match it.
passthrough_plus(<<"+++", Rest0/bits>>, _) ->
	{Pos, _} = binary:match(Rest0, <<"+++">>),
	<<Content:Pos/binary, _:3/binary, Rest/bits>> = Rest0,
	{ok, {passthrough, #{}, Content, inline}, Rest}.

unconstrained_mono(Data, _) ->
	unconstrained(Data, monospaced, $+).

unconstrained(<<C, C, _/bits>> = Data, Type, C) ->
	[{0, End}|_] = quote_spans(Data, C),
	ContentLen = End - 4,
	<<_:2/binary, Content:ContentLen/binary, _:2/binary, Rest/bits>> = Data,
	{ok, {Type, #{
		left => C,
		right => C
	}, inline(Content), inline}, Rest}.

%% Doubled marks are applied to the whole paragraph before
%% the single mark of the same character. A constrained
%% quote therefore does not close on either half of "**"
%% or "__". "2 * 3 = **six**." leaves the single * literal.
%%
%% +++text+++ is removed before any of those marks are
%% paired. The pluses and the bytes inside are not marks.
quote_spans(Bin, C) ->
	quote_spans(Bin, C, 0, plus_passthroughs(Bin)).

quote_spans(Bin, C, From, Skip) ->
	case find_mark(Bin, C, From, Skip) of
		nomatch ->
			[];
		Open ->
			case find_closer(Bin, C, Open, Skip) of
				nomatch ->
					quote_spans(Bin, C, Open + 1, Skip);
				Close ->
					[{Open, Close + 2}|quote_spans(Bin, C, Close + 2, Skip)]
			end
	end.

%% The closer is the first doubled mark that leaves at least
%% one byte of content. An adjacent mark is content:
%% "*****" is strong of a single "*". "****" is not a quote.
find_closer(Bin, C, Open, Skip) ->
	find_closer(Bin, C, Open, Open + 2, Skip).

find_closer(Bin, C, Open, From, Skip) ->
	case find_mark(Bin, C, From, Skip) of
		nomatch ->
			nomatch;
		Close when Close > Open + 2 ->
			Close;
		Close ->
			find_closer(Bin, C, Open, Close + 1, Skip)
	end.

find_mark(Bin, C, From, Skip) ->
	Size = byte_size(Bin),
	case From > Size - 2 of
		true ->
			nomatch;
		false ->
			case binary:match(Bin, <<C, C>>, [{scope, {From, Size - From}}]) of
				nomatch ->
					nomatch;
				{Pos, 2} ->
					Inside = case in_span(Pos, Skip) of
						true -> Pos;
						false -> Pos + 1
					end,
					case in_span(Inside, Skip) of
						true ->
							find_mark(Bin, C, skip_end(Inside, Skip), Skip);
						false ->
							Pos
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
superscript(<<$^, Rest/bits>>, _) ->
	single_mark(Rest, $^, superscript).
subscript(<<$~, Rest/bits>>, _) ->
	single_mark(Rest, $~, subscript).

single_mark(Rest, C, Type) ->
	Pos = single_closer(Rest, C, 0, plus_passthroughs(Rest)),
	<<Content:Pos/binary, _:1/binary, Tail/bits>> = Rest,
	{ok, {Type, #{}, inline(Content), inline}, Tail}.

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
%% @todo <<"\\*"
emphasized_single_quote(Data, Prev) ->
	quoted_text(Data, Prev, emphasized, $', $').
emphasized_underline(Data, Prev) ->
	quoted_text(Data, Prev, emphasized, $_, $_).
strong(Data, Prev) ->
	quoted_text(Data, Prev, strong, $*, $*).

quoted_text(<<Left, Rest0/bits>>, Prev, Type, Left, Right) when ?IS_BOUNDARY(Prev) ->
	{Content, Rest} = constrained_content(Rest0, Left),
	%% "**" is not strong. The content has to be non-empty.
	true = Content =/= <<>>,
	{ok, {Type, #{
		left => Left,
		right => Right
	}, inline(Content), inline}, Rest}.

%% * and _ have a doubled form. Skip those spans when
%% looking for the closing mark. +++text+++ is not text
%% for any quote, so those spans are skipped too.
constrained_content(Bin, C) when C =:= $*; C =:= $_ ->
	Passes = plus_passthroughs(Bin),
	Doubled = quote_spans(Bin, C, 0, Passes),
	take_constrained(Bin, C, lists:sort(Doubled ++ Passes), 0);
constrained_content(Bin, C) ->
	take_constrained(Bin, C, plus_passthroughs(Bin), 0).

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
	<<"*** Three.">> = inline(<<"*** Three.">>),
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
	<<"****">> = inline(<<"****">>),
	<<"__">> = inline(<<"__">>),
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
	<<"++++">> = inline(<<"++++">>),
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
line_break(<<WS, "+", Rest0/bits>>, _) when ?IS_WS(WS) ->
	{Eol, Rest} = case while(fun(C) -> (C =/= $\r) andalso (C =/= $\n) end, Rest0) of
		{Eol0, <<"\r\n", Rest1/bits>>} -> {Eol0, Rest1};
		{Eol0, <<"\n", Rest1/bits>>} -> {Eol0, Rest1};
		Tuple -> Tuple
	end,
	<<>> = trim(Eol),
	{ok, {line_break, #{}, <<>>, inline}, Rest}.

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
	ok.
-endif.
