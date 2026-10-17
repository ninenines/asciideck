%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(asciideck_to_html).

-export([translate/2]).

translate(AST, Opts) ->
	Output0 = ast(AST),
	Output1 = maybe_header_footer(Output0, Opts),
	{CompressExt, Output} = case Opts of
		#{compress := gzip} -> {".gz", zlib:gzip(Output1)};
		_ -> {"", Output1}
	end,
	case Opts of
		#{outdir := Path, outfile := Filename} ->
			file:write_file(binary_to_list(iolist_to_binary(
				[Path, "/", Filename, ".html", CompressExt])), Output);
		#{outdir := Path} ->
			Filename = filename_from_ast(AST),
			file:write_file(binary_to_list(iolist_to_binary(
				[Path, "/", Filename, ".html", CompressExt])), Output);
		_ ->
			Output
	end.

maybe_header_footer(Body, #{no_header_footer := _}) ->
	Body;
maybe_header_footer(Body, _Opts) ->
	[
		"<!DOCTYPE html>\n"
		"<html lang=\"en\">\n"
		"<head>\n"
		"<meta charset=\"utf-8\"/>\n"
		"<title>TODO title</title>\n" %% @todo
		"</head>\n"
		"<body>\n",
		Body,
		"</body>\n"
		"</html>\n"
	].

filename_from_ast([{section_title, #{level := 0}, Filename, _}|_]) ->
	Filename.

%% Loop over all types of AST nodes.

ast(AST) ->
	fold(AST, fun ast_node/1).

fold(AST, Fun) ->
	lists:reverse(lists:foldl(
		fun(Node, Acc) -> [Fun(Node)|Acc] end,
		[], AST)).

ast_node(Node={Type, _, _, _}) ->
	try
		case Type of
			section_title -> section_title(Node);
			paragraph -> paragraph(Node);
			admonition -> admonition(Node);
			literal_paragraph -> literal_paragraph(Node);
			literal_block -> literal_paragraph(Node);
			listing_block -> listing_block(Node);
			passthrough_block -> passthrough_block(Node);
			open_block -> open_block(Node);
			sidebar_block -> sidebar_block(Node);
			quote_block -> quote_block(Node);
			example_block -> example_block(Node);
			list -> list(Node);
			table -> table(Node);
			block_macro -> block_macro(Node);
			comment_line -> comment_line(Node);
			comment_block -> [];
			attribute_entry -> [];
			_ -> ast_error({unknown_type, Node})
		end
	catch C:E:S ->
		ast_error({crash, C, E, S, Node})
	end.

ast_error(Error) ->
	[
		"<p class=\"asciideck-error\">",
		html_encode(unicode:characters_to_binary(io_lib:format("~p", [Error]))),
		"</p>"
	].

%% Section titles.

section_title({section_title, Attrs=#{level := Level}, Title, _}) ->
	LevelC = $1 + Level,
	ID = case Attrs of
		#{<<"id">> := ID0} -> ID0;
		_ -> id_from_title(Title)
	end,
	["<h", LevelC, " id=\"", ID, "\">", inline(Title), "</h", LevelC, ">\n"].

%% Asciidoc User Guide 8.4.2
%% @todo Handle cases where the title is repeated in the same document.
id_from_title(Title) ->
	ID0 = unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(Title))),
	ID1 = <<if
		C >= $a, C =< $z -> <<C/utf8>>;
		C >= $0, C =< $9 -> <<C/utf8>>;
		true -> <<$_>>
	end || <<C/utf8>> <= ID0>>,
	ID = string:strip(unicode:characters_to_list(ID1), both, $_),
	[$_, unicode:characters_to_binary(ID)].

%% Paragraphs.

paragraph({paragraph, _, Text, _}) ->
	["<p>", inline(Text), "</p>\n"].

%% Admonition paragraphs.

admonition({admonition, Attrs=#{style := Style}, Text, _}) ->
	Class = string:to_lower(binary_to_list(Style)),
	[
		"<div class=\"admonitionblock ", Class, "\">",
		"<div class=\"title\">", admonition_label(Style), "</div>\n",
		"<div class=\"content\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		admonition_body(Text),
		"</div></div>\n"
	].

%% A paragraph admonition is text. An open-block admonition
%% is a list of blocks. Inline nodes are not blocks.
admonition_body(AST=[{Type, _, _, _}|_])
		when Type =/= link, Type =/= xref, Type =/= emphasized,
			Type =/= strong, Type =/= inline_literal_passthrough,
			Type =/= superscript, Type =/= subscript,
			Type =/= monospaced, Type =/= passthrough,
			Type =/= line_break ->
	ast(AST);
admonition_body(Text) ->
	inline(Text).

admonition_label(<<C, Rest/bits>>) ->
	[string:to_upper([C]), string:to_lower(binary_to_list(Rest))].

%% Literal paragraphs and delimited literal blocks.

literal_paragraph({Type, Attrs, Text, _})
		when Type =:= literal_paragraph; Type =:= literal_block ->
	[
		"<div class=\"literalblock\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		"<div class=\"content\"><pre>", html_encode(Text), "</pre></div></div>\n"
	].

%% Listing blocks.

listing_block({listing_block, Attrs, Listing0, _}) ->
	Listing = case Attrs of
		#{1 := <<"source">>, 2 := _} ->
			case asciideck_source_highlight:filter(Listing0, Attrs) of
				{ok, Highlighted} ->
					Highlighted;
				error ->
					verbatim_listing(Listing0)
			end;
		_ ->
			verbatim_listing(Listing0)
	end,
	[
		"<div class=\"listingblock\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		"<div class=\"content\">",
		Listing,
		"</div></div>\n"
	].

verbatim_listing(Listing) ->
	["<pre>", html_encode(Listing), "</pre>"].

%% Passthrough blocks.

passthrough_block({passthrough_block, _, HTML, _}) ->
	HTML.

%% Sidebar blocks. The title sits inside the content.
sidebar_block({sidebar_block, Attrs, Children, _}) ->
	[
		"<div class=\"sidebarblock\">",
		"<div class=\"content\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		ast(Children),
		"</div></div>\n"
	].

%% Quote and verse blocks. The title sits before the body.
%% Attribution is positional attributes 2 and 3.

quote_block({quote_block, Attrs, Body, _}) ->
	case quote_style(Attrs) of
		<<"verse">> ->
			[
				"<div class=\"verseblock\">\n",
				block_title(Attrs),
				"<pre class=\"content\">", inline(Body), "</pre>\n",
				attribution(Attrs),
				"</div>\n"
			];
		_ ->
			[
				"<div class=\"quoteblock\">\n",
				block_title(Attrs),
				"<div class=\"content\">\n",
				ast(Body),
				"</div>\n",
				attribution(Attrs),
				"</div>\n"
			]
	end.

block_title(#{<<"title">> := Title}) ->
	["<div class=\"title\">", inline(Title), "</div>\n"];
block_title(_) ->
	[].

attribution(Attrs) ->
	[
		"<div class=\"attribution\">\n",
		attribution_body(quote_value(Attrs, 3, <<"citetitle">>),
			quote_value(Attrs, 2, <<"attribution">>)),
		"</div>\n"
	].

attribution_body(none, none) ->
	[];
attribution_body(Cite, none) ->
	["<em>", inline(Cite), "</em>\n"];
attribution_body(none, Author) ->
	["&#8212; ", inline(Author), "\n"];
attribution_body(Cite, Author) ->
	["<em>", inline(Cite), "</em><br />\n&#8212; ", inline(Author), "\n"].

%% Positional attribute 1 is the style when that key was written.
%% Otherwise the named style attribute is. The default is quote.
quote_style(Attrs) ->
	case maps:is_key(1, Attrs) of
		true ->
			maps:get(1, Attrs);
		false ->
			maps:get(<<"style">>, Attrs, <<"quote">>)
	end.

%% A positional attribute is written over a named one.
%% An empty positional value is absent. Named is used
%% only when that position was not in the list.
quote_value(Attrs, Pos, Name) ->
	case maps:is_key(Pos, Attrs) of
		true ->
			case maps:get(Pos, Attrs) of
				<<>> -> none;
				Value -> Value
			end;
		false ->
			case maps:get(Name, Attrs, <<>>) of
				<<>> -> none;
				Value -> Value
			end
	end.

%% Example blocks. The title sits before the body.
%% A caption prefixes that title. It is not a title alone.
%% Examples are not numbered.
example_block({example_block, Attrs, Children, _}) ->
	[
		"<div class=\"exampleblock\">\n",
		example_title(Attrs),
		"<div class=\"content\">\n",
		ast(Children),
		"</div></div>\n"
	].

example_title(#{<<"title">> := Title}=Attrs) ->
	["<div class=\"title\">", inline(example_caption(Attrs)),
		inline(Title), "</div>\n"];
example_title(_) ->
	[].

%% The caption attribute includes any separator it wants.
example_caption(#{<<"caption">> := Caption}) when Caption =/= <<>> ->
	Caption;
example_caption(_) ->
	<<>>.

%% Open blocks.

open_block({open_block, Attrs, Children, _}) ->
	Class = case Attrs of
		#{1 := Style} ->
			["openblock ", string:to_lower(binary_to_list(Style))];
		_ ->
			"openblock"
	end,
	[
		"<div class=\"", Class, "\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		"<div class=\"content\">",
		ast(Children),
		"</div></div>\n"
	].

%% Lists.

list({list, #{type := bulleted}, Items, _}) ->
	["<ul>", fold(Items, fun list_item/1), "</ul>\n"];
list({list, #{type := numbered}, Items, _}) ->
	["<ol>", fold(Items, fun list_item/1), "</ol>\n"];
list({list, #{type := labeled}, Items, _}) ->
	["<dl>", fold(Items, fun labeled_list_item/1), "</dl>\n"];
list({list, #{type := callout}, Items, _}) ->
	["<div class=\"colist\"><ol>", fold(Items, fun list_item/1), "</ol></div>\n"].

list_item({list_item, _, [{paragraph, _, Text, _}|AST], _}) ->
	[
		"<li>",
		inline(Text), "\n",
		ast(AST),
		"</li>\n"
	].

labeled_list_item({list_item, #{label := Label}, AST, _}) ->
	[
		"<dt>", inline(Label), "</dt>\n",
		"<dd>",
		ast(AST),
		"</dd>\n"
	].

%% Tables.

table({table, Attrs, [{row, _, Head, _}|Rows], _}) ->
	[
		"<table rules=\"all\" width=\"100%\" frame=\"border\"
			cellspacing=\"0\" cellpadding=\"4\">\n",
		case Attrs of
			#{<<"title">> := Caption} -> ["<caption>", inline(Caption), "</caption>"];
			_ -> []
		end,
		"<thead><tr>", table_head(Head), "</tr></thead>"
		"<tbody>", table_body(Rows), "</tbody>"
		"</table>\n"
	].

table_head(Cells) ->
	[["<th>", table_cell(AST), "</th>\n"]
		|| {cell, _, AST, _} <- Cells].

table_body(Rows) ->
	[["<tr>", table_body_cells(Cells), "</tr>\n"]
		|| {row, _, Cells, _} <- Rows].

table_body_cells(Cells) ->
	[["<td>", table_cell(AST), "</td>\n"]
		|| {cell, _, AST, _} <- Cells].

table_cell(AST0) ->
	AST = [Node || Node={Type, _, _, _} <- AST0,
		Type =/= comment_line, Type =/= comment_block],
	case AST of
		[{paragraph, _, Text, _}] ->
			inline(Text);
		_ ->
			ast(AST)
	end.

%% Block macros.

block_macro({block_macro, #{name := <<"image">>,
		target := Target, 1 := Caption}, _, _}) ->
	["<img src=\"", html_encode(Target), "\" "
		"alt=\"", html_encode(Caption), "\"/>"].

%% Comment lines are printed in the generated file
%% but are not visible in viewers.

comment_line({comment_line, _, Text, _}) ->
	["<!-- ", html_encode(Text), "-->\n"].

%% Inline formatting.

inline(Text) when is_binary(Text) ->
	html_encode(Text);
inline({link, #{target := Target}, Text, _}) ->
	["<a href=\"", html_encode(Target), "\">", html_encode(Text), "</a>"];
inline({xref, #{id := ID}, Text, _}) ->
	["<a href=\"#", html_encode(ID), "\">", html_encode(Text), "</a>"];
inline({emphasized, _, Text, _}) ->
	["<em>", inline(Text), "</em>"];
inline({strong, _, Text, _}) ->
	["<strong>", inline(Text), "</strong>"];
inline({inline_literal_passthrough, _, Text, _}) ->
	["<code>", inline(Text), "</code>"];
inline({superscript, _, Text, _}) ->
	["<sup>", inline(Text), "</sup>"];
inline({subscript, _, Text, _}) ->
	["<sub>", inline(Text), "</sub>"];
inline({monospaced, _, Text, _}) ->
	["<code>", inline(Text), "</code>"];
%% +++passthrough+++ is copied through with no substitutions.
inline({passthrough, _, Text, _}) ->
	Text;
inline({line_break, _, _, _}) ->
	"<br/>";
inline(Text) when is_list(Text) ->
	[inline(T) || T <- Text].

html_encode(Text) ->
	<<case C of
		$& -> <<"&amp;">>;
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$" -> <<"&quot;">>;
		$' -> <<"&apos;">>;
		_ -> <<C/utf8>>
	end || <<C/utf8>> <= Text>>.

-ifdef(TEST).
admonition_test() ->
	AST = [{admonition, #{style => <<"NOTE">>}, <<"Be careful.">>, #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"admonitionblock note\">"
		"<div class=\"title\">Note</div>\n"
		"<div class=\"content\">Be careful.</div></div>\n">> = HTML,
	ok.

callout_list_test() ->
	AST = [{list, #{type => callout}, [
		{list_item, #{type => callout, number => 1},
			[{paragraph, #{}, <<"First.">>, #{line => 1}}], #{line => 1}}
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"colist\"><ol><li>First.\n</li>\n</ol></div>\n">> = HTML,
	ok.

open_block_test() ->
	AST = [{open_block, #{}, [
		{paragraph, #{}, <<"Hello!">>, #{line => 2}}
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"openblock\"><div class=\"content\">"
		"<p>Hello!</p>\n"
		"</div></div>\n">> = HTML,
	ok.

sidebar_block_test() ->
	AST = [{sidebar_block, #{<<"title">> => <<"An Example Sidebar">>}, [
		{paragraph, #{}, <<"Outside the narrative.">>, #{line => 2}}
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"sidebarblock\"><div class=\"content\">"
		"<div class=\"title\">An Example Sidebar</div>\n"
		"<p>Outside the narrative.</p>\n"
		"</div></div>\n">> = HTML,
	ok.

attribute_entry_test() ->
	AST = [
		{attribute_entry, #{name => <<"author">>, unset => false},
			<<"Loic">>, #{line => 1}},
		{paragraph, #{}, <<"Hello.">>, #{line => 2}}
	],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<p>Hello.</p>\n">> = HTML,
	ok.

literal_paragraph_test() ->
	AST = [{literal_paragraph, #{<<"subs">> => <<"verbatim">>},
		<<"Hello *world*!">>, #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"literalblock\">"
		"<div class=\"content\"><pre>Hello *world*!</pre></div></div>\n">> = HTML,
	ok.

admonition_title_test() ->
	AST = [{admonition, #{
		style => <<"WARNING">>,
		<<"title">> => <<"Watch out">>
	}, <<"Be careful.">>, #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"admonitionblock warning\">"
		"<div class=\"title\">Warning</div>\n"
		"<div class=\"content\">"
		"<div class=\"title\">Watch out</div>\n"
		"Be careful.</div></div>\n">> = HTML,
	ok.

passthrough_test() ->
	AST = asciideck:parse("+++<b>raw</b>+++"),
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<p><b>raw</b></p>\n">> = HTML,
	Nested = asciideck:parse("see ++code +++<b>raw</b>+++ more++ please"),
	NestedHTML = iolist_to_binary(translate(Nested, #{no_header_footer => true})),
	<<"<p>see <code>code <b>raw</b> more</code> please</p>\n">> = NestedHTML,
	Shield = asciideck:parse("*bold +++*not*+++ more*"),
	ShieldHTML = iolist_to_binary(translate(Shield, #{no_header_footer => true})),
	<<"<p><strong>bold *not* more</strong></p>\n">> = ShieldHTML,
	Mono = asciideck:parse("++ *bold +++*x*+++ more ++"),
	MonoHTML = iolist_to_binary(translate(Mono, #{no_header_footer => true})),
	<<"<p><code> *bold *x* more </code></p>\n">> = MonoHTML,
	Tick = asciideck:parse("`tick +++`in`+++ here`"),
	TickHTML = iolist_to_binary(translate(Tick, #{no_header_footer => true})),
	<<"<p><code>tick `in` here</code></p>\n">> = TickHTML,
	Sup = asciideck:parse("^out +++^in^+++ tail^"),
	SupHTML = iolist_to_binary(translate(Sup, #{no_header_footer => true})),
	<<"<p><sup>out ^in^ tail</sup></p>\n">> = SupHTML,
	ok.

monospaced_test() ->
	AST = [{paragraph, #{}, [
		<<"Use ">>,
		{monospaced, #{}, <<"ok">>, inline},
		<<".">>
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<p>Use <code>ok</code>.</p>\n">> = HTML,
	Code = asciideck:parse("Use +lists:reverse/1+."),
	CodeHTML = iolist_to_binary(translate(Code, #{no_header_footer => true})),
	<<"<p>Use <code>lists:reverse/1</code>.</p>\n">> = CodeHTML,
	Literal = asciideck:parse("See +x+y here."),
	LiteralHTML = iolist_to_binary(translate(Literal, #{no_header_footer => true})),
	<<"<p>See +x+y here.</p>\n">> = LiteralHTML,
	ok.

super_sub_test() ->
	AST = [{paragraph, #{}, [
		<<"H">>,
		{subscript, #{}, <<"2">>, inline},
		<<"O and x">>,
		{superscript, #{}, <<"10">>, inline}
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<p>H<sub>2</sub>O and x<sup>10</sup></p>\n">> = HTML,
	ok.

admonition_inline_test() ->
	AST = [{admonition, #{style => <<"NOTE">>}, [
		<<"Be ">>,
		{strong, #{}, <<"bold">>, inline},
		<<".">>
	], #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"admonitionblock note\">"
		"<div class=\"title\">Note</div>\n"
		"<div class=\"content\">Be <strong>bold</strong>.</div></div>\n">> = HTML,
	ok.

open_listing_test() ->
	AST = asciideck:parse(
		"[listing]\n"
		"--\n"
		"f() -> ok.\n"
		"--\n"),
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"listingblock\">"
		"<div class=\"content\">"
		"<pre>f() -&gt; ok.</pre>"
		"</div></div>\n">> = HTML,
	ok.

source_highlight_fallback_test() ->
	AST = [{listing_block, #{
		1 => <<"source">>,
		2 => <<"erlang;touch /tmp/x">>
	}, <<"a < b">>, #{line => 1}}],
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"listingblock\">"
		"<div class=\"content\">"
		"<pre>a &lt; b</pre>"
		"</div></div>\n">> = HTML,
	ok.

open_note_test() ->
	AST = asciideck:parse(
		"[NOTE]\n"
		"--\n"
		"Hello *bold*.\n"
		"\n"
		"* One.\n"
		"--\n"),
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"admonitionblock note\">"
		"<div class=\"title\">Note</div>\n"
		"<div class=\"content\">"
		"<p>Hello <strong>bold</strong>.</p>\n"
		"<ul><li>One.\n</li>\n</ul>\n"
		"</div></div>\n">> = HTML,
	ok.

quote_block_test() ->
	Plain = asciideck:parse(
		"____\n"
		"Hello *world*.\n"
		"\n"
		"Second.\n"
		"____\n"),
	PlainHTML = iolist_to_binary(translate(Plain, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Hello <strong>world</strong>.</p>\n"
		"<p>Second.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = PlainHTML,
	Both = asciideck:parse(
		".My Quote\n"
		"[quote, Ann, Book]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	BothHTML = iolist_to_binary(translate(Both, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"title\">My Quote</div>\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = BothHTML,
	Author = asciideck:parse(
		"[quote, Ann]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	AuthorHTML = iolist_to_binary(translate(Author, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = AuthorHTML,
	Cite = asciideck:parse(
		"[quote, , The Book]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	CiteHTML = iolist_to_binary(translate(Cite, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>The Book</em>\n"
		"</div>\n"
		"</div>\n">> = CiteHTML,
	Encoded = asciideck:parse(
		"[quote, A & B, <Book>]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	EncodedHTML = iolist_to_binary(translate(Encoded, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>&lt;Book&gt;</em><br />\n"
		"&#8212; A &amp; B\n"
		"</div>\n"
		"</div>\n">> = EncodedHTML,
	List = asciideck:parse(
		"____\n"
		"* One\n"
		"____\n"),
	ListHTML = iolist_to_binary(translate(List, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<ul><li>One\n</li>\n</ul>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = ListHTML,
	%% A positional value is written over the same named attribute.
	Pos = asciideck:parse(
		"[quote, Ann, Book, attribution=\"Other\", citetitle=\"Other Book\"]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	PosHTML = iolist_to_binary(translate(Pos, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = PosHTML,
	Named = asciideck:parse(
		"[attribution=\"Someone\", citetitle=\"Work\"]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	NamedHTML = iolist_to_binary(translate(Named, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Work</em><br />\n"
		"&#8212; Someone\n"
		"</div>\n"
		"</div>\n">> = NamedHTML,
	%% An empty positional value is absent, even when the name is set.
	EmptyPos = asciideck:parse(
		"[quote, , , attribution=\"Ann\"]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	EmptyHTML = iolist_to_binary(translate(EmptyPos, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = EmptyHTML,
	%% A comment does not take the attribution.
	Commented = asciideck:parse(
		"[quote, Ann, Book]\n"
		"// c\n"
		"____\n"
		"Words.\n"
		"____\n"),
	CommentedHTML = iolist_to_binary(translate(Commented, #{no_header_footer => true})),
	<<"<!-- c-->\n"
		"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = CommentedHTML,
	%% A positional style wins over a later named style.
	PosStyle = asciideck:parse(
		"[quote, Ann, Book]\n"
		"[style=\"verse\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	PosStyleHTML = iolist_to_binary(translate(PosStyle, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>one two</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = PosStyleHTML,
	%% An empty positional author does not clear a later named one.
	LaterAnn = asciideck:parse(
		"[quote, , Book]\n"
		"[attribution=\"Ann\"]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	LaterAnnHTML = iolist_to_binary(translate(LaterAnn, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = LaterAnnHTML,
	EarlierAnn = asciideck:parse(
		"[attribution=\"Ann\", citetitle=\"Work\"]\n"
		"[quote, , Book]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	EarlierAnnHTML = iolist_to_binary(translate(EarlierAnn, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = EarlierAnnHTML,
	LaterCite = asciideck:parse(
		"[quote, Ann,]\n"
		"[citetitle=\"Book\"]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	LaterCiteHTML = iolist_to_binary(translate(LaterCite, #{no_header_footer => true})),
	<<"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = LaterCiteHTML,
	%% A literal delimiter does not let a verse style swallow the quote.
	Lit = asciideck:parse(
		"[verse]\n"
		"....\n"
		"....\n"
		"____\n"
		"line\n"
		"____\n"),
	LitHTML = iolist_to_binary(translate(Lit, #{no_header_footer => true})),
	<<"<div class=\"literalblock\">"
		"<div class=\"content\"><pre></pre></div></div>\n"
		"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>line</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = LitHTML,
	InnerVerse = asciideck:parse(
		"....\n"
		"[verse]\n"
		"....\n"
		"____\n"
		"line\n"
		"____\n"),
	InnerVerseHTML = iolist_to_binary(translate(InnerVerse, #{no_header_footer => true})),
	<<"<div class=\"literalblock\">"
		"<div class=\"content\"><pre>[verse]</pre></div></div>\n"
		"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>line</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = InnerVerseHTML,
	%% A rejected attribute list is a paragraph. The delimiter still opens.
	Bad = asciideck:parse(
		"[quote, \"Ann]\n"
		"____\n"
		"hello malformed\n"
		"____\n"),
	BadHTML = iolist_to_binary(translate(Bad, #{no_header_footer => true})),
	<<"<p>[quote, &quot;Ann]</p>\n"
		"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello malformed</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = BadHTML,
	ok.

verse_block_test() ->
	AST = asciideck:parse(
		".Verse Title\n"
		"[verse, Ann, Book]\n"
		"____\n"
		"To *see* a world\n"
		"And a heaven\n"
		"\n"
		"Hold infinity.\n"
		"____\n"),
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<div class=\"title\">Verse Title</div>\n"
		"<pre class=\"content\">To <strong>see</strong> a world\n"
		"And a heaven\n"
		"\n"
		"Hold infinity.</pre>\n"
		"<div class=\"attribution\">\n"
		"<em>Book</em><br />\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = HTML,
	Cross = asciideck:parse(
		"[verse]\n"
		"____\n"
		"*bold\n"
		"still*\n"
		"____\n"),
	CrossHTML = iolist_to_binary(translate(Cross, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\"><strong>bold\n"
		"still</strong></pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = CrossHTML,
	Meta = asciideck:parse(
		"[verse]\n"
		"____\n"
		"line\n"
		"\n"
		".Title\n"
		"____\n"
		"after\n"),
	MetaHTML = iolist_to_binary(translate(Meta, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">line\n"
		"\n"
		".Title</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n"
		"<p>after</p>\n">> = MetaHTML,
	Break = asciideck:parse(
		"[verse]\n"
		"____\n"
		"one +\n"
		"two\n"
		"____\n"),
	BreakHTML = iolist_to_binary(translate(Break, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">one<br/>\n"
		"two</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = BreakHTML,
	%% The first underscore line ends the verse. Later lines are not verse text.
	Inner = asciideck:parse(
		"[verse]\n"
		"____\n"
		"intro\n"
		"\n"
		"----\n"
		"____\n"
		"code\n"
		"----\n"
		"____\n"
		"after\n"),
	InnerHTML = iolist_to_binary(translate(Inner, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">intro\n"
		"\n"
		"----</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n",
		_/bits>> = InnerHTML,
	%% A comment between the style and the delimiter stays a verse.
	Comment = asciideck:parse(
		"[verse]\n"
		"// comment\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	CommentHTML = iolist_to_binary(translate(Comment, #{no_header_footer => true})),
	<<"<!-- comment-->\n"
		"<div class=\"verseblock\">\n"
		"<pre class=\"content\">one\n"
		"two</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = CommentHTML,
	EmptyPos = asciideck:parse(
		"[verse]\n"
		"[, Ann]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	EmptyPosHTML = iolist_to_binary(translate(EmptyPos, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">one\n"
		"two</pre>\n"
		"<div class=\"attribution\">\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = EmptyPosHTML,
	Named = asciideck:parse(
		"[style=\"verse\", attribution=\"Ann\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	NamedHTML = iolist_to_binary(translate(Named, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">one\n"
		"two</pre>\n"
		"<div class=\"attribution\">\n"
		"&#8212; Ann\n"
		"</div>\n"
		"</div>\n">> = NamedHTML,
	Closest = asciideck:parse(
		"[style=\"quote\"]\n"
		"[style=\"verse\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	ClosestHTML = iolist_to_binary(translate(Closest, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">one\n"
		"two</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n">> = ClosestHTML,
	%% A continued attribute entry does not keep the verse open.
	Cont = asciideck:parse(
		"[verse]\n"
		":description: hello +\n"
		"continued\n"
		"____\n"
		"intro\n"
		"\n"
		"----\n"
		"____\n"
		"code\n"
		"----\n"
		"____\n"
		"after\n"),
	ContHTML = iolist_to_binary(translate(Cont, #{no_header_footer => true})),
	<<"<div class=\"verseblock\">\n"
		"<pre class=\"content\">intro\n"
		"\n"
		"----</pre>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n",
		_/bits>> = ContHTML,
	ok.

example_block_test() ->
	Plain = asciideck:parse(
		"====\n"
		"Hello *world*.\n"
		"\n"
		"Second.\n"
		"====\n"),
	PlainHTML = iolist_to_binary(translate(Plain, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>Hello <strong>world</strong>.</p>\n"
		"<p>Second.</p>\n"
		"</div></div>\n">> = PlainHTML,
	Titled = asciideck:parse(
		".An example\n"
		"====\n"
		"Qui in magna commodo, est labitur dolorum an. Est ne magna primis\n"
		"adolescens.\n"
		"====\n"),
	TitledHTML = iolist_to_binary(translate(Titled, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"title\">An example</div>\n"
		"<div class=\"content\">\n"
		"<p>Qui in magna commodo, est labitur dolorum an. "
			"Est ne magna primis adolescens.</p>\n"
		"</div></div>\n">> = TitledHTML,
	Caption = asciideck:parse(
		"[caption=\"Example 1: \"]\n"
		".An example with a custom caption\n"
		"====\n"
		"Words.\n"
		"====\n"),
	CaptionHTML = iolist_to_binary(translate(Caption, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"title\">Example 1: An example with a custom caption</div>\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div></div>\n">> = CaptionHTML,
	CaptionOnly = asciideck:parse(
		"[caption=\"Example 1: \"]\n"
		"====\n"
		"Words.\n"
		"====\n"),
	CaptionOnlyHTML = iolist_to_binary(translate(CaptionOnly,
		#{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div></div>\n">> = CaptionOnlyHTML,
	Encoded = translate([{example_block, #{
		<<"caption">> => <<"A & B: ">>,
		<<"title">> => <<"<x>">>
	}, [
		{paragraph, #{}, <<"Words.">>, #{line => 1}}
	], #{line => 1}}], #{no_header_footer => true}),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"title\">A &amp; B: &lt;x&gt;</div>\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div></div>\n">> = iolist_to_binary(Encoded),
	EmptyCap = translate([{example_block, #{
		<<"caption">> => <<>>,
		<<"title">> => <<"An example">>
	}, [
		{paragraph, #{}, <<"Words.">>, #{line => 1}}
	], #{line => 1}}], #{no_header_footer => true}),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"title\">An example</div>\n"
		"<div class=\"content\">\n"
		"<p>Words.</p>\n"
		"</div></div>\n">> = iolist_to_binary(EmptyCap),
	Role = asciideck:parse(
		"[role=\"x\"]\n"
		"====\n"
		"hello\n"
		"====\n"),
	RoleHTML = iolist_to_binary(translate(Role, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello</p>\n"
		"</div></div>\n">> = RoleHTML,
	Listed = asciideck:parse(
		"====\n"
		"* One.\n"
		"====\n"),
	ListedHTML = iolist_to_binary(translate(Listed, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<ul><li>One.\n</li>\n</ul>\n"
		"</div></div>\n">> = ListedHTML,
	Listing = asciideck:parse(
		"====\n"
		"Intro paragraph is long enough.\n"
		"\n"
		"----\n"
		"====\n"
		"code\n"
		"----\n"
		"====\n"),
	ListingHTML = iolist_to_binary(translate(Listing, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>Intro paragraph is long enough.</p>\n"
		"<div class=\"listingblock\">"
		"<div class=\"content\">"
		"<pre>====\ncode</pre>"
		"</div></div>\n"
		"</div></div>\n">> = ListingHTML,
	Dropped = asciideck:parse(
		"====\n"
		"before\n"
		"\n"
		"== Not a title\n"
		"\n"
		"after\n"
		"====\n"),
	DroppedHTML = iolist_to_binary(translate(Dropped, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>before</p>\n"
		"<p>after</p>\n"
		"</div></div>\n">> = DroppedHTML,
	Side = asciideck:parse(
		"====\n"
		"before\n"
		"\n"
		"****\n"
		"side\n"
		"****\n"
		"\n"
		"after\n"
		"====\n"),
	SideHTML = iolist_to_binary(translate(Side, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>before</p>\n"
		"<div class=\"sidebarblock\"><div class=\"content\">"
		"<p>side</p>\n"
		"</div></div>\n"
		"<p>after</p>\n"
		"</div></div>\n">> = SideHTML,
	Note = asciideck:parse(
		"[NOTE]\n"
		".A NOTE admonition block\n"
		"====\n"
		"Hello *bold*.\n"
		"\n"
		"* One.\n"
		"====\n"),
	NoteHTML = iolist_to_binary(translate(Note, #{no_header_footer => true})),
	<<"<div class=\"admonitionblock note\">"
		"<div class=\"title\">Note</div>\n"
		"<div class=\"content\">"
		"<div class=\"title\">A NOTE admonition block</div>\n"
		"<p>Hello <strong>bold</strong>.</p>\n"
		"<ul><li>One.\n</li>\n</ul>\n"
		"</div></div>\n">> = NoteHTML,
	Nested = asciideck:parse(
		"====\n"
		"outer\n"
		"\n"
		"====\n"
		"inner\n"
		"====\n"
		"====\n"),
	NestedHTML = iolist_to_binary(translate(Nested, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>outer</p>\n"
		"</div></div>\n"
		"<p>inner</p>\n"
		"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"</div></div>\n">> = NestedHTML,
	Empty = asciideck:parse(
		"====\n"
		"====\n"
		"after\n"),
	EmptyHTML = iolist_to_binary(translate(Empty, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"</div></div>\n"
		"<p>after</p>\n">> = EmptyHTML,
	NestedTitle = asciideck:parse(
		"====\n"
		"____\n"
		"hello\n"
		"\n"
		".Title\n"
		"____\n"
		"====\n"
		"====\n"
		"body\n"
		"====\n"),
	NestedTitleHTML = iolist_to_binary(translate(NestedTitle,
		#{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<div class=\"quoteblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello</p>\n"
		"</div>\n"
		"<div class=\"attribution\">\n"
		"</div>\n"
		"</div>\n"
		"</div></div>\n"
		"<div class=\"exampleblock\">\n"
		"<div class=\"title\">Title</div>\n"
		"<div class=\"content\">\n"
		"<p>body</p>\n"
		"</div></div>\n">> = NestedTitleHTML,
	Spaced = asciideck:parse(
		"==== \n"
		"\n"
		"hello there friend\n"
		"\n"
		"====\n"),
	SpacedHTML = iolist_to_binary(translate(Spaced, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello there friend</p>\n"
		"</div></div>\n">> = SpacedHTML,
	TrailComment = asciideck:parse(
		"====\n"
		"hello there friend\n"
		"\n"
		".Title\n"
		"// c\n"
		"\n"
		"====\n"
		"after the example here\n"),
	TrailCommentHTML = iolist_to_binary(translate(TrailComment,
		#{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello there friend</p>\n"
		"<!-- c-->\n"
		"</div></div>\n"
		"<p>after the example here</p>\n">> = TrailCommentHTML,
	Trail = asciideck:parse(
		"====\n"
		"hello\n"
		"\n"
		".Title\n"
		":foo: bar\n"
		"[role=\"x\"]\n"
		"====\n"
		"after\n"),
	TrailHTML = iolist_to_binary(translate(Trail, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>hello</p>\n"
		"</div></div>\n"
		"<p>after</p>\n">> = TrailHTML,
	ok.

%% User Guide 16.5. The block stays in the AST and is not written.
comment_block_test() ->
	AST = asciideck:parse(
		"Before.\n"
		"\n"
		"////\n"
		"hidden *stars*\n"
		"////\n"
		"\n"
		"After.\n"),
	[{paragraph, _, <<"Before.">>, _},
		{comment_block, _, <<"hidden *stars*">>, _},
		{paragraph, _, <<"After.">>, _}] = AST,
	HTML = iolist_to_binary(translate(AST, #{no_header_footer => true})),
	<<"<p>Before.</p>\n"
		"<p>After.</p>\n">> = HTML,
	Nested = asciideck:parse(
		"====\n"
		"visible\n"
		"\n"
		"////\n"
		"hidden\n"
		"////\n"
		"\n"
		"after\n"
		"====\n"),
	[{example_block, _, [
		{paragraph, _, <<"visible">>, _},
		{comment_block, _, <<"hidden">>, _},
		{paragraph, _, <<"after">>, _}
	], _}] = Nested,
	NestedHTML = iolist_to_binary(translate(Nested, #{no_header_footer => true})),
	<<"<div class=\"exampleblock\">\n"
		"<div class=\"content\">\n"
		"<p>visible</p>\n"
		"<p>after</p>\n"
		"</div></div>\n">> = NestedHTML,
	%% A comment in a cell is not written, and the cell stays one paragraph.
	Table = asciideck:parse(
		"|===\n"
		"|A |B\n"
		"|C\n"
		"////\n"
		"hidden\n"
		"////\n"
		"|D\n"
		"|===\n"),
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"A">>, _}], _},
			{cell, _, [{paragraph, _, <<"B">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [
				{paragraph, _, <<"C">>, _},
				{comment_block, _, <<"hidden">>, _}
			], _},
			{cell, _, [{paragraph, _, <<"D">>, _}], _}
		], _}
	], _}] = Table,
	TableHTML = iolist_to_binary(translate(Table, #{no_header_footer => true})),
	<<"<table rules=\"all\" width=\"100%\" frame=\"border\"\n"
		"			cellspacing=\"0\" cellpadding=\"4\">\n"
		"<thead><tr><th>A</th>\n"
		"<th>B</th>\n"
		"</tr></thead><tbody><tr><td>C</td>\n"
		"<td>D</td>\n"
		"</tr>\n"
		"</tbody></table>\n">> = TableHTML,
	ok.
-endif.
