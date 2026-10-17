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
			list -> list(Node);
			table -> table(Node);
			block_macro -> block_macro(Node);
			comment_line -> comment_line(Node);
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
	AST = [Node || Node={Type, _, _, _} <- AST0, Type =/= comment_line],
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
-endif.
