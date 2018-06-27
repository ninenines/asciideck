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
			listing_block -> listing_block(Node);
			passthrough_block -> passthrough_block(Node);
			list -> list(Node);
			table -> table(Node);
			block_macro -> block_macro(Node);
			comment_line -> comment_line(Node);
			_ -> ast_error({unknown_type, Node})
		end
	catch C:E ->
		ast_error({crash, C, E, erlang:get_stacktrace(), Node})
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

%% Listing blocks.

listing_block({listing_block, Attrs, Listing0, _}) ->
	Listing = case Attrs of
		#{1 := <<"source">>, 2 := _} ->
			try asciideck_source_highlight:filter(Listing0, Attrs) catch C:E -> io:format("~p ~p ~p~n", [C, E, erlang:get_stacktrace()]), exit(bad) end;
		_ ->
			["<pre>", html_encode(Listing0), "</pre>"]
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

%% Passthrough blocks.

passthrough_block({passthrough_block, _, HTML, _}) ->
	HTML.

%% Lists.

list({list, #{type := bulleted}, Items, _}) ->
	["<ul>", fold(Items, fun list_item/1), "</ul>\n"];
list({list, #{type := numbered}, Items, _}) ->
	["<ol>", fold(Items, fun list_item/1), "</ol>\n"];
list({list, #{type := labeled}, Items, _}) ->
	["<dl>", fold(Items, fun labeled_list_item/1), "</dl>\n"].

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
