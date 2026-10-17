%% Copyright (c) 2016-2018, Loïc Hoguin <essen@ninenines.eu>
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

%% The Groff documentation section 4.1 has a pretty good
%% description of the format expected for man pages.
-module(asciideck_to_manpage).

-export([translate/2]).

translate(AST, Opts) ->
	{Man, Section, Output0} = man(AST, Opts),
	{CompressExt, Output} = case Opts of
		#{compress := gzip} -> {".gz", zlib:gzip(Output0)};
		_ -> {"", Output0}
	end,
	case Opts of
		#{outdir := Path} ->
			file:write_file(binary_to_list(iolist_to_binary(
				[Path, "/", Man, ".", Section, CompressExt])), Output);
		_ ->
			Output
	end.

%% Header of the man page file.
%%
%% Comment blocks are not written. One before the title, or between
%% the title and the Name section, must not hide either.

man([{comment_block, _, _, _}|AST], Opts) ->
	man(AST, Opts);
man([{section_title, #{level := 0}, Title0, _Ann}|AST], Opts) ->
	ensure_name_section(AST),
	[Title, << Section:1/binary, _/bits >>] = binary:split(Title0, <<"(">>),
	Extra1 = maps:get(extra1, Opts, today()),
	Extra2 = maps:get(extra2, Opts, ""),
	Extra3 = maps:get(extra3, Opts, ""),
	{Title, Section, [
		".TH \"", Title, "\" \"", Section, "\" \"",
			Extra1, "\" \"", Extra2, "\" \"", Extra3, "\"\n"
		".ta T 4n\n\\&\n",
		ast(AST)
	]}.

ensure_name_section([{comment_block, _, _, _}|AST]) ->
	ensure_name_section(AST);
ensure_name_section([{section_title, #{level := 1}, Title, _}|_]) ->
	case string:to_lower(string:strip(binary_to_list(Title))) of
		"name" -> ok;
		_ -> error(badarg)
	end;
ensure_name_section(_) ->
	error(badarg).

today() ->
	{{Y, M, D}, _} = calendar:universal_time(),
	io_lib:format("~b-~2.10.0b-~2.10.0b", [Y, M, D]).

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
			literal_paragraph -> listing_block(Node);
			literal_block -> listing_block(Node);
			listing_block -> listing_block(Node);
			open_block -> open_block(Node);
			sidebar_block -> sidebar_block(Node);
			quote_block -> quote_block(Node);
			example_block -> example_block(Node);
			list -> list(Node);
			table -> table(Node);
			comment_line -> comment_line(Node);
			comment_block -> [];
			attribute_entry -> [];
			_ ->
				io:format("Ignored AST node ~p~n", [Node]),
				[]
		end
	catch C:E:S ->
		io:format("Ignored AST node ~p~nReason: ~p:~p~nStacktrace: ~p~n",
			[Node, C, E, S]),
		[]
	end.

%% Section titles.

section_title({section_title, #{level := 1}, Title, _}) ->
	[".SH ", string:to_upper(binary_to_list(Title)), "\n"];
section_title({section_title, #{level := 2}, Title, _}) ->
	[".SS ", Title, "\n"].

%% Paragraphs.

paragraph({paragraph, _, Text, _}) ->
	[".LP\n", inline(Text), "\n.sp\n"].

%% Admonition paragraphs.

admonition({admonition, Attrs=#{style := Style}, Text, _}) ->
	Label = admonition_label(Style),
	Title = case Attrs of
		#{<<"title">> := T} -> [": ", inline(T)];
		_ -> []
	end,
	[
		".RS 4\n"
		"\\fB", Label, Title, "\\fR\n"
		".br\n",
		admonition_body(Text), "\n"
		".RE\n"
		".sp\n"
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

%% Open blocks.

open_block({open_block, _, Children, _}) ->
	ast(Children).

%% Sidebar blocks.

sidebar_block({sidebar_block, Attrs, Children, _}) ->
	[
		case Attrs of
			#{<<"title">> := Title} ->
				[".PP\n\\fB", inline(Title), "\\fR\n"];
			_ ->
				[]
		end,
		ast(Children)
	].

%% Quote and verse blocks.

quote_block({quote_block, Attrs, Body, _}) ->
	case quote_style(Attrs) of
		<<"verse">> ->
			[
				man_block_title(Attrs),
				".nf\n", inline(Body), "\n.fi\n",
				man_attribution(Attrs)
			];
		_ ->
			[
				man_block_title(Attrs),
				ast(Body),
				man_attribution(Attrs)
			]
	end.

%% Positional attribute 1 is the style when that key was written.
%% Otherwise the named style attribute is. The default is quote.
quote_style(Attrs) ->
	case maps:is_key(1, Attrs) of
		true ->
			maps:get(1, Attrs);
		false ->
			maps:get(<<"style">>, Attrs, <<"quote">>)
	end.

man_block_title(#{<<"title">> := Title}) ->
	[".PP\n\\fB", inline(Title), "\\fR\n"];
man_block_title(_) ->
	[].

man_attribution(Attrs) ->
	Cite = man_quote_value(Attrs, 3, <<"citetitle">>),
	Author = man_quote_value(Attrs, 2, <<"attribution">>),
	case {Cite, Author} of
		{none, none} ->
			[];
		{Cite, none} ->
			[".PP\n\\fI", inline(Cite), "\\fR\n"];
		{none, Author} ->
			[".PP\n\\(em ", inline(Author), "\n"];
		{Cite, Author} ->
			[".PP\n\\fI", inline(Cite), "\\fR\n.br\n\\(em ", inline(Author), "\n"]
	end.

%% A positional attribute is written over a named one.
man_quote_value(Attrs, Pos, Name) ->
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

%% Example blocks. A caption prefixes the title.
%% A caption without a title is not a title line.
%% Examples are not numbered.
example_block({example_block, Attrs, Children, _}) ->
	[example_title(Attrs), ast(Children)].

example_title(#{<<"title">> := Title}=Attrs) ->
	[".PP\n\\fB", inline(example_caption(Attrs)), inline(Title), "\\fR\n"];
example_title(_) ->
	[].

%% The caption attribute includes any separator it wants.
example_caption(#{<<"caption">> := Caption}) when Caption =/= <<>> ->
	Caption;
example_caption(_) ->
	<<>>.

%% Listing blocks.

listing_block({_, Attrs, Listing, _}) ->
	[
		case Attrs of
			#{<<"title">> := Title} ->
				[".PP\n\\fB", Title, "\\fR\n"];
			_ ->
				[]
		end,
		".if n \\{\\\n"
		".RS 4\n"
		".\\}\n"
		".nf\n",
		escape(Listing),
		"\n"
		".fi\n"
		".if n \\{\\\n"
		".RE\n"
		".\\}\n"
	].

%% Lists.

list({list, #{type := bulleted}, Items, _}) ->
	fold(Items, fun bulleted_list_item/1);
list({list, #{type := labeled}, Items, _}) ->
	fold(Items, fun labeled_list_item/1);
list({list, #{type := callout}, Items, _}) ->
	callout_items(Items, 1).

bulleted_list_item({list_item, _, [{paragraph, _, Text, _}|AST], _}) ->
	[
		".ie n \\{\\\n"
		".RS 2\n"
		"\\h'-02'\\(bu\\h'+01'\\c\n"
		".\\}\n"
		".el \\{\\\n"
		".RS 4\n"
		".sp -1\n"
		".IP \\(bu 2.3\n"
		".\\}\n",
		inline(Text), "\n",
		ast(AST),
		".RE\n"
	].

%% User Guide 20. The rendered number is the item's position.
%% The source marker is not used. AsciiDoc warns when an
%% explicit marker does not match that position.
callout_items([], _) ->
	[];
callout_items([Item|Items], N) ->
	[callout_list_item(Item, N)|callout_items(Items, N + 1)].

callout_list_item({list_item, _, [{paragraph, _, Text, _}|AST], _}, N) ->
	[
		".IP ", integer_to_list(N), "\n",
		inline(Text), "\n",
		ast(AST)
	].

labeled_list_item({list_item, #{label := Label}, [{paragraph, _, Text, _}|AST], _}) ->
	[
		".PP\n"
		"\\fB", inline(Label), "\\fR\n",
		".RS 4\n",
		inline(Text), "\n",
		ast(AST),
		".RE\n"
	].

%% Tables.

table({table, _, Rows0, _}) ->
	Rows = table_apply_options(Rows0),
	[
		".TS\n"
		"allbox tab(:);\n",
		table_style(Rows), ".\n",
		table_contents(Rows),
		".TE\n"
		".sp 1\n"
	].

%% @todo Currently acts as if options="headers" was always set.
table_apply_options([{row, RAttrs, Headers0, RAnn}|Tail]) ->
	Headers = [{cell, CAttrs#{style => <<"strong">>}, CText, CAnn}
		|| {cell, CAttrs, CText, CAnn} <- Headers0],
	[{row, RAttrs, Headers, RAnn}|Tail].

table_style(Rows) ->
	[[table_style_cells(Cells), "\n"]
		|| {row, _, Cells, _} <- Rows].

table_style_cells(Cells) ->
	[case CAttrs of
		#{style := <<"strong">>} -> "ltb ";
		_ -> "lt "
	end || {cell, CAttrs, _, _} <- Cells].

table_contents(Rows) ->
	[[table_contents_cells(Cells), "\n"]
		|| {row, _, Cells, _} <- Rows].

table_contents_cells([FirstCell|Cells]) ->
	[table_contents_cell(FirstCell),
		[[":", table_contents_cell(Cell)] || Cell <- Cells]].

table_contents_cell({cell, _, Blocks, _}) ->
	case [Node || Node={Type, _, _, _} <- Blocks, Type =/= comment_block] of
		[{paragraph, _, Text, _}] ->
			["T{\n", inline(Text), "\nT}"];
		[] ->
			["T{\n\nT}"]
	end.

%% Comment lines are printed in the generated file
%% but are not visible in viewers.

comment_line({comment_line, _, Text, _}) ->
	["\\# ", Text, "\n"].

%% Inline formatting.

inline(Text) when is_binary(Text) ->
	escape(Text);
%% When the link is the text we only print it once.
inline({link, #{target := Link}, Link, _}) ->
	Link;
inline({link, #{target := Link}, Text, _}) ->
	case re:run(Text, "^([-_:.a-zA-Z0-9]*)(\\([0-9]\\))$", [{capture, all, binary}]) of
		nomatch -> [Text, " (", Link, ")"];
		{match, [_, ManPage, ManSection]} -> ["\\fB", ManPage, "\\fR", ManSection]
	end;
inline({emphasized, _, Text, _}) ->
	["\\fI", inline(Text), "\\fR"];
inline({strong, _, Text, _}) ->
	["\\fB", inline(Text), "\\fR"];
%% We are already using a monospace font.
inline({inline_literal_passthrough, _, Text, _}) ->
	inline(Text);
inline({monospaced, _, Text, _}) ->
	inline(Text);
inline({passthrough, _, Text, _}) ->
	inline(Text);
inline({superscript, _, Text, _}) ->
	["\\u", inline(Text), "\\d"];
inline({subscript, _, Text, _}) ->
	["\\d", inline(Text), "\\u"];
%% Xref links appear as plain text in manuals.
inline({xref, _, Text, _}) ->
	inline(Text);
inline({line_break, _, _, _}) ->
	"\n.br\n";
inline(Text) when is_list(Text) ->
	[inline(T) || T <- Text].

escape(Text) ->
	binary:replace(iolist_to_binary(Text), <<$\\>>, <<$\\, $\\>>, [global]).
