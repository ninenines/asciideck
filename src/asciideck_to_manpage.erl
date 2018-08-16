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

-ifdef(OTP_RELEASE).
-compile({nowarn_deprecated_function, [{erlang, get_stacktrace, 0}]}).
-endif.

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
			listing_block -> listing_block(Node);
			list -> list(Node);
			table -> table(Node);
			comment_line -> comment_line(Node);
			_ ->
				io:format("Ignored AST node ~p~n", [Node]),
				[]
		end
	catch C:E ->
		io:format("Ignored AST node ~p~nReason: ~p:~p~nStacktrace: ~p~n",
			[Node, C, E, erlang:get_stacktrace()]),
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

%% Listing blocks.

listing_block({listing_block, Attrs, Listing, _}) ->
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
	fold(Items, fun labeled_list_item/1).

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

table_contents_cell({cell, _, [{paragraph, _, Text, _}], _}) ->
	["T{\n", inline(Text), "\nT}"].

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
%% Xref links appear as plain text in manuals.
inline({xref, _, Text, _}) ->
	inline(Text);
inline({line_break, _, _, _}) ->
	"\n.br\n";
inline(Text) when is_list(Text) ->
	[inline(T) || T <- Text].

escape(Text) ->
	binary:replace(iolist_to_binary(Text), <<$\\>>, <<$\\, $\\>>, [global]).
