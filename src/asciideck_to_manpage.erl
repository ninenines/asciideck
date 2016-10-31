%% Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
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
	{Man, Section, Output0} = man(AST),
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

man([{title, #{level := 0}, Title0, _Ann}|AST]) ->
	[Title, << Section:1/binary, _/bits >>] = binary:split(Title0, <<"(">>),
	Extra1 = "2016-10-17", %% @todo
	Extra2 = "Project 1.0", %% @todo
	Extra3 = "Project Function Reference", %% @todo
	{Title, Section, [
		".TH \"", Title, "\" \"", Section, "\" \"",
			Extra1, "\" \"", Extra2, "\" \"", Extra3, "\"\n"
		".ta T 4n\n\\&\n",
		man(AST, [])
	]}.

man([], Acc) ->
	lists:reverse(Acc);
man([{title, #{level := 1}, Title, _Ann}|Tail], Acc) ->
	man(Tail, [[".SH ", string:to_upper(binary_to_list(Title)), "\n"]|Acc]);
man([{title, #{level := 2}, Title, _Ann}|Tail], Acc) ->
	man(Tail, [[".SS ", Title, "\n"]|Acc]);
man([{p, _Attrs, Text, _Ann}|Tail], Acc) ->
	man(Tail, [[".LP\n", man_format(Text), "\n.sp\n"]|Acc]);
man([{listing, Attrs, Listing, _Ann}|Tail], Acc0) ->
	Acc1 = case Attrs of
		#{title := Title} ->
			[[".PP\n\\fB", Title, "\\fR\n"]|Acc0];
		_ ->
			Acc0
	end,
	Acc = [[
		".if n \\{\\\n"
		".RS 4\n"
		".\\}\n"
		".nf\n",
		Listing,
		"\n"
		".fi\n"
		".if n \\{\\\n"
		".RE\n"
		".\\}\n"]|Acc1],
	man(Tail, Acc);
man([{ul, _Attrs, Items, _Ann}|Tail], Acc0) ->
	Acc = man_ul(Items, Acc0),
	man(Tail, Acc);
man([{ll, _Attrs, Items, _Ann}|Tail], Acc0) ->
	Acc = man_ll(Items, Acc0),
	man(Tail, Acc);
%% @todo Attributes.
%% Currently acts as if options="headers" was always set.
man([{table, _TAttrs, [{row, RowAttrs, Headers0, RowAnn}|Rows0], _TAnn}|Tail], Acc0) ->
	Headers = [{cell, CAttrs, [{p, Attrs, [{strong, #{}, P, CAnn}], Ann}], CAnn}
		|| {cell, CAttrs, [{p, Attrs, P, Ann}], CAnn} <- Headers0],
	Rows = [{row, RowAttrs, Headers, RowAnn}|Rows0],
	Acc = [[
		".TS\n"
		"allbox tab(:);\n",
		man_table_style(Rows, []),
		man_table_contents(Rows),
		".TE\n"
		".sp 1\n"]|Acc0],
	man(Tail, Acc);
%% Skip everything we don't understand.
man([_Ignore|Tail], Acc) ->
	io:format("Ignore ~p~n", [_Ignore]), %% @todo lol io:format
	man(Tail, Acc).

man_ll([], Acc) ->
	Acc;
man_ll([{li, #{label := Label}, [{p, _PAttrs, Text, _PAnn}], _LiAnn}|Tail], Acc0) ->
	Acc = [[
		".PP\n"
		"\\fB", Label, "\\fR\n",
		".RS 4\n",
		man_format(Text), "\n"
		".RE\n"]|Acc0],
	man_ll(Tail, Acc).

man_ul([], Acc) ->
	Acc;
man_ul([{li, _LiAttrs, [{p, _PAttrs, Text, _PAnn}], _LiAnn}|Tail], Acc0) ->
	Acc = [[
		".ie n \\{\\\n"
		".RS 2\n"
		"\\h'-02'\\(bu\\h'+01'\\c\n"
		".\\}\n"
		".el \\{\\\n"
		".RS 4\n"
		".sp -1\n"
		".IP \\(bu 2.3\n"
		".\\}\n",
		man_format(Text), "\n"
		".RE\n"]|Acc0],
	man_ul(Tail, Acc).

man_table_style([], [_|Acc]) ->
	lists:reverse([".\n"|Acc]);
man_table_style([{row, _, Cols, _}|Tail], Acc) ->
	man_table_style(Tail, [$\n, man_table_style_cols(Cols, [])|Acc]).

man_table_style_cols([], [_|Acc]) ->
	lists:reverse(Acc);
man_table_style_cols([{cell, _, _, _}|Tail], Acc) ->
	man_table_style_cols(Tail, [$\s, "lt"|Acc]).

man_table_contents(Rows) ->
	[man_table_contents_cols(Cols, []) || {row, _, Cols, _} <- Rows].

man_table_contents_cols([], [_|Acc]) ->
	lists:reverse(["\n"|Acc]);
man_table_contents_cols([{cell, _CAttrs, [{p, _PAttrs, Text, _PAnn}], _CAnn}|Tail], Acc) ->
	man_table_contents_cols(Tail, [$:, "\nT}", man_format(Text), "T{\n"|Acc]).

man_format(Text) when is_binary(Text) ->
	Text;
man_format({rel_link, #{target := Link}, Text, _}) ->
	case re:run(Text, "^([-_:.a-zA-Z]*)(\\([0-9]\\))$", [{capture, all, binary}]) of
		nomatch -> [Text, " (", Link, ")"];
		{match, [_, ManPage, ManSection]} -> ["\\fB", ManPage, "\\fR", ManSection]
	end;
man_format({strong, _, Text, _}) ->
	["\\fB", man_format(Text), "\\fR"];
%% We are already using a monospace font.
%% @todo Maybe there's a readable formatting we could use to differentiate from normal text?
man_format({mono, _, Text, _}) ->
	man_format(Text);
man_format(Text) when is_list(Text) ->
	[man_format(T) || T <- Text].
