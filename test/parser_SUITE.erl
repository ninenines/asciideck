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

-module(parser_SUITE).
-compile(export_all).

-import(asciideck, [parse/1]).
-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

%% @todo Test formatting too!

%% Empty lines.

empty_line(_) ->
	doc("Empty lines are not part of the AST."),
	[] = parse(""),
	[] = parse("\n"),
	[] = parse("\n\n\n\n\n"),
	ok.

empty_line_spaces(_) ->
	doc("Lines containing only whitespace are empty."),
	[] = parse(" "),
	[] = parse("  \n"),
	[] = parse(" \n  \n   \n    \n     \n"),
	ok.

%% Title.

%% @todo Long titles. (11.1)
%% @todo Floating titles. (11.3)

title_short(_) ->
	doc("The trailing title delimiter is optional. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!"),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!"),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!"),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!"),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world!"),
	ok.

title_short_no_spaces(_) ->
	doc("One or more spaces must fall between the title and the delimiter. (11.2)"),
	[{p, _, <<"=Hello world!">>, _}] = parse("=Hello world!"),
	[{p, _, <<"==Hello world!">>, _}] = parse("==Hello world!"),
	[{p, _, <<"===Hello world!">>, _}] = parse("===Hello world!"),
	[{p, _, <<"====Hello world!">>, _}] = parse("====Hello world!"),
	[{p, _, <<"=====Hello world!">>, _}] = parse("=====Hello world!"),
	ok.

title_short_trim_spaces_before(_) ->
	doc("Spaces between the title and delimiter must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!"),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!"),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!"),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!"),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world!"),
	ok.

title_short_trim_spaces_after(_) ->
	doc("Spaces after the title must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!     "),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!    "),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!   "),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!  "),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! "),
	ok.

title_short_trim_spaces_before_after(_) ->
	doc("Spaces before and after the title must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!     "),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!    "),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!   "),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!  "),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world! "),
	ok.

title_short_trailer(_) ->
	doc("The trailing title delimiter is optional. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world! ="),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world! =="),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world! ==="),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world! ===="),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ====="),
	ok.

title_short_trailer_no_spaces(_) ->
	doc("One or more spaces must fall between the title and the trailer. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!=">>, _}] = parse("= Hello world!="),
	[{title, #{level := 1}, <<"Hello world!==">>, _}] = parse("== Hello world!=="),
	[{title, #{level := 2}, <<"Hello world!===">>, _}] = parse("=== Hello world!==="),
	[{title, #{level := 3}, <<"Hello world!====">>, _}] = parse("==== Hello world!===="),
	[{title, #{level := 4}, <<"Hello world!=====">>, _}] = parse("===== Hello world!====="),
	ok.

title_short_trim_spaces_before_trailer(_) ->
	doc("Spaces between the title and trailer must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!          ="),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!        =="),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!      ==="),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!    ===="),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world!  ====="),
	ok.

title_short_trim_spaces_after_trailer(_) ->
	doc("Spaces after the trailer must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world! =         "),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world! ==       "),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world! ===     "),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world! ====   "),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ===== "),
	ok.

title_short_trim_spaces_before_after_trailer(_) ->
	doc("Spaces before and after the trailer must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!     =     "),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!    ==    "),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!   ===   "),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!  ====  "),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ===== "),
	ok.

title_short_trim_spaces_before_after_title_trailer(_) ->
	doc("Spaces before and after both the title and the trailer must be ignored. (11.2)"),
	[{title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!     =     "),
	[{title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!    ==    "),
	[{title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!   ===   "),
	[{title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!  ====  "),
	[{title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world! ===== "),
	ok.

title_short_wrong_trailer(_) ->
	doc("The delimiters must be the same size when a trailer is present. (11.2)"),
	[{title, #{level := 0}, <<"Hello world! ===">>, _}] = parse("= Hello world! ==="),
	[{title, #{level := 1}, <<"Hello world! ====">>, _}] = parse("== Hello world! ===="),
	[{title, #{level := 2}, <<"Hello world! =====">>, _}] = parse("=== Hello world! ====="),
	[{title, #{level := 3}, <<"Hello world! =">>, _}] = parse("==== Hello world! ="),
	[{title, #{level := 4}, <<"Hello world! ==">>, _}] = parse("===== Hello world! =="),
	ok.

%% Normal paragraphs.

%% @todo Other kinds of paragraphs. (15 excluding 15.1)

paragraph(_) ->
	doc("Normal paragraph. (15.1)"),
	[{p, _, <<"Hello world this is a paragraph peace.">>, _}] = parse(
		"Hello world\n"
		"this is a paragraph\n"
		"peace.\n"),
	[
		{p, _, <<"Hello world this is a paragraph peace.">>, _},
		{p, _, <<"This is another paragraph.">>, _}
	] = parse(
		"Hello world\n"
		"this is a paragraph\n"
		"peace.\n"
		"\n"
		"This is another paragraph.\n"),
	ok.

paragraph_title(_) ->
	doc("Paragraph preceded by a block title. (12, 15.1)"),
	[{p, #{title := <<"Block title!">>}, <<"Hello world this is a paragraph peace.">>, _}] = parse(
		".Block title!\n"
		"Hello world\n"
		"this is a paragraph\n"
		"peace.\n"),
	ok.

%% Listing blocks.

listing(_) ->
	doc("Listing blocks retain line and whitespace formatting. (16.2)"),
	Source = <<
		"init(Req, State) ->\n"
		"    {ok, Req, State}.">>,
	[{listing, _, Source, _}] = parse(iolist_to_binary([
		"----\n",
		Source, "\n"
		"----\n"])),
	ok.

listing_title(_) ->
	doc("Listing block with title. (12, 16.2)"),
	[{listing, #{title := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
		".Block title!\n"
		"----\n"
		"1 = 2.\n"
		"----\n"),
	ok.

listing_filter_source(_) ->
	doc("Source code listing filter. (source-highlight-filter)"),
	Source = <<
		"init(Req, State) ->\n"
		"    {ok, Req, State}.">>,
	[{listing, #{language := <<"erlang">>}, Source, _}] = parse(iolist_to_binary([
		"[source,erlang]\n"
		"----\n",
		Source, "\n"
		"----\n"])),
	ok.

listing_filter_source_title(_) ->
	doc("Source code listing filter with title. (12, source-highlight-filter)"),
	[{listing, #{language := <<"erlang">>, title := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
		".Block title!\n"
		"[source,erlang]\n"
		"----\n"
		"1 = 2.\n"
		"----\n"),
	[{listing, #{language := <<"erlang">>, title := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
		"[source,erlang]\n"
		".Block title!\n"
		"----\n"
		"1 = 2.\n"
		"----\n"),
	ok.

%% Bulleted lists.

unordered_list(_) ->
	doc("Unoredered lists. (17.1)"),
	[{ul, _, [
		{li, _, [{p, _, <<"Hello!">>, _}], _}
	], _}] = parse("* Hello!"),
	[{ul, _, [
		{li, _, [{p, _, <<"Hello!">>, _}], _},
		{li, _, [{p, _, <<"World!">>, _}], _},
		{li, _, [{p, _, <<"Hehe.">>, _}], _}
	], _}] = parse(
		"* Hello!\n"
		"* World!\n"
		"* Hehe.\n"),
	%% @todo Lists with -, **, ***, ****, *****.
	ok.

%% @todo doc("Unoredered lists ignore optional indentation. (17)"),
%% @todo list item continuation
%% @todo comment block/line terminates list
%% @todo listindex attribute
%% @todo block title probably?

%% Labeled lists. (17, 17.3)

labeled_list(_) ->
	doc("Labeled lists. (17.3)"),
	[{ll, _, [
		{li, #{label := <<"The label">>}, [{p, _, <<"The value!">>, _}], _}
	], _}] = parse("The label:: The value!"),
	[{ll, _, [
		{li, #{label := <<"The label">>}, [{p, _, <<"The value!">>, _}], _}
	], _}] = parse(
		"The label::\n"
		"\n"
		"The value!"),
	ok.

%% @todo Very little was implemented from labeled lists. They need more work.

%% Macros.

comment_line(_) ->
	doc("Lines starting with two slashes are treated as comments. (21.2.3)"),
	[{comment, _, <<"This is a comment.">>, _}] = parse("// This is a comment."),
	[{comment, _, <<"This is a comment.">>, _}] = parse("//   This is a comment.  "),
	ok.

%% Tables. (23)

table(_) ->
	%% @todo I think I read somewhere that paragraphs are not allowed in cells... Double check.
	[{table, _, [
		{row, _, [
			{cell, _, [{p, _, <<"1">>, _}], _},
			{cell, _, [{p, _, <<"2">>, _}], _},
			{cell, _, [{p, _, <<"A">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{p, _, <<"3">>, _}], _},
			{cell, _, [{p, _, <<"4">>, _}], _},
			{cell, _, [{p, _, <<"B">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{p, _, <<"5">>, _}], _},
			{cell, _, [{p, _, <<"6">>, _}], _},
			{cell, _, [{p, _, <<"C">>, _}], _}
		], _}
	], _}]= parse(
		"|=======\n"
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C\n"
		"|======="),
	ok.

%% @todo Check attributes also.
%% @todo Plenty more cases to test for tables.
