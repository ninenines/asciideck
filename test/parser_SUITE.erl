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

-module(parser_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(asciideck, [parse/1]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, blocks}].

%% @todo Test formatting too!
groups() ->
	[{blocks, [parallel], ct_helper:all(?MODULE)}].

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

%% Text formatting.

quoted_text_strong(_) ->
	doc("Strong text formatting. (10.1)"),
	[{paragraph, _, [{strong, _, <<"Hello beautiful world!">>, _}], _}] =
		parse("*Hello beautiful world!*"),
	[{paragraph, _, [{strong, _, <<"Hello">>, _}, <<" beautiful world!">>], _}] =
		parse("*Hello* beautiful world!"),
	[{paragraph, _, [<<"Hello ">>, {strong, _, <<"beautiful">>, _}, <<" world!">>], _}] =
		parse("Hello *beautiful* world!"),
	[{paragraph, _, [<<"Hello beautiful ">>, {strong, _, <<"world!">>, _}], _}] =
		parse("Hello beautiful *world!*"),
	[{paragraph, _, [<<"Hello beautiful ">>, {strong, _, <<"multiline world!">>, _}, <<" lol">>], _}] =
		parse("Hello beautiful *multiline\nworld!* lol"),
	%% Nested formatting.
	[{paragraph, _, [{strong, _, [
		<<"Hello ">>,
		{link, #{target := <<"downloads/cowboy-2.0.tgz">>}, <<"2.0">>, _},
		<<" world!">>
	], _}], _}] =
		parse("*Hello link:downloads/cowboy-2.0.tgz[2.0] world!*"),
	ok.

quoted_text_literal_mono(_) ->
	doc("Literal monospace text formatting. (10.1)"),
	[{paragraph, _, [{inline_literal_passthrough, _, <<"Hello beautiful world!">>, _}], _}] =
		parse("`Hello beautiful world!`"),
	[{paragraph, _, [{inline_literal_passthrough, _, <<"Hello">>, _}, <<" beautiful world!">>], _}] =
		parse("`Hello` beautiful world!"),
	[{paragraph, _, [<<"Hello ">>, {inline_literal_passthrough, _, <<"beautiful">>, _}, <<" world!">>], _}] =
		parse("Hello `beautiful` world!"),
	[{paragraph, _, [<<"Hello beautiful ">>, {inline_literal_passthrough, _, <<"world!">>, _}], _}] =
		parse("Hello beautiful `world!`"),
	[{paragraph, _, [<<"Hello beautiful ">>, {inline_literal_passthrough, _, <<"multiline\nworld!">>, _}, <<" lol">>], _}] =
		parse("Hello beautiful `multiline\nworld!` lol"),
	%% No text formatting must occur inside backticks.
	[{paragraph, _, [{inline_literal_passthrough, _, <<"Hello *beautiful* world!">>, _}], _}] =
		parse("`Hello *beautiful* world!`"),
	ok.

%% Title.

%% @todo Long titles. (11.1)
%% @todo Floating titles. (11.3)

title_short(_) ->
	doc("The trailing title delimiter is optional. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!"),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!"),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!"),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!"),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world!"),
	ok.

title_short_no_spaces(_) ->
	doc("One or more spaces must fall between the title and the delimiter. (11.2)"),
	[{paragraph, _, <<"=Hello world!">>, _}] = parse("=Hello world!"),
	[{paragraph, _, <<"==Hello world!">>, _}] = parse("==Hello world!"),
	[{paragraph, _, <<"===Hello world!">>, _}] = parse("===Hello world!"),
	[{paragraph, _, <<"====Hello world!">>, _}] = parse("====Hello world!"),
	[{paragraph, _, <<"=====Hello world!">>, _}] = parse("=====Hello world!"),
	ok.

title_short_trim_spaces_before(_) ->
	doc("Spaces between the title and delimiter must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!"),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!"),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!"),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!"),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world!"),
	ok.

title_short_trim_spaces_after(_) ->
	doc("Spaces after the title must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!     "),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!    "),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!   "),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!  "),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! "),
	ok.

title_short_trim_spaces_before_after(_) ->
	doc("Spaces before and after the title must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!     "),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!    "),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!   "),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!  "),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world! "),
	ok.

title_short_trailer(_) ->
	doc("The trailing title delimiter is optional. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world! ="),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world! =="),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world! ==="),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world! ===="),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ====="),
	ok.

title_short_trailer_no_spaces(_) ->
	doc("One or more spaces must fall between the title and the trailer. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!=">>, _}] = parse("= Hello world!="),
	[{section_title, #{level := 1}, <<"Hello world!==">>, _}] = parse("== Hello world!=="),
	[{section_title, #{level := 2}, <<"Hello world!===">>, _}] = parse("=== Hello world!==="),
	[{section_title, #{level := 3}, <<"Hello world!====">>, _}] = parse("==== Hello world!===="),
	[{section_title, #{level := 4}, <<"Hello world!=====">>, _}] = parse("===== Hello world!====="),
	ok.

title_short_trim_spaces_before_trailer(_) ->
	doc("Spaces between the title and trailer must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!          ="),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!        =="),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!      ==="),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!    ===="),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world!  ====="),
	ok.

title_short_trim_spaces_after_trailer(_) ->
	doc("Spaces after the trailer must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world! =         "),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world! ==       "),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world! ===     "),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world! ====   "),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ===== "),
	ok.

title_short_trim_spaces_before_after_trailer(_) ->
	doc("Spaces before and after the trailer must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!     =     "),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!    ==    "),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!   ===   "),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!  ====  "),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world! ===== "),
	ok.

title_short_trim_spaces_before_after_title_trailer(_) ->
	doc("Spaces before and after both the title and the trailer must be ignored. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("=      Hello world!     =     "),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("==     Hello world!    ==    "),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("===    Hello world!   ===   "),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("====   Hello world!  ====  "),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("=====  Hello world! ===== "),
	ok.

title_short_wrong_trailer(_) ->
	doc("The delimiters must be the same size when a trailer is present. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world! ===">>, _}] = parse("= Hello world! ==="),
	[{section_title, #{level := 1}, <<"Hello world! ====">>, _}] = parse("== Hello world! ===="),
	[{section_title, #{level := 2}, <<"Hello world! =====">>, _}] = parse("=== Hello world! ====="),
	[{section_title, #{level := 3}, <<"Hello world! =">>, _}] = parse("==== Hello world! ="),
	[{section_title, #{level := 4}, <<"Hello world! ==">>, _}] = parse("===== Hello world! =="),
	ok.

%% Normal paragraphs.

%% @todo Other kinds of paragraphs. (15 excluding 15.1)

paragraph(_) ->
	doc("Normal paragraph. (15.1)"),
	[{paragraph, _, <<"Hello world this is a paragraph peace.">>, _}] = parse(
		"Hello world\n"
		"this is a paragraph\n"
		"peace.\n"),
	[
		{paragraph, _, <<"Hello world this is a paragraph peace.">>, _},
		{paragraph, _, <<"This is another paragraph.">>, _}
	] = parse(
		"Hello world\n"
		"this is a paragraph\n"
		"peace.\n"
		"\n"
		"This is another paragraph.\n"),
	ok.

paragraph_title(_) ->
	doc("Paragraph preceded by a block title. (12, 15.1)"),
	[{paragraph, #{<<"title">> := <<"Block title!">>}, <<"Hello world this is a paragraph peace.">>, _}] = parse(
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
	[{listing_block, _, Source, _}] = parse(iolist_to_binary([
		"----\n",
		Source, "\n"
		"----\n"])),
	ok.

listing_title(_) ->
	doc("Listing block with title. (12, 16.2)"),
	[{listing_block, #{<<"title">> := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
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
	[{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>}, Source, _}] = parse(iolist_to_binary([
		"[source,erlang]\n"
		"----\n",
		Source, "\n"
		"----\n"])),
	ok.

listing_filter_source_title(_) ->
	doc("Source code listing filter with title. (12, source-highlight-filter)"),
	[{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>, <<"title">> := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
		".Block title!\n"
		"[source,erlang]\n"
		"----\n"
		"1 = 2.\n"
		"----\n"),
	[{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>, <<"title">> := <<"Block title!">>}, <<"1 = 2.">>, _}] = parse(
		"[source,erlang]\n"
		".Block title!\n"
		"----\n"
		"1 = 2.\n"
		"----\n"),
	ok.

%% Bulleted lists.

unordered_list(_) ->
	doc("Unoredered lists. (17.1)"),
	[{list, #{type := bulleted}, [
		{list_item, _, [{paragraph, #{}, <<"Hello!">>, _}], _}
	], _}] = parse("* Hello!"),
	[{list, #{type := bulleted}, [
		{list_item, _, [{paragraph, #{}, <<"Hello!">>, _}], _},
		{list_item, _, [{paragraph, #{}, <<"World!">>, _}], _},
		{list_item, _, [{paragraph, #{}, <<"Hehe.">>, _}], _}
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
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>},
			[{paragraph, #{}, <<"The value!">>, _}], _}
	], _}] = parse("The label:: The value!"),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>},
			[{paragraph, #{}, <<"The value!">>, _}], _},
		{list_item, #{label := <<"More labels">>},
			[{paragraph, #{}, <<"More values!">>, _}], _}
	], _}] = parse(
		"The label:: The value!\n"
		"More labels:: More values!\n"),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>},
			[{paragraph, #{}, <<"The value!">>, _}], _}
	], _}] = parse(
		"The label::\n"
		"\n"
		"The value!"),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>},
			[{paragraph, #{}, <<"The value!">>, _}], _}
	], _}] = parse(
		"The label::\n"
		"    The value!"),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>}, [
			{paragraph, _, <<"The value!">>, _},
			{paragraph, _, <<"With continuations!">>, _},
			{paragraph, _, <<"OK good.">>, _}
		], _}
	], _}] = parse(
		"The label::\n"
		"\n"
		"The value!\n"
		"+\n"
		"With continuations!\n"
		"+\n"
		"OK good."),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"The label">>}, [
			{paragraph, #{}, <<"The value!">>, _},
			{list, #{type := bulleted}, [
				{list_item, _, [{paragraph, #{}, <<"first list item">>, _}], _},
				{list_item, _, [{paragraph, #{}, <<"second list item">>, _}], _},
				{list_item, _, [{paragraph, #{}, <<"third list item">>, _}], _}
			], _}
		], _}
	], _}] = parse(
		"The label::\n"
		"\n"
		"The value!\n"
		"+\n"
		"    * first list item\n"
		"    * second list\n"
		"      item\n"
		"    * third list\n"
		"      item\n"
		"\n"),
	ok.

%% Macros.

rel_link(_) ->
	doc("Relative links are built using the link:Target[Caption] macro. (21.1.3)"),
	[{paragraph, _, [
		{link, #{target := <<"downloads/cowboy-2.0.tgz">>}, <<"2.0">>, _}
	], _}] = parse("link:downloads/cowboy-2.0.tgz[2.0]"),
	[{paragraph, _, [
		<<"Download ">>,
		{link, #{target := <<"downloads/cowboy-2.0.zip">>}, <<"Cowboy 2.0">>, _},
		<<" as zip">>
	], _}] = parse("Download link:downloads/cowboy-2.0.zip[Cowboy 2.0] as zip"),
	ok.

comment_line(_) ->
	doc("Lines starting with two slashes are treated as comments. (21.2.3)"),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse("//This is a comment."),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse("// This is a comment."),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse("//   This is a comment.  "),
	[
		{comment_line, _, <<"First line.">>, _},
		{comment_line, _, <<"Second line.">>, _}
	] = parse(
		"// First line.\n"
		"// Second line.\n"),
	ok.

%% Tables. (23)

table(_) ->
	%% @todo I think I read somewhere that paragraphs are not allowed in cells... Double check.
	[{table, _, [
		{row, _, [
			{cell, _, <<"1">>, _},
			{cell, _, <<"2">>, _},
			{cell, _, <<"A">>, _}
		], _},
		{row, _, [
			{cell, _, <<"3">>, _},
			{cell, _, <<"4">>, _},
			{cell, _, <<"B">>, _}
		], _},
		{row, _, [
			{cell, _, <<"5">>, _},
			{cell, _, <<"6">>, _},
			{cell, _, <<"C">>, _}
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
