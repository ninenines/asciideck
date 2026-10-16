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

block_line(_) ->
	doc("A block is annotated with its own line, not the next one."),
	[
		{section_title, _, <<"Title">>, #{line := 1}},
		{paragraph, _, <<"Paragraph continued.">>, #{line := 3}},
		{listing_block, #{<<"title">> := <<"Listing">>}, <<"Hello!">>, #{line := 7}},
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One">>, _}], #{line := 11}},
			{list_item, _, [{paragraph, _, <<"Two">>, _}], #{line := 12}}
		], #{line := 11}},
		{comment_line, _, <<"Comment">>, #{line := 14}}
	] = parse(
		"= Title\n"
		"\n"
		"Paragraph\n"
		"continued.\n"
		"\n"
		".Listing\n"
		"----\n"
		"Hello!\n"
		"----\n"
		"\n"
		"* One\n"
		"* Two\n"
		"\n"
		"// Comment\n"),
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

%% @todo Other kinds of paragraphs. (15.3)

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

%% Literal paragraphs.

literal_paragraph(_) ->
	doc("Indented paragraphs are literal. (15.2)"),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"Consul *necessitatibus* per id,">>, _}] =
		parse("  Consul *necessitatibus* per id,"),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"Consul *necessitatibus* per id,\n"
		  "consetetur, eu pro everti postulant\n"
		  "homero verear ea mea, qui.">>, _}] = parse(
			"  Consul *necessitatibus* per id,\n"
			"  consetetur, eu pro everti postulant\n"
			"  homero verear ea mea, qui.\n"),
	%% Verbatim: no inline formatting.
	[{literal_paragraph, _, <<"Hello *world*!">>, _}] =
		parse("  Hello *world*!"),
	%% Relative indentation is preserved.
	[{literal_paragraph, _, <<"foo() ->\n    ok.">>, _}] = parse(
		"  foo() ->\n"
		"      ok.\n"),
	%% Indented // and + stay inside the literal paragraph.
	[{literal_paragraph, _, <<"x = 1\n// increment\nx = x + 1\n+">>, _}] = parse(
		"  x = 1\n"
		"  // increment\n"
		"  x = x + 1\n"
		"  +\n"),
	ok.

literal_paragraph_style(_) ->
	doc("Literal style applied to the following paragraph. (15.2)"),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"Consul *necessitatibus* per id,">>, _}] = parse(
			"[literal]\n"
			"Consul *necessitatibus* per id,\n"),
	%% Attribute-list styles override implicit literal paragraphs.
	[{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>},
		<<"f() -> ok.">>, _}] = parse(
			"[source,erlang]\n"
			"    f() -> ok.\n"),
	[{admonition, #{style := <<"NOTE">>}, [
		<<"Use ">>,
		{strong, _, <<"bold">>, _},
		<<".">>
	], _}] = parse(
		"[NOTE]\n"
		"    Use *bold*.\n"),
	ok.

literal_paragraph_in_list(_) ->
	doc("A literal paragraph immediately following a list item is a child. (17, 15.2)"),
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"Lorem">>}, [
			{paragraph, _, <<"Fusce euismod commodo velit.">>, _},
			{literal_paragraph, _, <<"Fusce euismod commodo velit.">>, _}
		], _}
	], _}] = parse(
		"Lorem::\n"
		"  Fusce euismod commodo velit.\n"
		"\n"
		"  Fusce euismod commodo velit.\n"),
	ok.

%% Admonition paragraphs.

admonition_paragraph(_) ->
	doc("Admonition paragraphs. (15.4)"),
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _}] =
		parse("NOTE: This is a note."),
	[{admonition, #{style := <<"TIP">>}, <<"This is a tip.">>, _}] =
		parse("TIP: This is a tip."),
	[{admonition, #{style := <<"IMPORTANT">>}, <<"This is important.">>, _}] =
		parse("IMPORTANT: This is important."),
	[{admonition, #{style := <<"WARNING">>}, <<"This is a warning.">>, _}] =
		parse("WARNING: This is a warning."),
	[{admonition, #{style := <<"CAUTION">>}, <<"This is a caution.">>, _}] =
		parse("CAUTION: This is a caution."),
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note. It spans two lines.">>, _}] =
		parse(
			"NOTE: This is a note.\n"
			"It spans two lines.\n"),
	[
		{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _},
		{paragraph, _, <<"This is a paragraph.">>, _}
	] = parse(
		"NOTE: This is a note.\n"
		"\n"
		"This is a paragraph.\n"),
	[{paragraph, _, <<"NOTE:This is not an admonition.">>, _}] =
		parse("NOTE:This is not an admonition."),
	[{paragraph, _, <<"Note: This is not an admonition.">>, _}] =
		parse("Note: This is not an admonition."),
	ok.

admonition_paragraph_style(_) ->
	doc("Admonition style applied to the following paragraph. (15.4)"),
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _}] =
		parse(
			"[NOTE]\n"
			"This is a note.\n"),
	[{admonition, #{style := <<"TIP">>}, <<"This is a tip.">>, _}] =
		parse(
			"[TIP]\n"
			"This is a tip.\n"),
	[{admonition, #{style := <<"IMPORTANT">>}, <<"This is important.">>, _}] =
		parse(
			"[IMPORTANT]\n"
			"This is important.\n"),
	[{admonition, #{style := <<"WARNING">>}, <<"This is a warning.">>, _}] =
		parse(
			"[WARNING]\n"
			"This is a warning.\n"),
	[{admonition, #{style := <<"CAUTION">>}, <<"This is a caution.">>, _}] =
		parse(
			"[CAUTION]\n"
			"This is a caution.\n"),
	ok.

admonition_paragraph_title(_) ->
	doc("Admonition paragraph preceded by a block title. (12, 15.4)"),
	[{admonition, #{style := <<"NOTE">>, <<"title">> := <<"Watch out">>},
		<<"This is a note.">>, _}] = parse(
			".Watch out\n"
			"NOTE: This is a note.\n"),
	[{admonition, #{style := <<"NOTE">>, <<"title">> := <<"Watch out">>},
		<<"This is a note.">>, _}] = parse(
			".Watch out\n"
			"[NOTE]\n"
			"This is a note.\n"),
	ok.

admonition_paragraph_formatting(_) ->
	doc("Admonition paragraphs honor inline formatting. (10.1, 15.4)"),
	[{admonition, #{style := <<"NOTE">>}, [
		<<"This is ">>,
		{strong, _, <<"important">>, _},
		<<".">>
	], _}] = parse("NOTE: This is *important*."),
	ok.

%% Open blocks.

open_block(_) ->
	doc("Open blocks group block elements. (16.10)"),
	[{open_block, _, [
		{paragraph, _, <<"Hello!">>, _}
	], _}] = parse(
		"--\n"
		"Hello!\n"
		"--\n"),
	[{open_block, _, [
		{paragraph, _, <<"Hello!">>, _},
		{paragraph, _, <<"World!">>, _}
	], _}] = parse(
		"--\n"
		"Hello!\n"
		"\n"
		"World!\n"
		"--\n"),
	[{open_block, #{1 := <<"abstract">>}, [
		{paragraph, _, <<"In this paper we will.">>, _}
	], _}] = parse(
		"[abstract]\n"
		"--\n"
		"In this paper we will.\n"
		"--\n"),
	[{open_block, #{<<"title">> := <<"Intro">>}, [
		{paragraph, _, <<"Optional part introduction goes here.">>, _}
	], _}] = parse(
		".Intro\n"
		"--\n"
		"Optional part introduction goes here.\n"
		"--\n"),
	%% Inline formatting inside the open block.
	[{open_block, _, [
		{paragraph, _, [{strong, _, <<"Hello">>, _}, <<"!">>], _}
	], _}] = parse(
		"--\n"
		"*Hello*!\n"
		"--\n"),
	%% Lists inside the open block are aggregated.
	[{open_block, _, [
		{list, #{type := bulleted}, [
			{list_item, _, [{paragraph, _, <<"One">>, _}], _},
			{list_item, _, [{paragraph, _, <<"Two">>, _}], _}
		], _}
	], _}] = parse(
		"--\n"
		"* One\n"
		"* Two\n"
		"--\n"),
	%% Open blocks group list item continuation content.
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"item">>, _},
			{open_block, _, [
				{list, #{type := bulleted}, [
					{list_item, _, [{paragraph, _, <<"nested">>, _}], _},
					{list_item, _, [{paragraph, _, <<"items">>, _}], _}
				], _}
			], _}
		], _}
	], _}] = parse(
		"* item\n"
		"+\n"
		"--\n"
		"* nested\n"
		"* items\n"
		"--\n"),
	ok.

open_block_line(_) ->
	doc("Open block children keep the file line, not a line in the slice."),
	[
		{paragraph, _, <<"line1">>, #{line := 1}},
		{open_block, _, [
			{paragraph, _, <<"Hello!">>, #{line := 4}},
			{paragraph, _, <<"World!">>, #{line := 6}}
		], #{line := 3}}
	] = parse(
		"line1\n"
		"\n"
		"--\n"
		"Hello!\n"
		"\n"
		"World!\n"
		"--\n"),
	[
		{open_block, _, [
			{paragraph, _, <<"Hello!">>, #{line := 2}},
			{listing_block, _, <<"Hi">>, #{line := 4}}
		], #{line := 1}}
	] = parse(
		"--\n"
		"Hello!\n"
		"\n"
		"----\n"
		"Hi\n"
		"----\n"
		"--\n"),
	%% A table inside the open block keeps both offsets.
	[
		{open_block, _, [
			{table, _, [
				{row, _, [
					{cell, _, [{paragraph, _, <<"A">>, #{line := 3}}], _},
					{cell, _, [{paragraph, _, <<"B">>, #{line := 3}}], _}
				], _}
			], #{line := 2}}
		], #{line := 1}}
	] = parse(
		"--\n"
		"|===\n"
		"|A |B\n"
		"|===\n"
		"--\n"),
	%% A leading empty line is skipped, and the next block keeps its file line.
	[{open_block, _, [
		{paragraph, _, <<"Hello">>, #{line := 3}}
	], #{line := 1}}] = parse(
		"--\n"
		"\n"
		"Hello\n"
		"--\n"),
	[{open_block, _, [
		{paragraph, _, <<"Hello">>, #{line := 4}},
		{paragraph, _, <<"World">>, #{line := 6}}
	], #{line := 1}}] = parse(
		"--\n"
		"\n"
		"\n"
		"Hello\n"
		"\n"
		"World\n"
		"--\n"),
	%% A whitespace line was already kept. It must stay on its own line.
	[{open_block, _, [
		{paragraph, _, <<"Hello">>, #{line := 3}}
	], #{line := 1}}] = parse(
		"--\n"
		" \n"
		"Hello\n"
		"--\n"),
	%% The open block offset and the table's own leading empty line both apply.
	[{open_block, _, [
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"A">>, #{line := 5}}], _},
				{cell, _, [{paragraph, _, <<"B">>, #{line := 5}}], _}
			], _}
		], #{line := 3}}
	], #{line := 1}}] = parse(
		"--\n"
		"\n"
		"|===\n"
		"\n"
		"|A |B\n"
		"|===\n"
		"--\n"),
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

paragraph_filter_source(_) ->
	doc("Source code listing filter as a paragraph. (source-highlight-filter)"),
	Source = <<
		"init(Req, State) ->\n"
		"    {ok, Req, State}.">>,
	[{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>}, Source, _}] = parse(iolist_to_binary([
		"[source,erlang]\n",
		Source, "\n"])),
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

%% Callout lists.

callout_list(_) ->
	doc("Callout lists annotate verbatim text. (20)"),
	[{list, #{type := callout}, [
		{list_item, #{number := 1},
			[{paragraph, _, <<"This directory holds MS-DOS.">>, _}], _},
		{list_item, #{number := 2},
			[{paragraph, _, <<"System startup code for DOS.">>, _}], _},
		{list_item, #{number := 3},
			[{paragraph, _, <<"Some sort of Windows 3.1 hack.">>, _}], _}
	], _}] = parse(
		"<1> This directory holds MS-DOS.\n"
		"<2> System startup code for DOS.\n"
		"<3> Some sort of Windows 3.1 hack.\n"),
	[{list, #{type := callout}, [
		{list_item, #{number := 1},
			[{paragraph, _, <<"First.">>, _}], _},
		{list_item, #{number := 2},
			[{paragraph, _, <<"Second.">>, _}], _}
	], _}] = parse(
		"1> First.\n"
		"2> Second.\n"),
	[{list, #{type := callout}, [
		{list_item, #{number := auto},
			[{paragraph, _, <<"Implicit.">>, _}], _}
	], _}] = parse("> Implicit.\n"),
	%% A listing followed by a callout list.
	[
		{listing_block, _, <<"10/17/97   9:04         <DIR>    bin <1>">>, _},
		{list, #{type := callout}, [
			{list_item, #{number := 1},
				[{paragraph, _, <<"This directory holds MS-DOS.">>, _}], _}
		], _}
	] = parse(
		"----\n"
		"10/17/97   9:04         <DIR>    bin <1>\n"
		"----\n"
		"<1> This directory holds MS-DOS.\n"),
	ok.

%% Attribute entries.

attribute_entry(_) ->
	doc("Attribute entries set document attributes. (28)"),
	[{attribute_entry, #{name := <<"author">>, unset := false},
		<<"Stuart Rackham">>, _}] = parse(":author: Stuart Rackham"),
	[{attribute_entry, #{name := <<"authorinitials">>, unset := false},
		<<"JB">>, _}] = parse(":Author Initials: JB"),
	[{attribute_entry, #{name := <<"toc">>, unset := false},
		<<>>, _}] = parse(":toc:"),
	[{attribute_entry, #{name := <<"toc">>, unset := true},
		<<>>, _}] = parse(":toc!:"),
	[{attribute_entry, #{name := <<"description">>, unset := false},
		<<"AsciiDoc is a text document format for writing notes, "
		  "documentation, articles, books, slideshows, web pages "
		  "and man pages.">>, _}] = parse(
			":description: AsciiDoc is a text document format for writing notes, +\n"
			"              documentation, articles, books, slideshows, web pages +\n"
			"              and man pages.\n"),
	%% Attribute entries do not consume a following block title.
	[
		{attribute_entry, #{name := <<"author">>}, <<"Loic">>, _},
		{paragraph, #{<<"title">> := <<"Notes">>}, <<"Hello.">>, _}
	] = parse(
		":author: Loic\n"
		".Notes\n"
		"Hello.\n"),
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
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"1">>, _}], _},
			{cell, _, [{paragraph, _, <<"2">>, _}], _},
			{cell, _, [{paragraph, _, <<"A">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"3">>, _}], _},
			{cell, _, [{paragraph, _, <<"4">>, _}], _},
			{cell, _, [{paragraph, _, <<"B">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"5">>, _}], _},
			{cell, _, [{paragraph, _, <<"6">>, _}], _},
			{cell, _, [{paragraph, _, <<"C">>, _}], _}
		], _}
	], _}]= parse(
		"|=======\n"
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C\n"
		"|======="),
	ok.

table_cell_line(_) ->
	doc("A table cell paragraph keeps the file line of its row."),
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"A">>, #{line := 2}}], _},
			{cell, _, [{paragraph, _, <<"B">>, #{line := 2}}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"C">>, #{line := 3}}], _},
			{cell, _, [{paragraph, _, <<"D">>, #{line := 3}}], _}
		], _}
	], #{line := 1}}] = parse(
		"|===\n"
		"|A |B\n"
		"|C |D\n"
		"|===\n"),
	%% The row line follows the table, wherever the table sits.
	[
		{paragraph, _, <<"Before">>, #{line := 1}},
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"A">>, #{line := 4}}], _},
				{cell, _, [{paragraph, _, <<"B">>, #{line := 4}}], _}
			], _}
		], #{line := 3}}
	] = parse(
		"Before\n"
		"\n"
		"|===\n"
		"|A |B\n"
		"|===\n"),
	%% A newline inside a cell moves the following cell down.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"One two">>, #{line := 3}}], _},
			{cell, _, [{paragraph, _, <<"Three">>, #{line := 4}}], _}
		], _}
	], #{line := 2}}] = parse(
		"[cols=\"2\"]\n"
		"|===\n"
		"|One\n"
		"two|Three\n"
		"|===\n"),
	%% A blank line under the delimiter is not a row.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"A">>, #{line := 3}}], _},
			{cell, _, [{paragraph, _, <<"B">>, #{line := 3}}], _}
		], _}
	], #{line := 1}}] = parse(
		"|===\n"
		"\n"
		"|A |B\n"
		"|===\n"),
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"A">>, #{line := 4}}], _},
			{cell, _, [{paragraph, _, <<"B">>, #{line := 4}}], _}
		], _}
	], #{line := 2}}] = parse(
		"[cols=\"2\"]\n"
		"|===\n"
		"\n"
		"|A |B\n"
		"|===\n"),
	ok.

%% @todo Check attributes also.
%% @todo Plenty more cases to test for tables.
