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

quoted_text_unconstrained(_) ->
	doc("Unconstrained quotes may appear inside a word. (10.1.2)"),
	[{paragraph, _, [
		<<"The ">>,
		{strong, _, <<"DEPRECATED">>, _},
		<<" form.">>
	], _}] = parse("The **DEPRECATED** form."),
	[{paragraph, _, [
		{strong, _, <<"F">>, _},
		<<"ile Open.">>
	], _}] = parse("**F**ile Open."),
	[{paragraph, _, [
		<<"See the ">>,
		{emphasized, _, <<"File">>, _},
		<<"name.">>
	], _}] = parse("See the __File__name."),
	[{paragraph, _, [{strong, _, <<"multi line">>, _}], _}]
		= parse("**multi\nline**"),
	%% The single mark stays literal when a doubled mark follows.
	[{paragraph, _, [
		<<"2 * 3 = ">>,
		{strong, _, <<"six">>, _},
		<<".">>
	], _}] = parse("2 * 3 = **six**."),
	[{paragraph, _, [
		<<"a _ b and ">>,
		{emphasized, _, <<"call">>, _},
		<<".">>
	], _}] = parse("a _ b and __call__."),
	ok.

quoted_text_unconstrained_mono(_) ->
	doc("Unconstrained monospace may appear inside a word. (10.1.2)"),
	[{paragraph, _, [
		<<"Use ">>,
		{monospaced, _, <<"lists:reverse/1">>, _},
		<<".">>
	], _}] = parse("Use ++lists:reverse/1++."),
	[{paragraph, _, [
		<<"a">>,
		{monospaced, _, <<"b">>, _},
		<<"c">>
	], _}] = parse("a++b++c"),
	%% +++ is a passthrough, not a ++ quote.
	[{paragraph, _, [{passthrough, _, <<"<b>raw</b>">>, _}], _}]
		= parse("+++<b>raw</b>+++"),
	[{paragraph, _, [
		{monospaced, _, <<"a">>, _},
		<<" ">>,
		{passthrough, _, <<"b">>, _}
	], _}] = parse("++a++ +++b+++"),
	%% A +++ span is not quote text for the surrounding marks.
	[{paragraph, _, [{strong, _, [
		<<"bold ">>,
		{passthrough, _, <<"*not*">>, _},
		<<" more">>
	], _}], _}] = parse("*bold +++*not*+++ more*"),
	[{paragraph, _, [{monospaced, _, [
		<<" *bold ">>,
		{passthrough, _, <<"*x*">>, _},
		<<" more ">>
	], _}], _}] = parse("++ *bold +++*x*+++ more ++"),
	[{paragraph, _, [{inline_literal_passthrough, _, [
		<<"tick ">>,
		{passthrough, _, <<"`in`">>, _},
		<<" here">>
	], _}], _}] = parse("`tick +++`in`+++ here`"),
	ok.

quoted_text_constrained_mono(_) ->
	doc("Constrained monospace does not match inside a word. (10.1)"),
	[{paragraph, _, [
		<<"Use ">>,
		{monospaced, _, <<"lists:reverse/1">>, _},
		<<".">>
	], _}] = parse("Use +lists:reverse/1+."),
	[{paragraph, _, <<"See +x+y here.">>, _}] = parse("See +x+y here."),
	[{paragraph, _, [
		{monospaced, _, <<"x">>, _},
		<<"y">>
	], _}] = parse("++x++y"),
	[{paragraph, _, [
		<<"A ">>,
		{monospaced, _, <<"+">>, _},
		<<"+ B">>
	], _}] = parse("A ++++ B"),
	[{paragraph, _, [
		<<"A ">>,
		{monospaced, _, <<"+">>, _},
		<<" B">>
	], _}] = parse("A +++ B"),
	[{paragraph, _, [
		<<"see ">>,
		{monospaced, _, [
			<<"the ">>,
			{monospaced, _, <<"big">>, _},
			<<" end">>
		], _}
	], _}] = parse("see +the ++big++ end+"),
	[{paragraph, _, [{emphasized, _, <<"+a+">>, _}], _}]
		= parse("_+a+_"),
	[{paragraph, _, [
		{monospaced, _, <<"init">>, _},
		<<".">>,
		{monospaced, _, <<"loop">>, _}
	], _}] = parse("+init+.+loop+"),
	[{paragraph, _, <<"See :+code+: here.">>, _}]
		= parse("See :+code+: here."),
	ok.

quoted_text_super_sub(_) ->
	doc("Superscripts and subscripts. (10.2)"),
	[{paragraph, _, [
		<<"e">>,
		{superscript, _, <<"pi">>, _},
		<<"+1 and H">>,
		{subscript, _, <<"2">>, _},
		<<"O">>
	], _}] = parse("e^pi^+1 and H~2~O"),
	[{paragraph, _, [
		<<"x">>,
		{superscript, _, <<"10">>, _}
	], _}] = parse("x^10^"),
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

%% Underline titles (11.1) are intentionally not recognized.
%% @todo Floating titles. (11.3)

title_short(_) ->
	doc("The trailing title delimiter is optional. (11.2)"),
	[{section_title, #{level := 0}, <<"Hello world!">>, _}] = parse("= Hello world!"),
	[{section_title, #{level := 1}, <<"Hello world!">>, _}] = parse("== Hello world!"),
	[{section_title, #{level := 2}, <<"Hello world!">>, _}] = parse("=== Hello world!"),
	[{section_title, #{level := 3}, <<"Hello world!">>, _}] = parse("==== Hello world!"),
	[{section_title, #{level := 4}, <<"Hello world!">>, _}] = parse("===== Hello world!"),
	ok.

title_not_underlined(_) ->
	doc("An underline is not a section title. (11.1)"),
	[] = [T || {section_title, _, _, _} = T <- parse(
		"Hello\n"
		"----\n"
		"code\n"
		"----\n")],
	[] = [T || {section_title, _, _, _} = T <- parse(
		"Hello\n"
		"====\n"
		"code\n"
		"====\n")],
	[] = [T || {section_title, _, _, _} = T <- parse(
		"[NOTE]\n"
		"====\n")],
	%% A lone + stays a list continuation, not an underline title.
	[] = [T || {section_title, _, _, _} = T <- parse("+\n")],
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

paragraph_ends(_) ->
	doc("A paragraph ends at a delimiter, a table, a list, or an attribute list. (15.1)"),
	[
		{paragraph, _, <<"Hello">>, _},
		{listing_block, _, <<"code">>, _}
	] = parse(
		"Hello\n"
		"----\n"
		"code\n"
		"----\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{example_block, _, <<"code">>, _}
	] = parse(
		"Hello\n"
		"====\n"
		"code\n"
		"====\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{open_block, _, [
			{paragraph, _, <<"Inside">>, _}
		], _}
	] = parse(
		"Hello\n"
		"--\n"
		"Inside\n"
		"--\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"A">>, _}], _},
				{cell, _, [{paragraph, _, <<"B">>, _}], _}
			], _}
		], _}
	] = parse(
		"Hello\n"
		"|===\n"
		"|A |B\n"
		"|===\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{admonition, #{style := <<"NOTE">>}, <<"Watch out.">>, _}
	] = parse(
		"Hello\n"
		"[NOTE]\n"
		"Watch out.\n"),
	[
		{admonition, #{style := <<"NOTE">>}, <<"Before.">>, _},
		{listing_block, _, <<"code">>, _}
	] = parse(
		"NOTE: Before.\n"
		"----\n"
		"code\n"
		"----\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{list, #{type := bulleted}, [
			{list_item, _, [{paragraph, _, <<"One">>, _}], _}
		], _}
	] = parse(
		"Hello\n"
		"* One\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{list, #{type := numbered}, [
			{list_item, _, [{paragraph, _, <<"Two">>, _}], _}
		], _}
	] = parse(
		"Hello\n"
		". Two\n"),
	%% Three dashes are not a delimiter, and an unclosed bracket is not an attribute list.
	[{paragraph, _, <<"Hello --- still [not closed still.">>, _}] = parse(
		"Hello\n"
		"--- still\n"
		"[not closed\n"
		"still.\n"),
	%% An empty label is not a labeled list.
	[{paragraph, _, <<"Hello :: nope still.">>, _}] = parse(
		"Hello\n"
		":: nope\n"
		"still.\n"),
	[{paragraph, _, <<"Hello ;; nope still.">>, _}] = parse(
		"Hello\n"
		";; nope\n"
		"still.\n"),
	[{paragraph, _, <<"Hello ::: nope still.">>, _}] = parse(
		"Hello\n"
		"::: nope\n"
		"still.\n"),
	%% A real labeled list does end the paragraph.
	[
		{paragraph, _, <<"Hello">>, _},
		{list, #{type := labeled}, [
			{list_item, _, [{paragraph, _, <<"Answer">>, _}], _}
		], _}
	] = parse(
		"Hello\n"
		"Question:: Answer\n"),
	%% A callout needs whitespace and item text.
	[{paragraph, _, <<"Check 3>2 is false here.">>, _}] = parse(
		"Check\n"
		"3>2 is false\n"
		"here.\n"),
	[{paragraph, _, <<"<1>Note">>, _}] = parse("<1>Note\n"),
	[
		{paragraph, _, <<"Hello">>, _},
		{list, #{type := callout}, [
			{list_item, #{number := 1}, [{paragraph, _, <<"Note">>, _}], _}
		], _}
	] = parse(
		"Hello\n"
		"<1> Note\n"),
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

titled_literal_not_in_list(_) ->
	doc("A block title ends the list. A continuation keeps the block. (17.7)"),
	[
		{list, #{type := bulleted}, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{literal_paragraph, #{<<"title">> := <<"Code">>}, <<"foo()">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Code\n"
		"  foo()\n"),
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{literal_paragraph, #{<<"title">> := <<"Code">>}, <<"foo()">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		".Code\n"
		"  foo()\n"),
	%% A title attribute does not end the list.
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{literal_paragraph, #{<<"title">> := <<"Code">>}, <<"foo()">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"\n"
		"[title=\"Code\"]\n"
		"  foo()\n"),
	%% The indented paragraph is a child. The style is applied after.
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>}, <<"foo().">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"\n"
		"[source,erlang]\n"
		"  foo().\n"),
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{admonition, #{style := <<"NOTE">>}, <<"Careful.">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"\n"
		"[NOTE]\n"
		"  Careful.\n"),
	ok.

titled_list_ends(_) ->
	doc("A title line ends the list before the next item. (17.7)"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{list, #{<<"title">> := <<"Title">>}, [
			{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
		], _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"* Two.\n"),
	[
		{list, _, [
			{list_item, _, [
				{paragraph, _, <<"One.">>, _},
				{list, _, [
					{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
				], _}
			], _}
		], _},
		{list, #{<<"title">> := <<"Title">>}, [
			{list_item, _, [{paragraph, _, <<"Three.">>, _}], _}
		], _}
	] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"* Three.\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{list, #{<<"title">> := <<"Title">>}, [
			{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
		], _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"** Two.\n"),
	%% A continuation does not keep an item of the same list.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{list, #{<<"title">> := <<"Title">>}, [
			{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
		], _}
	] = parse(
		"* One.\n"
		"+\n"
		".Title\n"
		"* Two.\n"),
	%% A continuation does keep a deeper list, title included.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, #{<<"title">> := <<"Title">>}, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		".Title\n"
		"** Two.\n"),
	%% A title before + is not a continuation. The + starts
	%% the next paragraph, and a list marker stays in it.
	%% The inline pass turns the newline after + into a space.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ Second paragraph.">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"Second paragraph.\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ ** Two.">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"** Two.\n"),
	%% The inner list ends. The parent item takes the +.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _},
			{paragraph, #{<<"title">> := <<"Title">>}, <<"Para.">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"
		"Para.\n"),
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _},
			{list, #{<<"title">> := <<"Title">>}, [
				{list_item, _, [{paragraph, _, <<"Three.">>, _}], _}
			], _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"
		"** Three.\n"),
	ok.

%% A title before + makes the + paragraph text. The paragraph
%% reader keeps the following lines. A parent list can still
%% use that + as a continuation.

titled_plus_paragraph(_) ->
	doc("A title before + keeps the original paragraph lines. (15.1, 17.7)"),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"  foo()\n",
		<<"+   foo()">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"NOTE: careful\n"
		"still\n",
		<<"+ NOTE: careful still">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		".Other\n"
		"text\n",
		<<"+ .Other text">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"Term::\n"
		"  definition\n"
		"** Two.\n",
		<<"+ Term::   definition ** Two.">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"1> callout text\n",
		<<"+ 1> callout text">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"*** Three.\n",
		<<"+ *** Three.">>),
	%% A comment is skipped. The lines around it stay one paragraph.
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"// note\n"
		"text\n",
		<<"+ text">>),
	%% A delimited block ends the paragraph.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+">>, _},
		{table, _, _, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"|===\n"
		"|A\n"
		"|===\n"),
	%% An attribute list ends the paragraph too.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ text">>, _},
		{admonition, #{style := <<"NOTE">>}, <<"Careful.">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"text\n"
		"[NOTE]\n"
		"Careful.\n"),
	%% The style written with the title applies to this paragraph.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{listing_block, #{
			<<"title">> := <<"Title">>,
			1 := <<"source">>,
			2 := <<"erlang">>
		}, <<"+\ncode">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"[source,erlang]\n"
		"+\n"
		"code\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{listing_block, #{
			<<"title">> := <<"Title">>,
			1 := <<"source">>,
			2 := <<"erlang">>
		}, <<"+\ncode">>, _}
	] = parse(
		"* One.\n"
		"\n"
		"[source,erlang]\n"
		".Title\n"
		"+\n"
		"code\n"),
	%% The parent item takes the +. The listing is only the code.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _},
			{listing_block, #{
				<<"title">> := <<"Title">>,
				1 := <<"source">>,
				2 := <<"erlang">>
			}, <<"code">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"[source,erlang]\n"
		"+\n"
		"code\n"),
	%% Nothing after the + continues the parent and adds nothing.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"),
	%% A second + is its own paragraph.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ line">>, _},
		{paragraph, MoreAttrs, <<"+ more">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"line\n"
		"+\n"
		"more\n"),
	false = maps:is_key(<<"title">>, MoreAttrs),
	ok.

%% A delimiter or an attribute list ends the lines kept after +.
%% An attribute list before + ends the list. A second + is text.

plus_stops_before_delimiter(_) ->
	doc("A block after + stays when the paragraph reader stops. (15.1, 17.7)"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ * Two.">>, _},
		{listing_block, _, <<"code">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"* Two.\n"
		"----\n"
		"code\n"
		"----\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"+ * Two.">>, _},
		{admonition, #{style := <<"NOTE">>}, <<"Careful.">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"* Two.\n"
		"[NOTE]\n"
		"Careful.\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>},
			<<"+ Term::   definition">>, _},
		{listing_block, _, <<"code">>, _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"Term::\n"
		"  definition\n"
		"----\n"
		"code\n"
		"----\n"),
	[
		{paragraph, _, <<"+ * Two.">>, _},
		{listing_block, _, <<"code">>, _}
	] = parse(
		"+\n"
		"* Two.\n"
		"----\n"
		"code\n"
		"----\n"),
	%% The first + continues the item. The second + is text.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{paragraph, #{<<"title">> := <<"Title">>}, <<"+ text">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		".Title\n"
		"+\n"
		"text\n"),
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{paragraph, _, <<"+ text">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		"+\n"
		"text\n"),
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _},
			{paragraph, #{<<"title">> := <<"Title">>}, <<"+ more">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"
		"+\n"
		"more\n"),
	%% An attribute list before + is not a continuation.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>},
			<<"+\ncode">>, _}
	] = parse(
		"* One.\n"
		"\n"
		"[source,erlang]\n"
		"+\n"
		"code\n"),
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>},
			<<"+\ncode">>, _}
	] = parse(
		"* One.\n"
		"[source,erlang]\n"
		"+\n"
		"code\n"),
	%% The inline pass turns the newline after + into a space.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{admonition, #{style := <<"NOTE">>}, <<"+ Careful.">>, _}
	] = parse(
		"* One.\n"
		"\n"
		"[NOTE]\n"
		"+\n"
		"Careful.\n"),
	%% The parent still continues. The style applies to the code.
	[{list, _, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{list, _, [
				{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
			], _},
			{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>},
				<<"code">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		"[source,erlang]\n"
		"+\n"
		"code\n"),
	%% An indented + is a line break inside the same paragraph.
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, [
			<<"+  ">>,
			{line_break, _, <<>>, _},
			<<"more">>
		], _}
	] = parse(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"  +\n"
		"more\n"),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"  // not a comment\n"
		"text\n",
		<<"+   // not a comment text">>),
	titled_plus(
		"* One.\n"
		"\n"
		".Title\n"
		"+\n"
		"/// not comment\n"
		"text\n",
		<<"+ /// not comment text">>),
	%% A comment or an attribute entry uses up the + and the list ends.
	[
		{list, _, [
			{list_item, _, [
				{paragraph, _, <<"One.">>, _},
				{list, _, [
					{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
				], _}
			], _}
		], _},
		{comment_line, CommentAttrs, <<"comment">>, _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"Para.">>, _}
	] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"
		"// comment\n"
		"Para.\n"),
	false = maps:is_key(<<"title">>, CommentAttrs),
	[
		{list, _, [
			{list_item, _, [
				{paragraph, _, <<"One.">>, _},
				{list, _, [
					{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
				], _}
			], _}
		], _},
		{attribute_entry, EntryAttrs, <<"Loic">>, _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"Para.">>, _}
	] = parse(
		"* One.\n"
		"** Two.\n"
		"\n"
		".Title\n"
		"+\n"
		":author: Loic\n"
		"Para.\n"),
	false = maps:is_key(<<"title">>, EntryAttrs),
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

open_block_style(_) ->
	doc("Styles on an open block select the block type. (16.10)"),
	%% listing is verbatim text. Children are not parsed.
	[{listing_block, #{
		1 := <<"listing">>,
		<<"subs">> := <<"verbatim">>
	}, <<"Hello *bold*.\n\n* One.">>, _}] = parse(
		"[listing]\n"
		"--\n"
		"Hello *bold*.\n"
		"\n"
		"* One.\n"
		"--\n"),
	[{listing_block, _, <<"[source,erlang]\nf() -> ok.">>, _}] = parse(
		"[listing]\n"
		"--\n"
		"[source,erlang]\n"
		"f() -> ok.\n"
		"--\n"),
	%% Blank lines at either end are dropped. A space-only line is blank.
	%% Trailing spaces on a content line stay.
	[{listing_block, _, <<"Hi\n  x  ">>, _}] = parse(
		"[listing]\n"
		"--\n"
		" \n"
		"Hi\n"
		"  x  \n"
		"\n"
		"--\n"),
	[{literal_block, #{
		1 := <<"literal">>,
		<<"subs">> := <<"verbatim">>
	}, <<"  Hello *bold*.">>, _}] = parse(
		"[literal]\n"
		"--\n"
		"  Hello *bold*.\n"
		"--\n"),
	[{listing_block, #{
		1 := <<"source">>,
		2 := <<"erlang">>,
		<<"subs">> := <<"verbatim">>
	}, <<"f() -> *ok*.">>, #{line := 2}}] = parse(
		"[source,erlang]\n"
		"--\n"
		"f() -> *ok*.\n"
		"--\n"),
	[{listing_block, #{<<"title">> := <<"Code">>}, <<"f() -> ok.">>, _}] = parse(
		".Code\n"
		"[listing]\n"
		"--\n"
		"f() -> ok.\n"
		"--\n"),
	[{listing_block, #{<<"title">> := <<"Code">>}, <<"f() -> ok.">>, _}] = parse(
		"[listing]\n"
		".Code\n"
		"--\n"
		"f() -> ok.\n"
		"--\n"),
	%% NOTE keeps parsed children.
	[{admonition, #{style := <<"NOTE">>}, [
		{paragraph, _, [
			<<"Hello ">>,
			{strong, _, <<"bold">>, _},
			<<".">>
		], _},
		{list, #{type := bulleted}, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _}
	], _}] = parse(
		"[NOTE]\n"
		"--\n"
		"Hello *bold*.\n"
		"\n"
		"* One.\n"
		"--\n"),
	[{admonition, #{
		style := <<"WARNING">>,
		<<"title">> := <<"Watch">>
	}, [
		{paragraph, _, <<"Careful.">>, _}
	], _}] = parse(
		".Watch\n"
		"[WARNING]\n"
		"--\n"
		"Careful.\n"
		"--\n"),
	[{admonition, #{style := <<"NOTE">>}, [
		{listing_block, #{1 := <<"source">>, 2 := <<"erlang">>},
			<<"f() -> ok.">>, _}
	], _}] = parse(
		"[NOTE]\n"
		"--\n"
		"[source,erlang]\n"
		"f() -> ok.\n"
		"--\n"),
	%% A table inside the admonition is still built.
	[{admonition, #{style := <<"NOTE">>}, [
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"A">>, _}], _},
				{cell, _, [{paragraph, _, <<"B">>, _}], _}
			], _}
		], _}
	], _}] = parse(
		"[NOTE]\n"
		"--\n"
		"|===\n"
		"|A |B\n"
		"|===\n"
		"--\n"),
	%% Lower case is not an admonition style. raw does not leak.
	[{open_block, #{1 := <<"note">>}, [
		{paragraph, _, <<"Hello.">>, _}
	], Ann}] = parse(
		"[note]\n"
		"--\n"
		"Hello.\n"
		"--\n"),
	false = maps:is_key(raw, Ann),
	%% A listing after + stays in the item, as text.
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{listing_block, #{<<"subs">> := <<"verbatim">>}, <<"* Two.">>, _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		"[listing]\n"
		"--\n"
		"* Two.\n"
		"--\n"),
	[{list, #{type := bulleted}, [
		{list_item, _, [
			{paragraph, _, <<"One.">>, _},
			{admonition, #{style := <<"TIP">>}, [
				{list, #{type := bulleted}, [
					{list_item, _, [{paragraph, _, <<"Two.">>, _}], _}
				], _}
			], _}
		], _}
	], _}] = parse(
		"* One.\n"
		"+\n"
		"[TIP]\n"
		"--\n"
		"* Two.\n"
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

%% Not a test. Arity 2 so ct_helper:all/1 does not pick it up.
titled_plus(Input, Body) ->
	[
		{list, _, [
			{list_item, _, [{paragraph, _, <<"One.">>, _}], _}
		], _},
		{paragraph, #{<<"title">> := <<"Title">>}, Body, _}
	] = parse(Input).
