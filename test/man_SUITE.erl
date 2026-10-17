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

-module(man_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

%% @todo We should try to see if it accepts all valid AST.
%% @todo Test all options.

missing_name_section(_) ->
	AST = asciideck:parse("= missing_name(3)\n\n== Not name\n\nFail!"),
	{'EXIT', _} = (catch asciideck:to_manpage(AST)),
	ok.

callout_list(_) ->
	doc("Callout lists are numbered by position. (20)"),
	AST = asciideck:parse(
		"= callout(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"callout - test\n"
		"\n"
		"== Description\n"
		"\n"
		"> First.\n"
		"> Second.\n"
		"<9> Third.\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<".IP 1\nFirst.">>),
	{_, _} = binary:match(Man, <<".IP 2\nSecond.">>),
	{_, _} = binary:match(Man, <<".IP 3\nThird.">>),
	nomatch = binary:match(Man, <<".IP 9">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	ok.

attribute_entry(_) ->
	doc("Attribute entries are omitted from man page output. (28)"),
	AST = asciideck:parse(
		"= attr(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"attr - test\n"
		"\n"
		":author: Loic Hoguin\n"
		"\n"
		"== Description\n"
		"\n"
		"Hello.\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"Hello.">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	nomatch = binary:match(Man, <<"Loic Hoguin">>),
	ok.

admonition_paragraph(_) ->
	doc("Admonition paragraphs are rendered in man pages. (15.4)"),
	AST = asciideck:parse(
		"= admonition(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"admonition - test\n"
		"\n"
		"== Description\n"
		"\n"
		"NOTE: The static handler considers that the local filesystem "
			"is trusted.\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"\\fBNote\\fR">>),
	{_, _} = binary:match(Man, <<"The static handler considers that the local filesystem is trusted.">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	ok.

open_block_style(_) ->
	doc("Styled open blocks are rendered in man pages. (16.10)"),
	AST = asciideck:parse(
		"= open(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"open - test\n"
		"\n"
		"== Description\n"
		"\n"
		"[listing]\n"
		"--\n"
		"f() -> ok.\n"
		"--\n"
		"\n"
		"[NOTE]\n"
		"--\n"
		"Careful.\n"
		"\n"
		"* One.\n"
		"--\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"f() -> ok.">>),
	{_, _} = binary:match(Man, <<"\\fBNote\\fR">>),
	{_, _} = binary:match(Man, <<"Careful.">>),
	{_, _} = binary:match(Man, <<"One.">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	ok.

super_sub(_) ->
	doc("Superscripts and subscripts are rendered in man pages. (10.2)"),
	AST = asciideck:parse(
		"= super(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"super - test\n"
		"\n"
		"== Description\n"
		"\n"
		"H~2~O and x^10^.\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"H\\d2\\uO">>),
	{_, _} = binary:match(Man, <<"x\\u10\\d.">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	ok.

sidebar_block(_) ->
	doc("Sidebar blocks are rendered in man pages. (16.4)"),
	AST = asciideck:parse(
		"= sidebar(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"sidebar - test\n"
		"\n"
		"== Description\n"
		"\n"
		".An Example Sidebar\n"
		"****\n"
		"Outside the narrative.\n"
		"****\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"\\fBAn Example Sidebar\\fR">>),
	{_, _} = binary:match(Man, <<"Outside the narrative.">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	AST2 = asciideck:parse(
		"= sidebar(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"sidebar - test\n"
		"\n"
		"== Description\n"
		"\n"
		".C:\\Windows\n"
		"****\n"
		"body\n"
		"****\n"),
	Man2 = iolist_to_binary(asciideck:to_manpage(AST2)),
	{_, _} = binary:match(Man2, <<"\\fBC:\\\\Windows\\fR">>),
	ok.

quote_block(_) ->
	doc("Quote and verse blocks are rendered in man pages. (16.7)"),
	AST = asciideck:parse(
		"= quote(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"quote - test\n"
		"\n"
		"== Description\n"
		"\n"
		".My Quote\n"
		"[quote, C:\\Ann, The Book]\n"
		"____\n"
		"Words.\n"
		"____\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST)),
	{_, _} = binary:match(Man, <<"\\fBMy Quote\\fR">>),
	{_, _} = binary:match(Man, <<"Words.">>),
	{_, _} = binary:match(Man, <<"\\fIThe Book\\fR">>),
	{_, _} = binary:match(Man, <<"\\(em C:\\\\Ann">>),
	nomatch = binary:match(Man, <<"Ignored AST node">>),
	Verse = asciideck:parse(
		"= verse(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"verse - test\n"
		"\n"
		"== Description\n"
		"\n"
		"[verse]\n"
		"____\n"
		"To *see* a world\n"
		"____\n"),
	VerseMan = iolist_to_binary(asciideck:to_manpage(Verse)),
	{_, _} = binary:match(VerseMan, <<".nf\nTo \\fBsee\\fR a world\n.fi\n">>),
	nomatch = binary:match(VerseMan, <<"Ignored AST node">>),
	Named = asciideck:parse(
		"= named(3)\n"
		"\n"
		"== Name\n"
		"\n"
		"named - test\n"
		"\n"
		"== Description\n"
		"\n"
		"[style=\"verse\", attribution=\"Ann\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n"),
	NamedMan = iolist_to_binary(asciideck:to_manpage(Named)),
	{_, _} = binary:match(NamedMan, <<".nf\none\ntwo\n.fi\n">>),
	{_, _} = binary:match(NamedMan, <<"\\(em Ann">>),
	ok.
