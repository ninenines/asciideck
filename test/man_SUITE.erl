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

table_cell_blocks(_) ->
	doc("Table cells may contain any block, including blocks "
		"that aren't a paragraph."),
	AST = asciideck:parse("= cells(3)\n\n== Name\n\ncells - Cells\n\n"
		"== Table\n\n|===\n| Type   | Value\n|===\n"),
	Man0 = iolist_to_binary(asciideck:to_manpage(AST, #{})),
	nomatch = binary:match(Man0, <<"asciideck-error">>),
	{_, _} = binary:match(Man0, <<"T{\nType\nT}:T{\nValue\nT}">>),
	Cell = fun(Blocks) -> {cell, #{specifiers => <<>>}, Blocks, #{}} end,
	Table = {table, #{}, [{row, #{}, [
		Cell([{literal_block, #{}, <<"literal">>, #{line => 1}}]),
		Cell([{paragraph, #{}, <<"one">>, #{line => 1}}, {paragraph, #{}, <<"two">>, #{line => 2}}]),
		Cell([])
	], #{}}], #{line => 1}},
	[Title, NameT, NameP, _|_] = AST,
	Man = iolist_to_binary(asciideck:to_manpage([Title, NameT, NameP, Table], #{})),
	nomatch = binary:match(Man, <<"asciideck-error">>),
	{_, _} = binary:match(Man, <<"literal">>),
	{_, _} = binary:match(Man, <<"two">>),
	HTML = iolist_to_binary(asciideck:to_html([Table], #{})),
	nomatch = binary:match(HTML, <<"asciideck-error">>),
	{_, _} = binary:match(HTML, <<"<pre>literal</pre>">>),
	ok.

skipped_node_marker(_) ->
	doc("Nodes that can't be rendered leave a marker in the output."),
	AST = asciideck:parse("= skip(3)\n\n== Name\n\nskip - Skip\n"),
	Man = iolist_to_binary(asciideck:to_manpage(AST ++ [{unknown_node, #{}, <<"x">>, #{line => 9}}], #{})),
	{_, _} = binary:match(Man, <<"\\# asciideck-error: ignored unknown_node node line 9\n">>),
	ok.
