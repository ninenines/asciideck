%% Copyright (c) 2026, Loïc Hoguin <essen@ninenines.eu>
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

-module(html_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

%% Not a test case: ct_helper:all/1 only picks up arity 1 functions.
html(Source, Opts) ->
	iolist_to_binary(asciideck:to_html(asciideck:parse(Source),
		Opts#{no_header_footer => true})).

table_no_pipe(_) ->
	doc("Table lines without cell separators are rendered as cells."),
	HTML = html("[format=csv]\n|===\na,b\nc,d\n|===\n", #{}),
	nomatch = binary:match(HTML, <<"asciideck-error">>),
	{_, _} = binary:match(HTML, <<"<th>a,b</th>">>),
	{_, _} = binary:match(HTML, <<"<td>c,d</td>">>),
	ok.
