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

-module(asciideck).

-export([parse_stdin/0]).
-export([parse_stdin/1]).
-export([parse_file/1]).
-export([parse_file/2]).
-export([parse/1]).
-export([parse/2]).

-export([to_html/1]).
-export([to_html/2]).
-export([to_manpage/1]).
-export([to_manpage/2]).

parse_stdin() ->
	parse_stdin(#{}).

parse_stdin(St) ->
	{ok, ReaderPid} = asciideck_stdin_reader:start_link(),
	parse(ReaderPid, St).

parse_file(Filename) ->
	parse_file(Filename, #{}).

parse_file(Filename, St) ->
	{ok, File} = file:read_file(Filename),
	parse(File, St#{infile => filename:absname(Filename)}).

parse(Data) ->
	parse(Data, #{}).

parse(Data, _St) ->
	Passes = [
		asciideck_attributes_pass,
		asciideck_lists_pass,
		asciideck_tables_pass,
		asciideck_inline_pass
	],
	lists:foldl(fun(M, AST) -> M:run(AST) end,
		asciideck_block_parser:parse(Data), Passes).

to_html(AST) ->
	asciideck_to_html:translate(AST, #{}).

to_html(AST, Opts) ->
	asciideck_to_html:translate(AST, Opts).

to_manpage(AST) ->
	asciideck_to_manpage:translate(AST, #{}).

to_manpage(AST, Opts) ->
	asciideck_to_manpage:translate(AST, Opts).
