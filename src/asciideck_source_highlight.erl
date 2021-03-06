%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

%% https://www.gnu.org/software/src-highlite/source-highlight.html
-module(asciideck_source_highlight).

-export([filter/2]).

filter(Input, #{2 := Lang}) ->
	TmpFile = "/tmp/asciideck-" ++ integer_to_list(erlang:phash2(make_ref())),
	ok = file:write_file(TmpFile, Input),
	Output = os:cmd(io_lib:format(
		"source-highlight -i ~s -s ~s",
		[TmpFile, Lang])),
	_ = file:delete(TmpFile),
	unicode:characters_to_binary(Output).
