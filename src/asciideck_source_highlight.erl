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

-spec filter(binary(), map()) -> {ok, binary()} | error.
filter(Input, #{2 := Lang}) ->
	filter(Input, Lang, os:find_executable("source-highlight"));
filter(_, _) ->
	error.

filter(_, _, false) ->
	error;
filter(Input, Lang, Exe) ->
	case is_valid_lang(Lang) of
		true -> run(Input, Lang, Exe);
		false -> error
	end.

%% The language is given by the document and passed as an
%% argument to source-highlight so we only accept a strict
%% set of characters.
is_valid_lang(<<>>) ->
	false;
is_valid_lang(Lang) when is_binary(Lang) ->
	lists:all(fun
		(C) when C >= $a, C =< $z -> true;
		(C) when C >= $A, C =< $Z -> true;
		(C) when C >= $0, C =< $9 -> true;
		(C) -> lists:member(C, "_+.-")
	end, binary_to_list(Lang));
is_valid_lang(_) ->
	false.

run(Input, Lang, Exe) ->
	case tmp_file(Input, 10) of
		{ok, TmpFile} ->
			try
				Port = open_port({spawn_executable, Exe}, [
					{args, ["-i", TmpFile, "-s", binary_to_list(Lang), "-o", "STDOUT"]},
					exit_status, binary, stream, use_stdio, in
				]),
				collect(Port, [])
			after
				_ = file:delete(TmpFile)
			end;
		error ->
			error
	end.

%% We open the file exclusively to avoid writing
%% through a pre-existing file or symlink, and make
%% it private before writing the document contents.
tmp_file(_, 0) ->
	error;
tmp_file(Input, Tries) ->
	Filename = filename:join(tmp_dir(), lists:flatten(io_lib:format(
		"asciideck-~s-~b", [os:getpid(), erlang:unique_integer([positive])]))),
	case file:open(Filename, [write, exclusive, binary, raw]) of
		{ok, IoDevice} ->
			Res = case file:change_mode(Filename, 8#600) of
				ok -> file:write(IoDevice, Input);
				Error -> Error
			end,
			_ = file:close(IoDevice),
			case Res of
				ok -> {ok, Filename};
				_ -> _ = file:delete(Filename), error
			end;
		{error, eexist} ->
			tmp_file(Input, Tries - 1);
		{error, _} ->
			error
	end.

tmp_dir() ->
	case os:getenv("TMPDIR") of
		false -> "/tmp";
		"" -> "/tmp";
		Dir -> Dir
	end.

collect(Port, Acc) ->
	receive
		{Port, {data, Data}} ->
			collect(Port, [Data|Acc]);
		{Port, {exit_status, 0}} ->
			{ok, iolist_to_binary(lists:reverse(Acc))};
		{Port, {exit_status, _}} ->
			error
	end.

-ifdef(TEST).
-include_lib("kernel/include/file.hrl").

is_valid_lang_test() ->
	true = is_valid_lang(<<"erlang">>),
	true = is_valid_lang(<<"c++">>),
	true = is_valid_lang(<<"shell-session_1.0">>),
	false = is_valid_lang(<<>>),
	false = is_valid_lang(<<"erlang;touch /tmp/x">>),
	false = is_valid_lang(<<"$(id)">>),
	false = is_valid_lang(<<"a b">>),
	false = is_valid_lang(<<"a\nb">>),
	ok.

filter_injection_test() ->
	Marker = filename:join(tmp_dir(), "asciideck-injection-"
		++ os:getpid() ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
	_ = file:delete(Marker),
	Lang = iolist_to_binary(["erlang;touch ", Marker]),
	error = filter(<<"ok.">>, #{2 => Lang}),
	%% Also when the executable is the shell itself.
	error = filter(<<"ok.">>, Lang, "/bin/sh"),
	false = filelib:is_file(Marker),
	ok.

filter_missing_executable_test() ->
	error = filter(<<"ok.">>, <<"erlang">>, false),
	ok.

filter_failing_executable_test() ->
	error = filter(<<"ok.">>, <<"erlang">>, "/bin/false"),
	ok.

tmp_file_mode_test() ->
	{ok, TmpFile} = tmp_file(<<"secret">>, 1),
	{ok, #file_info{mode=Mode}} = file:read_file_info(TmpFile),
	_ = file:delete(TmpFile),
	0 = Mode band 8#077,
	ok.

filter_no_lang_test() ->
	error = filter(<<"ok.">>, #{}),
	ok.
-endif.
