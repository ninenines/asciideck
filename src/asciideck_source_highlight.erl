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
%%
%% The language comes from the document. It is passed as an
%% argument, never through a shell.
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

%% Letters, digits, and the characters source-highlight uses
%% in language names (c++, shell-session).
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
			try highlight(Exe, TmpFile, Lang) of
				Result -> Result
			catch _:_ ->
				error
			after
				_ = file:delete(TmpFile)
			end;
		error ->
			error
	end.

%% The port stays linked to a short-lived owner. A trap_exit
%% caller then does not see {'EXIT', Port, normal} when
%% source-highlight stops. Unlinking in the caller races
%% a child that exits before unlink.
highlight(Exe, TmpFile, Lang) ->
	Parent = self(),
	Pid = spawn(fun() ->
		Port = open_port({spawn_executable, Exe}, [
			{args, ["-i", TmpFile, "-s", binary_to_list(Lang), "-o", "STDOUT"]},
			exit_status, binary, stream, use_stdio, in
		]),
		Parent ! {self(), collect(Port, [])}
	end),
	Mon = monitor(process, Pid),
	receive
		{Pid, Result} ->
			demonitor(Mon, [flush]),
			Result;
		{'DOWN', Mon, process, Pid, _} ->
			error
	after 15000 ->
		exit(Pid, kill),
		demonitor(Mon, [flush]),
		error
	end.

%% Open exclusively so a pre-created symlink is not followed,
%% then restrict the mode before writing the listing.
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
				_ ->
					_ = file:delete(Filename),
					error
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
			error;
		{Port, closed} ->
			stop_port(Port),
			error
	after 10000 ->
		stop_port(Port),
		error
	end.

%% port_close only closes the pipes. A child that ignores
%% SIGHUP and SIGPIPE keeps running, so the timeout kills it.
stop_port(Port) ->
	case erlang:port_info(Port, os_pid) of
		{os_pid, OsPid} ->
			kill_os_pid(OsPid);
		_ ->
			ok
	end,
	try port_close(Port) catch _:_ -> ok end,
	flush_port(Port).

kill_os_pid(OsPid) ->
	case os:find_executable("kill") of
		false ->
			ok;
		Exe ->
			Port = open_port({spawn_executable, Exe}, [
				{args, ["-KILL", integer_to_list(OsPid)]},
				exit_status, stderr_to_stdout
			]),
			unlink(Port),
			_ = wait_exit(Port),
			ok
	end.

wait_exit(Port) ->
	receive
		{Port, {data, _}} ->
			wait_exit(Port);
		{Port, {exit_status, Status}} ->
			Status;
		{Port, closed} ->
			closed
	after 1000 ->
		try port_close(Port) catch _:_ -> ok end,
		timeout
	end.

flush_port(Port) ->
	receive
		{Port, _} -> flush_port(Port)
	after 0 ->
		ok
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
	error = filter(<<"ok.">>, Lang, "/bin/sh"),
	false = filelib:is_file(Marker),
	ok.

filter_missing_executable_test() ->
	error = filter(<<"ok.">>, <<"erlang">>, false),
	ok.

filter_failing_executable_test() ->
	error = filter(<<"ok.">>, <<"erlang">>, "/bin/false"),
	ok.

filter_no_lang_test() ->
	error = filter(<<"ok.">>, #{}),
	ok.

tmp_file_mode_test() ->
	{ok, TmpFile} = tmp_file(<<"secret">>, 1),
	{ok, #file_info{mode=Mode}} = file:read_file_info(TmpFile),
	_ = file:delete(TmpFile),
	0 = Mode band 8#077,
	ok.

filter_erlang_test() ->
	case os:find_executable("source-highlight") of
		false -> ok;
		_ ->
			{ok, HTML} = filter(<<"ok.">>, #{2 => <<"erlang">>}),
			{_, _} = binary:match(HTML, <<"ok">>),
			{_, _} = binary:match(HTML, <<"<pre>">>),
			{_, _} = binary:match(HTML, <<"<tt>">>),
			ok
	end.

%% A trap_exit caller must not see the port close.
%% A fresh process, so exits left by other tests are not ours.
filter_no_exit_signal_test() ->
	case os:find_executable("source-highlight") of
		false ->
			ok;
		_ ->
			Parent = self(),
			Pid = spawn(fun() ->
				process_flag(trap_exit, true),
				Ok = filter(<<"ok.">>, #{2 => <<"erlang">>}),
				Err = filter(<<"ok.">>, #{2 => <<"nope">>}),
				Exit = receive
					{'EXIT', _, Reason} -> {exit, Reason}
				after 100 -> none
				end,
				Parent ! {self(), Ok, Err, Exit}
			end),
			receive
				{Pid, {ok, _}, error, none} -> ok
			after 5000 ->
				error(timeout)
			end
	end.

%% port_close does not stop a child that ignores SIGHUP and SIGPIPE.
stop_port_kills_test() ->
	case os:find_executable("python3") of
		false -> ok;
		Python -> pause_then_dead(Python)
	end.

pause_then_dead(Python) ->
	Path = filename:join(tmp_dir(), "asciideck-pause-" ++ os:getpid() ++ ".py"),
	ok = file:write_file(Path, <<
		"import signal, time\n"
		"signal.signal(signal.SIGHUP, signal.SIG_IGN)\n"
		"signal.signal(signal.SIGPIPE, signal.SIG_IGN)\n"
		"signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
		"time.sleep(30)\n"
	>>),
	Port = open_port({spawn_executable, Python}, [
		{args, [Path]}, exit_status]),
	unlink(Port),
	{os_pid, OsPid} = erlang:port_info(Port, os_pid),
	stop_port(Port),
	false = wait_dead(OsPid, 20),
	_ = file:delete(Path),
	ok.

wait_dead(_, 0) ->
	true;
wait_dead(OsPid, N) ->
	case os_pid_alive(OsPid) of
		false -> false;
		true ->
			timer:sleep(50),
			wait_dead(OsPid, N - 1)
	end.

os_pid_alive(OsPid) ->
	Exe = os:find_executable("kill"),
	Port = open_port({spawn_executable, Exe}, [
		{args, ["-0", integer_to_list(OsPid)]},
		exit_status, stderr_to_stdout
	]),
	unlink(Port),
	wait_exit(Port) =:= 0.
-endif.
