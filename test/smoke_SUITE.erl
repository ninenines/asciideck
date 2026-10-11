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

%% Convert the documentation of projects that use Asciideck
%% and check that no errors are produced. The projects are
%% cloned by the smoke target in the Makefile. Test cases
%% are skipped when the projects are not available.
-module(smoke_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

%% Files known to fail, relative to the project directory.
expected_failures() -> #{
	%% include:: is not implemented.
	{"cowboy", "doc/src/guide/book.asciidoc"} => include,
	{"erlang.mk", "doc/src/guide/book.asciidoc"} => include,
	{"gun", "doc/src/guide/book.asciidoc"} => include
}.

cowboy_manual(_) ->
	doc("Cowboy manual pages."),
	check("cowboy", "doc/src/manual", man).

cowboy_guide(_) ->
	doc("Cowboy user guide."),
	check("cowboy", "doc/src/guide", html).

cowlib_manual(_) ->
	doc("Cowlib manual pages."),
	check("cowlib", "doc/src/manual", man).

erlang_mk_guide(_) ->
	doc("Erlang.mk user guide."),
	check("erlang.mk", "doc/src/guide", html).

gun_manual(_) ->
	doc("Gun manual pages."),
	check("gun", "doc/src/manual", man).

gun_guide(_) ->
	doc("Gun user guide."),
	check("gun", "doc/src/guide", html).

ranch_manual(_) ->
	doc("Ranch manual pages."),
	check("ranch", "doc/src/manual", man).

check(Project, SubDir, Format) ->
	ProjectDir = filename:join([os:getenv("ERLANG_MK_TMP", "."), "smoke", Project]),
	case filelib:is_dir(ProjectDir) of
		false ->
			{skip, {not_found, ProjectDir}};
		true ->
			Files = filelib:wildcard(filename:join(SubDir, "*.asciidoc"), ProjectDir),
			true = Files =/= [],
			Results = [{F, convert(filename:join(ProjectDir, F), Format)} || F <- Files],
			Expected = expected_failures(),
			Unexpected = [{F, R} || {F, R} <- Results, R =/= ok,
				not maps:is_key({Project, F}, Expected)],
			Fixed = [F || {F, ok} <- Results, maps:is_key({Project, F}, Expected)],
			case Fixed of
				[] -> ok;
				_ -> ct:comment("Expected failures now pass: ~p", [Fixed])
			end,
			case Unexpected of
				[] -> ok;
				_ -> ct:fail({unexpected_failures, Unexpected})
			end
	end.

convert(File, Format) ->
	try
		AST = asciideck:parse_file(File),
		HTML = iolist_to_binary(asciideck:to_html(AST, #{})),
		Man = case Format of
			man -> iolist_to_binary(asciideck:to_manpage(AST, #{}));
			html -> <<>>
		end,
		%% Both backends leave an asciideck-error marker
		%% for every node they skipped or failed to render.
		case {binary:match(HTML, <<"asciideck-error">>), binary:match(Man, <<"asciideck-error">>)} of
			{nomatch, nomatch} -> ok;
			_ -> {error_node, [L || L <- binary:split(Man, <<"\n">>, [global]),
				binary:match(L, <<"asciideck-error">>) =/= nomatch]}
		end
	catch Class:Reason ->
		{Class, Reason}
	end.
