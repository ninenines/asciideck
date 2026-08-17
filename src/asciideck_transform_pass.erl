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

%% The purpose of this pass is to transform elements based
%% on the style given in their attributes.
-module(asciideck_transform_pass).

-export([run/1]).

run([]) ->
	[];
%% The following syntax gets converted in the corresponding
%% listing_block element:
%%
%% [source,erlang]
%% f() -> ok.
%%
%% @todo We should not totally overwrite subs.
run([{paragraph, Attrs=#{1 := <<"source">>}, Text, Ann}|Tail]) ->
	[{listing_block, Attrs#{<<"subs">> => <<"verbatim">>}, Text, Ann}|run(Tail)];
%% [NOTE] (and other admonition styles) apply to the following paragraph.
run([{paragraph, Attrs=#{1 := Style}, Text, Ann}|Tail])
		when Style =:= <<"NOTE">>; Style =:= <<"TIP">>;
			Style =:= <<"IMPORTANT">>; Style =:= <<"WARNING">>;
			Style =:= <<"CAUTION">> ->
	[{admonition, Attrs#{style => Style}, Text, Ann}|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

-ifdef(TEST).
admonition_test() ->
	[{admonition, #{
		1 := <<"NOTE">>,
		style := <<"NOTE">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"NOTE">>}, <<"Hello!">>, #{line => 2}}
	]),
	[{admonition, #{
		1 := <<"TIP">>,
		style := <<"TIP">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"TIP">>}, <<"Hello!">>, #{line => 2}}
	]),
	[{admonition, #{
		1 := <<"IMPORTANT">>,
		style := <<"IMPORTANT">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"IMPORTANT">>}, <<"Hello!">>, #{line => 2}}
	]),
	[{admonition, #{
		1 := <<"WARNING">>,
		style := <<"WARNING">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"WARNING">>}, <<"Hello!">>, #{line => 2}}
	]),
	[{admonition, #{
		1 := <<"CAUTION">>,
		style := <<"CAUTION">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"CAUTION">>}, <<"Hello!">>, #{line => 2}}
	]),
	%% Unrelated styles are left untouched.
	[{paragraph, #{1 := <<"quote">>}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"quote">>}, <<"Hello!">>, #{line => 2}}
	]),
	ok.
-endif.
