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
-export([callout_lines/1]).

-define(IS_ADMONITION(S), S =:= <<"NOTE">>; S =:= <<"TIP">>; S =:= <<"IMPORTANT">>;
	S =:= <<"CAUTION">>; S =:= <<"WARNING">>).

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
%% Admonition blocks and paragraphs:
%%
%% [NOTE]
%% ====
%% Content.
%% ====
run([{example_block, Attrs=#{1 := Style}, Content, Ann}|Tail])
		when ?IS_ADMONITION(Style) ->
	[{admonition, Attrs#{type => admonition_type(Style)}, Content, Ann}|run(Tail)];
run([{paragraph, Attrs=#{1 := Style}, Text, Ann}|Tail])
		when ?IS_ADMONITION(Style) ->
	[{admonition, Attrs#{type => admonition_type(Style), paragraph => true}, Text, Ann}|run(Tail)];
%% Quote and verse paragraphs and blocks. Verses keep their line breaks.
run([{paragraph, Attrs=#{1 := <<"quote">>}, Text, Ann}|Tail]) ->
	[{quote_block, Attrs, Text, Ann}|run(Tail)];
run([{Type, Attrs=#{1 := <<"verse">>}, Text, Ann}|Tail])
		when Type =:= paragraph; Type =:= quote_block ->
	[{verse_block, Attrs#{<<"subs">> => <<"verbatim">>}, Text, Ann}|run(Tail)];
%% Listing blocks may contain callout markers (<1>) at the end of lines.
run([Block0={listing_block, Attrs, Text, Ann}|Tail]) when is_binary(Text) ->
	Block = case [N || {_, Ns} <- callout_lines(Text), N <- Ns] of
		[] -> Block0;
		Callouts -> {listing_block, Attrs#{callouts => Callouts}, Text, Ann}
	end,
	[Block|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

%% Split the text into lines with their callout numbers removed.
-spec callout_lines(binary()) -> [{binary(), [pos_integer()]}].
callout_lines(Text) ->
	[callout_line(Line) || Line <- binary:split(Text, <<"\n">>, [global])].

callout_line(Line0) ->
	case re:run(Line0, "^(.*?)\\s*((?:<\\d{1,3}>\\s*)+)$", [{capture, all_but_first, binary}]) of
		{match, [Line, Markers]} ->
			{match, Ns} = re:run(Markers, "<(\\d+)>", [global, {capture, all_but_first, binary}]),
			{Line, [binary_to_integer(N) || [N] <- Ns]};
		nomatch ->
			{Line0, []}
	end.

admonition_type(<<"NOTE">>) -> note;
admonition_type(<<"TIP">>) -> tip;
admonition_type(<<"IMPORTANT">>) -> important;
admonition_type(<<"CAUTION">>) -> caution;
admonition_type(<<"WARNING">>) -> warning.

-ifdef(TEST).
admonition_test() ->
	[{admonition, #{type := note}, <<"Hello.">>, _}] = run([
		{example_block, #{1 => <<"NOTE">>}, <<"Hello.">>, #{line => 2}}]),
	[{admonition, #{type := tip, paragraph := true}, <<"Hello.">>, _}] = run([
		{paragraph, #{1 => <<"TIP">>}, <<"Hello.">>, #{line => 2}}]),
	[{example_block, #{1 := <<"note">>}, _, _}] = run([
		{example_block, #{1 => <<"note">>}, <<"Hello.">>, #{line => 2}}]),
	ok.

callout_test() ->
	[{listing_block, #{callouts := [1, 2, 3]}, _, _}] = run([
		{listing_block, #{}, <<"a() <1>\nb() <2> <3>\nc()">>, #{line => 1}}]),
	[{listing_block, Attrs, _, _}] = run([
		{listing_block, #{}, <<"a < b\n<<x>>">>, #{line => 1}}]),
	false = maps:is_key(callouts, Attrs),
	[{<<"a()">>, [1]}, {<<"b()">>, [2, 3]}, {<<"c()">>, []}]
		= callout_lines(<<"a() <1>\nb() <2> <3>\nc()">>),
	ok.

quote_verse_test() ->
	[{quote_block, #{2 := <<"Me">>}, <<"Hi.">>, _}] = run([
		{paragraph, #{1 => <<"quote">>, 2 => <<"Me">>}, <<"Hi.">>, #{line => 2}}]),
	[{verse_block, #{<<"subs">> := <<"verbatim">>}, <<"A\nB">>, _}] = run([
		{quote_block, #{1 => <<"verse">>}, <<"A\nB">>, #{line => 2}}]),
	[{verse_block, _, <<"A\nB">>, _}] = run([
		{paragraph, #{1 => <<"verse">>}, <<"A\nB">>, #{line => 2}}]),
	ok.
-endif.
