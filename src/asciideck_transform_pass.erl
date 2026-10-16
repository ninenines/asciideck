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
run([{Type, Attrs=#{1 := <<"source">>}, Text, Ann}|Tail])
		when Type =:= paragraph; Type =:= literal_paragraph ->
	[{listing_block, Attrs#{<<"subs">> => <<"verbatim">>}, Text, Ann}|run(Tail)];
run([{paragraph, Attrs=#{1 := <<"literal">>}, Text, Ann}|Tail]) ->
	[{literal_paragraph, Attrs#{<<"subs">> => <<"verbatim">>}, Text, Ann}|run(Tail)];
%% [NOTE] (and other admonition styles) apply to the following paragraph
%% or implicit literal paragraph.
run([{Type, Attrs0=#{1 := Style}, Text, Ann}|Tail])
		when (Type =:= paragraph orelse Type =:= literal_paragraph),
			(Style =:= <<"NOTE">> orelse Style =:= <<"TIP">> orelse
				Style =:= <<"IMPORTANT">> orelse Style =:= <<"WARNING">> orelse
				Style =:= <<"CAUTION">>) ->
	%% Admonitions are not verbatim even if the body was indented.
	Attrs = maps:remove(<<"subs">>, Attrs0#{style => Style}),
	[{admonition, Attrs, Text, Ann}|run(Tail)];
run([{open_block, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[open_style(Attrs, Children, Ann)|run(Tail)];
run([{list, Attrs, Items, Ann}|Tail]) ->
	[{list, Attrs, run(Items), Ann}|run(Tail)];
run([{list_item, Attrs, Children, Ann}|Tail]) ->
	[{list_item, Attrs, run(Children), Ann}|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

%% User Guide 16.10. The first positional attribute is the style.
%% listing, literal, and source are verbatim text. Admonition
%% styles keep the parsed children. quote, example, and sidebar
%% stay open blocks.
open_style(#{1 := <<"listing">>}=Attrs, _, Ann) ->
	verbatim_open(listing_block, Attrs, Ann);
open_style(#{1 := <<"source">>}=Attrs, _, Ann) ->
	verbatim_open(listing_block, Attrs, Ann);
open_style(#{1 := <<"literal">>}=Attrs, _, Ann) ->
	verbatim_open(literal_block, Attrs, Ann);
open_style(Attrs0=#{1 := Style}, Children, Ann)
		when Style =:= <<"NOTE">> orelse Style =:= <<"TIP">> orelse
			Style =:= <<"IMPORTANT">> orelse Style =:= <<"WARNING">> orelse
			Style =:= <<"CAUTION">> ->
	%% Admonitions are not verbatim even if the body was indented.
	Attrs = maps:remove(<<"subs">>, Attrs0#{style => Style}),
	{admonition, Attrs, run(Children), maps:remove(raw, Ann)};
open_style(Attrs, Children, Ann) ->
	{open_block, Attrs, run(Children), maps:remove(raw, Ann)}.

verbatim_open(Type, Attrs, Ann) ->
	{Type, Attrs#{<<"subs">> => <<"verbatim">>},
		verbatim_text(maps:get(raw, Ann)), maps:remove(raw, Ann)}.

%% A blank line at either end is not part of the verbatim text.
%% A blank line between content lines is kept. Trailing spaces
%% on a content line are kept: the reader does not strip them.
verbatim_text(Raw) ->
	Lines = binary:split(Raw, <<"\n">>, [global]),
	join_lines(drop_trailing_blanks(drop_leading_blanks(Lines))).

drop_leading_blanks([Line|Rest]=Lines) ->
	case asciideck_block_parser:trim(Line) of
		<<>> -> drop_leading_blanks(Rest);
		_ -> Lines
	end;
drop_leading_blanks([]) ->
	[].

drop_trailing_blanks(Lines) ->
	lists:reverse(drop_leading_blanks(lists:reverse(Lines))).

join_lines([]) ->
	<<>>;
join_lines(Lines) ->
	iolist_to_binary(lists:join(<<"\n">>, Lines)).

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

literal_test() ->
	[{literal_paragraph, #{
		1 := <<"literal">>,
		<<"subs">> := <<"verbatim">>
	}, <<"Hello!">>, #{line := 2}}] = run([
		{paragraph, #{1 => <<"literal">>}, <<"Hello!">>, #{line => 2}}
	]),
	ok.

source_literal_paragraph_test() ->
	[{listing_block, #{
		1 := <<"source">>,
		2 := <<"erlang">>,
		<<"subs">> := <<"verbatim">>
	}, <<"f() -> ok.">>, #{line := 2}}] = run([
		{literal_paragraph, #{1 => <<"source">>, 2 => <<"erlang">>},
			<<"f() -> ok.">>, #{line => 2}}
	]),
	ok.

admonition_literal_paragraph_test() ->
	[{admonition, #{
		1 := <<"NOTE">>,
		style := <<"NOTE">>
	}, <<"Be careful.">>, #{line := 2}}] = run([
		{literal_paragraph, #{
			1 => <<"NOTE">>,
			<<"subs">> => <<"verbatim">>
		}, <<"Be careful.">>, #{line => 2}}
	]),
	ok.

open_block_style_test() ->
	[{listing_block, #{
		1 := <<"listing">>,
		<<"subs">> := <<"verbatim">>
	}, <<"Hello *bold*.\n\n* One.">>, #{line := 2}}] = run([
		{open_block, #{1 => <<"listing">>}, [
			{paragraph, #{}, <<"ignored">>, #{line => 3}}
		], #{line => 2, raw => <<"\nHello *bold*.\n\n* One.\n">>}}
	]),
	[{literal_block, #{
		<<"subs">> := <<"verbatim">>
	}, <<"  Hello *world*!">>, _}] = run([
		{open_block, #{1 => <<"literal">>}, [],
			#{line => 1, raw => <<"  Hello *world*!">>}}
	]),
	[{listing_block, #{
		1 := <<"source">>,
		2 := <<"erlang">>
	}, <<"f() -> ok.">>, Ann}] = run([
		{open_block, #{1 => <<"source">>, 2 => <<"erlang">>}, [],
			#{line => 1, raw => <<"f() -> ok.\n">>}}
	]),
	false = maps:is_key(raw, Ann),
	[{admonition, #{
		1 := <<"NOTE">>,
		style := <<"NOTE">>
	}, [
		{listing_block, #{<<"subs">> := <<"verbatim">>}, <<"f().">>, _}
	], #{line := 1}}] = run([
		{open_block, #{1 => <<"NOTE">>}, [
			{paragraph, #{1 => <<"source">>}, <<"f().">>, #{line => 2}}
		], #{line => 1, raw => <<"unused">>}}
	]),
	[{open_block, #{1 := <<"quote">>}, [
		{paragraph, #{}, <<"Hello.">>, _}
	], OpenAnn}] = run([
		{open_block, #{1 => <<"quote">>}, [
			{paragraph, #{}, <<"Hello.">>, #{line => 2}}
		], #{line => 1, raw => <<"Hello.">>}}
	]),
	false = maps:is_key(raw, OpenAnn),
	%% Trailing spaces stay. Blank lines at either end do not.
	<<"Hi\n  x  ">> = verbatim_text(<<"\n \nHi\n  x  \n\n">>),
	ok.
-endif.
