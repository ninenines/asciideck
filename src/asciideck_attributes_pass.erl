%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

%% The purpose of this pass is to apply attributes to
%% their corresponding blocks. For macros the attributes
%% are already applied. For inline elements the inline
%% pass is taking care of it.
-module(asciideck_attributes_pass).

-export([run/1]).

run([]) ->
	[];
%% A block identifier is an alternative way of specifying
%% the id attribute for a block.
run([{block_id, #{id := ID}, <<>>, _}|Tail0]) ->
	Tail = apply_attributes(Tail0, #{<<"id">> => ID}),
	run(Tail);
%% A block title is ultimately treated as an attribute
%% for the following block.
%% block_title marks a title line (".Title"), as opposed to
%% a title attribute. The lists pass uses the mark, then
%% removes it. A continuation keeps the mark: the title
%% came before the +, so it is not dropped.
run([{block_title, _, Title, _}|Tail]) ->
	run(apply_block_title(Tail, #{<<"title">> => Title, block_title => true}));
run([{attribute_list, Attrs, <<>>, _}|Tail0]) ->
	Tail = apply_attributes(Tail0, Attrs),
	run(Tail);
run([{open_block, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[{open_block, Attrs, run(Children), Ann}|run(Tail)];
%% Metadata with no following block belongs to the next
%% block after the sidebar, not to the sidebar.
run([{sidebar_block, Attrs, Children0, Ann}|Tail]) when is_list(Children0) ->
	{Children, Pending} = trailing_block_meta(Children0),
	[{sidebar_block, Attrs, run(Children), Ann}|run(Pending ++ Tail)];
%% Verse is lines of text. A title or attribute list in the
%% body is part of those lines, not metadata for the next block.
%% Metadata with no following block belongs to the next block
%% after a quote, not to the quote.
run([{quote_block, Attrs, Children0, Ann}|Tail]) when is_list(Children0) ->
	case quote_style(Attrs) of
		<<"verse">> ->
			[{quote_block, Attrs, Children0, Ann}|run(Tail)];
		_ ->
			{Children, Pending} = trailing_block_meta(Children0),
			[{quote_block, Attrs, run(Children), Ann}|run(Pending ++ Tail)]
	end;
run([Block|Tail]) ->
	[Block|run(Tail)].

%% A continuation is not the titled block. Keep the mark on
%% it so the lists pass can tell the title from a later +.
apply_block_title([], _) ->
	[];
apply_block_title([{list_item_continuation, Attrs, Body, Ann}|Tail], TitleAttrs) ->
	[{list_item_continuation, maps:merge(Attrs, TitleAttrs), Body, Ann}|Tail];
apply_block_title(AST=[Element={Type, _, _, _}|Tail], TitleAttrs) ->
	case can_apply(Type) of
		drop ->
			AST;
		skip ->
			[Element|apply_block_title(Tail, TitleAttrs)];
		apply ->
			apply_attributes(AST, TitleAttrs)
	end.

%% Find the next block to apply the attributes.
apply_attributes([], _) ->
	[];
apply_attributes(AST=[Element0={Type, Attrs0, Content, Ann}|Tail], Attrs) ->
	case can_apply(Type) of
		drop ->
			AST;
		skip ->
			[Element0|apply_attributes(Tail, Attrs)];
		apply ->
			Merged = case Type of
				quote_block ->
					merge_quote_attrs(Attrs0, Attrs);
				_ ->
					maps:merge(Attrs0, Attrs)
			end,
			Element = {Type, Merged, Content, Ann},
			[Element|Tail]
	end.

%% Block macros already come with a mandatory attribute list.
%% Just to play it safe we drop the attributes for now.
can_apply(block_macro) -> drop;
%% A continuation carries attributes that were written before
%% the +. The lists pass moves them onto the continued block,
%% or onto the paragraph when the + is text.
can_apply(list_item_continuation) -> apply;
%% We skip attribute lists and alike and let it sort itself out.
can_apply(block_id) -> skip;
can_apply(attribute_list) -> skip;
can_apply(block_title) -> skip;
can_apply(attribute_entry) -> skip;
%% A comment does not take the preceding attribute list or title.
can_apply(comment_line) -> skip;
can_apply(comment_block) -> skip;
%% Everything else is a block.
can_apply(_) -> apply.

%% An empty positional value is unset. In that same list it hides
%% the named attribute for the position. It does not clear a value
%% written in another list. A non-empty positional still wins.
merge_quote_attrs(Earlier, Closer) ->
	maps:merge(prepare_quote_attrs(Earlier), prepare_quote_attrs(Closer)).

prepare_quote_attrs(Attrs) ->
	Attrs1 = suppress_named(Attrs, 1, <<"style">>),
	Attrs2 = suppress_named(Attrs1, 2, <<"attribution">>),
	Attrs3 = suppress_named(Attrs2, 3, <<"citetitle">>),
	drop_empty_pos(Attrs3).

suppress_named(Attrs, Pos, Name) ->
	case maps:get(Pos, Attrs, undefined) of
		<<>> -> maps:remove(Name, Attrs);
		_ -> Attrs
	end.

drop_empty_pos(Attrs) ->
	maps:filter(fun
		(Key, <<>>) when is_integer(Key), Key > 0 -> false;
		(_, _) -> true
	end, Attrs).

%% Positional attribute 1 is the style when that key was written.
%% Otherwise the named style attribute is. The default is quote.
quote_style(Attrs) ->
	case maps:is_key(1, Attrs) of
		true ->
			maps:get(1, Attrs);
		false ->
			maps:get(<<"style">>, Attrs, <<"quote">>)
	end.

%% A title, id, or attribute list at the end of a sidebar
%% has the closer as its next line. It applies outside.
trailing_block_meta(Children) ->
	{Meta, RevBody} = take_block_meta(lists:reverse(Children), []),
	{lists:reverse(RevBody), Meta}.

take_block_meta([Node|Rest], Acc) ->
	case block_meta(Node) of
		true ->
			take_block_meta(Rest, [Node|Acc]);
		false ->
			{Acc, [Node|Rest]}
	end;
take_block_meta([], Acc) ->
	{Acc, []}.

block_meta({block_id, _, _, _}) -> true;
block_meta({attribute_list, _, _, _}) -> true;
block_meta({block_title, _, _, _}) -> true;
%% An attribute entry is skipped while applying a title,
%% the same as at the top level. It has to leave with
%% the title, or the title is dropped inside the sidebar.
block_meta({attribute_entry, _, _, _}) -> true;
block_meta(_) -> false.

-ifdef(TEST).
attribute_list_test() ->
	AST0 = [
		{attribute_list, #{
			0 => <<"width=400">>,
			<<"width">> => <<"400">>
		}, <<>>, #{line => 1}},
		{listing_block, #{}, <<"Hello!">>, #{line => 2}}
	],
	AST = [
		{listing_block, #{
			0 => <<"width=400">>,
			<<"width">> => <<"400">>
		}, <<"Hello!">>, #{line => 2}}
	],
	AST = run(AST0),
	ok.

block_id_test() ->
	AST0 = [
		{block_id, #{
			id => <<"cowboy_req">>
		}, <<>>, #{line => 1}},
		{listing_block, #{}, <<"Hello!">>, #{line => 2}}
	],
	AST = [
		{listing_block, #{
			<<"id">> => <<"cowboy_req">>
		}, <<"Hello!">>, #{line => 2}}
	],
	AST = run(AST0),
	ok.

block_title_test() ->
	AST0 = [
		{block_title, #{}, <<"Title">>, #{line => 1}},
		{listing_block, #{}, <<"Hello!">>, #{line => 2}}
	],
	AST = [
		{listing_block, #{
			<<"title">> => <<"Title">>,
			block_title => true
		}, <<"Hello!">>, #{line => 2}}
	],
	AST = run(AST0),
	ok.

style_continuation_test() ->
	Source = #{1 => <<"source">>, 2 => <<"erlang">>},
	TitleFirst = [
		{block_title, #{}, <<"Title">>, #{line => 1}},
		{attribute_list, Source, <<>>, #{line => 2}},
		{list_item_continuation, #{}, <<>>, #{line => 3}},
		{paragraph, #{}, <<"code">>, #{line => 4}}
	],
	StyleFirst = [
		{attribute_list, Source, <<>>, #{line => 1}},
		{block_title, #{}, <<"Title">>, #{line => 2}},
		{list_item_continuation, #{}, <<>>, #{line => 3}},
		{paragraph, #{}, <<"code">>, #{line => 4}}
	],
	[
		{list_item_continuation, #{
			1 := <<"source">>,
			2 := <<"erlang">>,
			<<"title">> := <<"Title">>,
			block_title := true
		}, <<>>, _},
		{paragraph, #{}, <<"code">>, _}
	] = run(TitleFirst),
	[
		{list_item_continuation, #{
			1 := <<"source">>,
			2 := <<"erlang">>,
			<<"title">> := <<"Title">>,
			block_title := true
		}, <<>>, _},
		{paragraph, #{}, <<"code">>, _}
	] = run(StyleFirst),
	ok.

block_title_continuation_test() ->
	AST0 = [
		{block_title, #{}, <<"Title">>, #{line => 1}},
		{attribute_entry, #{name => <<"author">>, unset => false},
			<<"Loic">>, #{line => 2}},
		{list_item_continuation, #{}, <<>>, #{line => 3}},
		{paragraph, #{}, <<"Second.">>, #{line => 4}}
	],
	[
		{attribute_entry, #{name := <<"author">>}, <<"Loic">>, _},
		{list_item_continuation, #{
			<<"title">> := <<"Title">>,
			block_title := true
		}, <<>>, _},
		{paragraph, #{}, <<"Second.">>, _}
	] = run(AST0),
	ok.

attribute_entry_skip_test() ->
	AST0 = [
		{block_title, #{}, <<"Title">>, #{line => 1}},
		{attribute_entry, #{name => <<"author">>, unset => false},
			<<"Loic">>, #{line => 2}},
		{paragraph, #{}, <<"Hello!">>, #{line => 3}}
	],
	AST = [
		{attribute_entry, #{name => <<"author">>, unset => false},
			<<"Loic">>, #{line => 2}},
		{paragraph, #{<<"title">> => <<"Title">>, block_title => true},
			<<"Hello!">>, #{line => 3}}
	],
	AST = run(AST0),
	ok.

sidebar_block_test() ->
	[{sidebar_block, _, [
		{paragraph, #{<<"title">> := <<"Title">>}, <<"Hello!">>, _}
	], _}] = run([
		{sidebar_block, #{}, [
			{block_title, #{}, <<"Title">>, #{line => 2}},
			{paragraph, #{}, <<"Hello!">>, #{line => 3}}
		], #{line => 1}}
	]),
	[{sidebar_block, _, [
		{paragraph, _, <<"hello">>, _}
	], _},
		{paragraph, #{
			<<"title">> := <<"Title">>,
			1 := <<"NOTE">>,
			<<"role">> := <<"x">>
		}, <<"paragraph">>, _}] = run(asciideck_block_parser:parse(<<
		"****\n"
		"hello\n"
		"\n"
		"[NOTE]\n"
		"[role=\"x\"]\n"
		".Title\n"
		"****\n"
		"paragraph\n">>)),
	[{sidebar_block, _, [], _},
		{paragraph, #{<<"id">> := <<"bid">>}, <<"paragraph">>, _}]
		= run(asciideck_block_parser:parse(<<
		"****\n"
		"[[bid]]\n"
		"****\n"
		"paragraph\n">>)),
	%% An attribute entry does not swallow the title or the style.
	[{sidebar_block, _, [{paragraph, _, <<"hello">>, _}], _},
		{attribute_entry, #{name := <<"foo">>}, <<"bar">>, _},
		{paragraph, #{<<"title">> := <<"Title">>}, <<"after">>, _}]
		= run(asciideck_block_parser:parse(<<
		"****\n"
		"hello\n"
		"\n"
		".Title\n"
		":foo: bar\n"
		"****\n"
		"after\n">>)),
	[{sidebar_block, _, [{paragraph, _, <<"hello">>, _}], _},
		{attribute_entry, #{name := <<"foo">>}, <<"bar">>, _},
		{paragraph, #{1 := <<"NOTE">>}, <<"after">>, _}]
		= run(asciideck_block_parser:parse(<<
		"****\n"
		"hello\n"
		"\n"
		"[NOTE]\n"
		":foo: bar\n"
		"****\n"
		"after\n">>)),
	[{sidebar_block, _, [{paragraph, _, <<"hello">>, _}], _},
		{attribute_entry, #{name := <<"foo">>}, <<"bar">>, _},
		{paragraph, #{
			<<"title">> := <<"Title">>,
			<<"role">> := <<"x">>
		}, <<"after">>, _}] = run(asciideck_block_parser:parse(<<
		"****\n"
		"hello\n"
		"\n"
		".Title\n"
		":foo: bar\n"
		"[role=\"x\"]\n"
		"****\n"
		"after\n">>)),
	ok.

quote_block_test() ->
	[{quote_block, #{
		1 := <<"quote">>,
		2 := <<"Ann">>,
		3 := <<"Book">>
	}, [{paragraph, _, <<"Words.">>, _}], _}] = run(asciideck_block_parser:parse(
		"[quote, Ann, Book]\n"
		"____\n"
		"Words.\n"
		"____\n")),
	[{quote_block, _, [{paragraph, _, <<"hello">>, _}], _},
		{attribute_entry, #{name := <<"foo">>}, <<"bar">>, _},
		{paragraph, #{
			<<"title">> := <<"Title">>,
			<<"role">> := <<"x">>
		}, <<"after">>, _}] = run(asciideck_block_parser:parse(
		"____\n"
		"hello\n"
		"\n"
		".Title\n"
		":foo: bar\n"
		"[role=\"x\"]\n"
		"____\n"
		"after\n")),
	%% A title inside verse stays in the verse text. It does not title "after".
	[{quote_block, #{1 := <<"verse">>}, [], #{raw := <<"line\n\n.Title">>}},
		{paragraph, #{}, <<"after">>, _}] = run(asciideck_block_parser:parse(
		"[verse]\n"
		"____\n"
		"line\n"
		"\n"
		".Title\n"
		"____\n"
		"after\n")),
	%% The closest list sets the style. An earlier list keeps attribution.
	[{quote_block, #{
		1 := <<"verse">>,
		2 := <<"Ann">>,
		3 := <<"Book">>
	}, [], _}] = run(asciideck_block_parser:parse(
		"[quote, Ann, Book]\n"
		"[verse]\n"
		"____\n"
		"line\n"
		"____\n")),
	[{quote_block, #{
		1 := <<"quote">>,
		2 := <<"Ann">>,
		3 := <<"Book">>
	}, [{paragraph, _, <<"line">>, _}], _}] = run(asciideck_block_parser:parse(
		"[verse]\n"
		"[quote, Ann, Book]\n"
		"____\n"
		"line\n"
		"____\n")),
	%% A comment does not take the attribute list or the title.
	[{comment_line, _, <<"comment">>, _},
		{quote_block, #{1 := <<"verse">>}, [], #{raw := <<"one\ntwo">>}}]
		= run(asciideck_block_parser:parse(
		"[verse]\n"
		"// comment\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{comment_line, _, <<"c">>, _},
		{quote_block, #{
			1 := <<"quote">>,
			2 := <<"Ann">>,
			3 := <<"Book">>
		}, [{paragraph, _, <<"Words.">>, _}], _}]
		= run(asciideck_block_parser:parse(
		"[quote, Ann, Book]\n"
		"// c\n"
		"____\n"
		"Words.\n"
		"____\n")),
	[{comment_block, _, <<"hidden">>, _},
		{quote_block, #{1 := <<"verse">>}, [], #{raw := <<"one\ntwo">>}}]
		= run(asciideck_block_parser:parse(
		"[verse]\n"
		"////\n"
		"hidden\n"
		"////\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{comment_line, _, <<"c">>, _},
		{quote_block, #{
			1 := <<"verse">>,
			<<"title">> := <<"Title">>
		}, [], _}] = run(asciideck_block_parser:parse(
		".Title\n"
		"// c\n"
		"[verse]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{comment_line, _, <<"comment">>, _},
		{paragraph, #{1 := <<"NOTE">>}, <<"Paragraph.">>, _}]
		= run(asciideck_block_parser:parse(
		"[NOTE]\n"
		"// comment\n"
		"Paragraph.\n")),
	%% Named style is the style when positional 1 is absent.
	[{quote_block, #{
		<<"style">> := <<"verse">>,
		<<"attribution">> := <<"Ann">>
	}, [], _}] = run(asciideck_block_parser:parse(
		"[style=\"verse\", attribution=\"Ann\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{quote_block, #{<<"style">> := <<"verse">>}, [], _}]
		= run(asciideck_block_parser:parse(
		"[style=\"quote\"]\n"
		"[style=\"verse\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	%% A positional style wins over a later named style.
	[{quote_block, #{
		1 := <<"quote">>,
		2 := <<"Ann">>,
		3 := <<"Book">>,
		<<"style">> := <<"verse">>
	}, [{paragraph, _, <<"one\ntwo">>, _}], _}]
		= run(asciideck_block_parser:parse(
		"[quote, Ann, Book]\n"
		"[style=\"verse\"]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	%% An empty positional value does not clear another list.
	[{quote_block, #{1 := <<"verse">>, 2 := <<"Ann">>}, [], _}]
		= run(asciideck_block_parser:parse(
		"[verse]\n"
		"[, Ann]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{quote_block, #{<<"style">> := <<"verse">>, 2 := <<"Ann">>}, [], _}]
		= run(asciideck_block_parser:parse(
		"[style=\"verse\"]\n"
		"[, Ann]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	[{quote_block, BookAnn=#{3 := <<"Book">>, <<"attribution">> := <<"Ann">>},
		[{paragraph, _, <<"Words.">>, _}], _}]
		= run(asciideck_block_parser:parse(
		"[quote, , Book]\n"
		"[attribution=\"Ann\"]\n"
		"____\n"
		"Words.\n"
		"____\n")),
	false = maps:is_key(2, BookAnn),
	[{quote_block, NamedPos=#{<<"attribution">> := <<"Ann">>, 3 := <<"Book">>},
		[{paragraph, _, <<"Words.">>, _}], _}]
		= run(asciideck_block_parser:parse(
		"[attribution=\"Ann\", citetitle=\"Work\"]\n"
		"[quote, , Book]\n"
		"____\n"
		"Words.\n"
		"____\n")),
	false = maps:is_key(2, NamedPos),
	[{quote_block, #{2 := <<"Ann">>, <<"citetitle">> := <<"Book">>},
		[{paragraph, _, <<"Words.">>, _}], _}]
		= run(asciideck_block_parser:parse(
		"[quote, Ann,]\n"
		"[citetitle=\"Book\"]\n"
		"____\n"
		"Words.\n"
		"____\n")),
	[{quote_block, #{1 := <<"verse">>, 2 := <<"Other">>, 3 := <<"Work">>},
		[], _}] = run(asciideck_block_parser:parse(
		"[quote, Ann, Book]\n"
		"[verse, Other, Work]\n"
		"____\n"
		"one\n"
		"two\n"
		"____\n")),
	ok.
-endif.
