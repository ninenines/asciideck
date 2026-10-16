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

%% The purpose of this pass is to aggregate list_item
%% blocks into proper lists. This involves building a
%% tree based on the rules for list items.
%%
%% The general rules are:
%%
%% - Any list item of different type/level than the
%%   current list item is a child of the latter.
%%
%% - The level ultimately does not matter when building
%%   the tree, * then **** then ** is accepted just fine.
%%
%% - Lists of the same type as a parent are not allowed.
%%   On the other hand reusing a type in different parts
%%   of the tree is not a problem.
%%
%% - Any literal paragraph following a list item is a
%%   child of that list item, unless a title line precedes
%%   it. A title line ends the list before the next item,
%%   including a deeper item. Ancestor lists end with it.
%%   A continuation keeps a titled literal or a deeper list.
%%   A title line or an attribute list before + ends the
%%   list that sees the +. A parent list moves those
%%   attributes onto the next block and still uses the +.
%%   With no parent, the + is paragraph text. A second +
%%   is paragraph text as well.
%%
%% - Any other block can be included as a child by using
%%   list continuations.
-module(asciideck_lists_pass).

-export([run/1]).

run(AST) ->
	strip_block_title(list(AST, [])).

list([], Acc) ->
	lists:reverse(Acc);
%% A + left after the list is paragraph text. Attributes make
%% that true even when nothing follows. A second + is too.
list([{list_item_continuation, CAttrs, _, CAnn}|Tail], Acc)
		when map_size(CAttrs) > 0 orelse Tail =/= [] ->
	{Para, Rest} = plus_paragraph(CAttrs, CAnn, Tail),
	list(Rest, [Para|Acc]);
%% Any trailing block continuation is ignored.
list([{list_item_continuation, _, _, _}], Acc) ->
	lists:reverse(Acc);
%% The first list item contains the attributes for the list.
list([LI={list_item, Attrs, _, Ann}|Tail0], Acc) ->
	{Items, Tail} = item(Tail0, LI, [type(Attrs)], []),
	list(Tail, [{list, Attrs, Items, Ann}|Acc]);
list([{open_block, Attrs, Children, Ann}|Tail], Acc) ->
	list(Tail, [{open_block, Attrs, run(Children), Ann}|Acc]);
list([Block|Tail], Acc) ->
	list(Tail, [Block|Acc]).

%% List items of the same type (and level/separator, when present)
%% are siblings. Callout items have a type but no level.
item([NextLI={list_item, NextAttrs, _, Ann}|Tail],
		CurrentLI={list_item, Attrs, _, _}, Parents, Acc) ->
	Same = type(NextAttrs) =:= type(Attrs),
	%% A title line ends this list before the next item.
	%% The item is returned so ancestor lists end as well.
	%% A continuation keeps a deeper titled item before this.
	case maps:get(block_title, NextAttrs, false) of
		true ->
			{lists:reverse([reverse_children(CurrentLI)|Acc]), [NextLI|Tail]};
		false when Same ->
			item(Tail, NextLI, Parents, [reverse_children(CurrentLI)|Acc]);
		false ->
			case lists:member(type(NextAttrs), Parents) of
				%% We have a parent list item. This is the end of this child list.
				true ->
					{lists:reverse([reverse_children(CurrentLI)|Acc]), [NextLI|Tail]};
				%% We have a child list item. This is the beginning of a new list.
				false ->
					{Items, Rest0} = item(Tail, NextLI, [type(NextAttrs)|Parents], []),
					%% The child list ended at the title or the
					%% attribute list. This item can still continue.
					Rest = release_attributed_continuation(Rest0),
					item(Rest, add_child(CurrentLI, {list, NextAttrs, Items, Ann}), Parents, Acc)
			end
	end;
%% A title line or an attribute list directly before + ends
%% this list. Leave the + in place: a parent releases it,
%% otherwise it is text.
item([{list_item_continuation, CAttrs, _, _}=Cont|Tail], CurrentLI, _, Acc)
		when map_size(CAttrs) > 0 ->
	{lists:reverse([reverse_children(CurrentLI)|Acc]), [Cont|Tail]};
%% A + with nothing after it continues this item and adds nothing.
item([{list_item_continuation, _, _, _}], CurrentLI, _, Acc) ->
	{lists:reverse([reverse_children(CurrentLI)|Acc]), []};
%% The first + continued this item. The next + is the text
%% of a paragraph, titled or not.
item([{list_item_continuation, _, _, _},
		{list_item_continuation, CAttrs, _, CAnn}|Tail],
		CurrentLI, Parents, Acc) ->
	{Para, Rest} = plus_paragraph(CAttrs, CAnn, Tail),
	item(Rest, add_child(CurrentLI, Para), Parents, Acc);
%% Blocks that immediately follow list_item_continuation are children,
%% unless they are list_item themselves in which case it depends on the
%% type and level of the list item. A title line would end the list;
%% a continuation keeps a deeper item and drops that mark first.
item([{list_item_continuation, _, _, _},
		LI0={list_item, NextAttrs, Children, Ann}|Tail],
		CurrentLI={list_item, Attrs, _, _}, Parents, Acc) ->
	Deeper = type(NextAttrs) =/= type(Attrs)
		andalso not lists:member(type(NextAttrs), Parents),
	LI = case Deeper andalso maps:get(block_title, NextAttrs, false) of
		true ->
			{list_item, maps:remove(block_title, NextAttrs), Children, Ann};
		false ->
			LI0
	end,
	item([LI|Tail], CurrentLI, Parents, Acc);
item([{list_item_continuation, _, _, _}, Block|Tail], CurrentLI, Parents, Acc) ->
	item(Tail, add_child(CurrentLI, maybe_lists(Block)), Parents, Acc);
%% A literal paragraph immediately following a list item is a child.
%% User Guide 17.7: a title line ends the list. A title attribute
%% does not. The attributes pass sets block_title only for ".Title".
item([LP={literal_paragraph, #{block_title := true}, _, _}|Tail], CurrentLI, _, Acc) ->
	{lists:reverse([reverse_children(CurrentLI)|Acc]), [LP|Tail]};
item([LP={literal_paragraph, _, _, _}|Tail], CurrentLI, Parents, Acc) ->
	item(Tail, add_child(CurrentLI, LP), Parents, Acc);
%% Anything else is the end of the list.
item(Tail, CurrentLI, _, Acc) ->
	{lists:reverse([reverse_children(CurrentLI)|Acc]), Tail}.

type(Attrs) ->
	maps:with([type, level, separator], Attrs).

add_child({list_item, Attrs, Children, Ann}, Child) ->
	{list_item, Attrs, [Child|Children], Ann}.

maybe_lists({open_block, Attrs, Children, Ann}) ->
	{open_block, Attrs, run(Children), Ann};
maybe_lists(Block) ->
	Block.

%% The inner list consumed the title or the attribute list.
%% Move them onto the next block and leave a plain +.
%% A comment or an attribute entry uses up that +. The title
%% stays on the following block and the list does not continue.
release_attributed_continuation([{list_item_continuation, CAttrs, _, _},
		{Type, _, _, _}|_]=Rest)
		when map_size(CAttrs) > 0,
			(Type =:= comment_line orelse Type =:= attribute_entry) ->
	used_continuation(Rest);
release_attributed_continuation([{list_item_continuation, CAttrs, Body, CAnn},
		{Type, Attrs, Content, Ann}|Tail])
		when map_size(CAttrs) > 0 ->
	[{list_item_continuation, #{}, Body, CAnn},
		{Type, maps:merge(Attrs, CAttrs), Content, Ann}|Tail];
%% Nothing followed the +. The parent still consumes it.
release_attributed_continuation([{list_item_continuation, CAttrs, Body, CAnn}])
		when map_size(CAttrs) > 0 ->
	[{list_item_continuation, #{}, Body, CAnn}];
release_attributed_continuation(Rest) ->
	Rest.

used_continuation([{list_item_continuation, CAttrs, _, _}|Tail]) ->
	move_continuation_attrs(Tail, CAttrs).

move_continuation_attrs([Node={Type, _, _, _}|Tail], Attrs)
		when Type =:= comment_line; Type =:= attribute_entry ->
	[Node|move_continuation_attrs(Tail, Attrs)];
move_continuation_attrs([{Type, BlockAttrs, Content, Ann}|Tail], Attrs) ->
	[{Type, maps:merge(BlockAttrs, Attrs), Content, Ann}|Tail];
move_continuation_attrs([], _) ->
	[].

%% raw is the paragraph text captured at parse time, including
%% indents and list markers. Blocks on those lines are dropped.
plus_paragraph(CAttrs, CAnn, Tail) ->
	Attrs = maps:remove(block_title, CAttrs),
	Raw = maps:get(raw, CAnn),
	Last = maps:get(raw_last, CAnn),
	{{paragraph, Attrs, Raw, maps:remove(raw_last, maps:remove(raw, CAnn))},
		drop_raw_blocks(Tail, maps:get(line, CAnn), Last)}.

drop_raw_blocks([Block|Tail], Start, Last) ->
	case line_of_block(Block) of
		Line when is_integer(Line), Line > Start, Line =< Last ->
			drop_raw_blocks(Tail, Start, Last);
		_ ->
			[Block|Tail]
	end;
drop_raw_blocks([], _, _) ->
	[].

line_of_block({_, _, _, Ann}) when is_map(Ann) ->
	maps:get(line, Ann, undefined);
line_of_block(_) ->
	undefined.

reverse_children({list_item, Attrs, Children, Ann}) ->
	{list_item, Attrs, lists:reverse(Children), Ann}.

%% The block_title mark is only for this pass.
strip_block_title(Nodes) when is_list(Nodes) ->
	[strip_block_title(Node) || Node <- Nodes];
strip_block_title({Type, Attrs, Children, Ann}) when is_list(Children) ->
	{Type, maps:remove(block_title, Attrs),
		[strip_block_title(Child) || Child <- Children], Ann};
strip_block_title({Type, Attrs, Body, Ann}) when is_map(Attrs) ->
	{Type, maps:remove(block_title, Attrs), Body, Ann};
strip_block_title(Other) ->
	Other.

-ifdef(TEST).
list_test() ->
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"Hello!">>, _}], #{line := 1}},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"World!">>, _}], #{line := 2}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Hello!">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"World!">>, #{line => 2}}], #{line => 2}}
	]),
	ok.

list_of_list_test() ->
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"Hello!">>, _},
			{list, #{type := bulleted, level := 2}, [
				{list_item, #{type := bulleted, level := 2},
					[{paragraph, #{}, <<"Cat!">>, _}], #{line := 2}},
				{list_item, #{type := bulleted, level := 2},
					[{paragraph, #{}, <<"Dog!">>, _}], #{line := 3}}
			], #{line := 2}}
		], #{line := 1}},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"World!">>, _}], #{line := 4}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Hello!">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => bulleted, level => 2},
			[{paragraph, #{}, <<"Cat!">>, #{line => 2}}], #{line => 2}},
		{list_item, #{type => bulleted, level => 2},
			[{paragraph, #{}, <<"Dog!">>, #{line => 3}}], #{line => 3}},
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"World!">>, #{line => 4}}], #{line => 4}}
	]),
	ok.

list_continuation_test() ->
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"Hello!">>, _},
			{listing_block, #{}, <<"hello() -> world.">>, #{line := 3}}
		], #{line := 1}},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"World!">>, _}], #{line := 6}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Hello!">>, #{line => 1}}], #{line => 1}},
		{list_item_continuation, #{}, <<>>, #{line => 2}},
		{listing_block, #{}, <<"hello() -> world.">>, #{line => 3}},
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"World!">>, #{line => 6}}], #{line => 6}}
	]),
	ok.

callout_list_test() ->
	[{list, #{type := callout}, [
		{list_item, #{number := 1},
			[{paragraph, #{}, <<"First.">>, _}], #{line := 1}},
		{list_item, #{number := 2},
			[{paragraph, #{}, <<"Second.">>, _}], #{line := 2}}
	], #{line := 1}}] = run([
		{list_item, #{type => callout, number => 1},
			[{paragraph, #{}, <<"First.">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => callout, number => 2},
			[{paragraph, #{}, <<"Second.">>, #{line => 2}}], #{line => 2}}
	]),
	ok.

titled_literal_paragraph_test() ->
	%% A block title ends the list. The literal stays outside.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"Item.">>, _}], #{line := 1}}
	], #{line := 1}},
	{literal_paragraph, Outside, <<"foo()">>, #{line := 3}}
	] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Item.">>, #{line => 1}}], #{line => 1}},
		{literal_paragraph, #{<<"subs">> => <<"verbatim">>,
			<<"title">> => <<"Code">>, block_title => true},
			<<"foo()">>, #{line => 3}}
	]),
	#{<<"title">> := <<"Code">>} = Outside,
	false = maps:is_key(block_title, Outside),
	%% A title attribute is not a title line. The literal stays in the item.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"Item.">>, _},
			{literal_paragraph, #{<<"title">> := <<"Code">>},
				<<"foo()">>, #{line := 3}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Item.">>, #{line => 1}}], #{line => 1}},
		{literal_paragraph, #{<<"subs">> => <<"verbatim">>,
			<<"title">> => <<"Code">>}, <<"foo()">>, #{line => 3}}
	]),
	%% A continuation keeps the titled literal in the item.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"Item.">>, _},
			{literal_paragraph, #{<<"title">> := <<"Code">>},
				<<"foo()">>, #{line := 4}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"Item.">>, #{line => 1}}], #{line => 1}},
		{list_item_continuation, #{}, <<>>, #{line => 2}},
		{literal_paragraph, #{<<"subs">> => <<"verbatim">>,
			<<"title">> => <<"Code">>, block_title => true},
			<<"foo()">>, #{line => 4}}
	]),
	ok.

titled_list_item_test() ->
	%% A title line before a deeper item ends the list.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"One.">>, _}], #{line := 1}}
	], #{line := 1}},
	{list, Second, [
		{list_item, #{type := bulleted, level := 2, <<"title">> := <<"Title">>},
			[{paragraph, #{}, <<"Two.">>, _}], #{line := 3}}
	], #{line := 3}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"One.">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => bulleted, level => 2,
			<<"title">> => <<"Title">>, block_title => true},
			[{paragraph, #{}, <<"Two.">>, #{line => 3}}], #{line => 3}}
	]),
	#{<<"title">> := <<"Title">>, type := bulleted, level := 2} = Second,
	false = maps:is_key(block_title, Second),
	%% A continuation keeps that deeper titled list.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"One.">>, _},
			{list, #{type := bulleted, level := 2, <<"title">> := <<"Title">>}, [
				{list_item, ItemAttrs, [
					{paragraph, #{}, <<"Two.">>, _}
				], #{line := 4}}
			], #{line := 4}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"One.">>, #{line => 1}}], #{line => 1}},
		{list_item_continuation, #{}, <<>>, #{line => 2}},
		{list_item, #{type => bulleted, level => 2,
			<<"title">> => <<"Title">>, block_title => true},
			[{paragraph, #{}, <<"Two.">>, #{line => 4}}], #{line => 4}}
	]),
	#{<<"title">> := <<"Title">>} = ItemAttrs,
	false = maps:is_key(block_title, ItemAttrs),
	ok.

titled_continuation_test() ->
	%% No parent list: the + is the paragraph, and a following
	%% list marker stays in that paragraph.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"One.">>, _}], #{line := 1}}
	], #{line := 1}},
	{paragraph, ParaAttrs, <<"+\n** Two.">>, #{line := 4}}
	] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"One.">>, #{line => 1}}], #{line => 1}},
		{list_item_continuation, #{<<"title">> => <<"Title">>, block_title => true},
			<<>>, #{line => 4, raw => <<"+\n** Two.">>, raw_last => 5}},
		{list_item, #{type => bulleted, level => 2},
			[{paragraph, #{}, <<"Two.">>, #{line => 5}}], #{line => 5}}
	]),
	#{<<"title">> := <<"Title">>} = ParaAttrs,
	false = maps:is_key(block_title, ParaAttrs),
	%% Nothing followed the inner +. The parent consumes it.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"One.">>, _},
			{list, #{type := bulleted, level := 2}, [
				{list_item, #{type := bulleted, level := 2},
					[{paragraph, #{}, <<"Two.">>, _}], #{line := 2}}
			], #{line := 2}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"One.">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => bulleted, level => 2},
			[{paragraph, #{}, <<"Two.">>, #{line => 2}}], #{line => 2}},
		{list_item_continuation, #{<<"title">> => <<"Title">>, block_title => true},
			<<>>, #{line => 4, raw => <<"+">>, raw_last => 4}}
	]),
	%% A parent list takes the + and keeps the title on the paragraph.
	[{list, #{type := bulleted, level := 1}, [
		{list_item, #{type := bulleted, level := 1}, [
			{paragraph, #{}, <<"One.">>, _},
			{list, #{type := bulleted, level := 2}, [
				{list_item, #{type := bulleted, level := 2},
					[{paragraph, #{}, <<"Two.">>, _}], #{line := 2}}
			], #{line := 2}},
			{paragraph, #{<<"title">> := <<"Title">>}, <<"Para.">>, #{line := 5}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => bulleted, level => 1},
			[{paragraph, #{}, <<"One.">>, #{line => 1}}], #{line => 1}},
		{list_item, #{type => bulleted, level => 2},
			[{paragraph, #{}, <<"Two.">>, #{line => 2}}], #{line => 2}},
		{list_item_continuation, #{<<"title">> => <<"Title">>, block_title => true},
			<<>>, #{line => 4}},
		{paragraph, #{}, <<"Para.">>, #{line => 5}}
	]),
	ok.

literal_paragraph_child_test() ->
	[{list, #{type := labeled}, [
		{list_item, #{label := <<"Lorem">>}, [
			{paragraph, #{}, <<"Fusce euismod commodo velit.">>, _},
			{literal_paragraph, #{}, <<"Fusce euismod commodo velit.">>, #{line := 4}}
		], #{line := 1}}
	], #{line := 1}}] = run([
		{list_item, #{type => labeled, separator => <<"::">>, label => <<"Lorem">>},
			[{paragraph, #{}, <<"Fusce euismod commodo velit.">>, #{line => 1}}], #{line => 1}},
		{literal_paragraph, #{<<"subs">> => <<"verbatim">>},
			<<"Fusce euismod commodo velit.">>, #{line => 4}}
	]),
	ok.
-endif.
