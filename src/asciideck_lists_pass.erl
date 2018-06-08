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
%%   child of that list item. @todo
%%
%% - Any other block can be included as a child by using
%%   list continuations.
-module(asciideck_lists_pass).

-export([run/1]).

run(AST) ->
	list(AST, []).

list([], Acc) ->
	lists:reverse(Acc);
%% Any trailing block continuation is ignored.
list([{list_item_continuation, _, _, _}], Acc) ->
	lists:reverse(Acc);
%% The first list item contains the attributes for the list.
list([LI={list_item, Attrs, _, Ann}|Tail0], Acc) ->
	{Items, Tail} = item(Tail0, LI, [type(Attrs)], []),
	list(Tail, [{list, Attrs, Items, Ann}|Acc]);
list([Block|Tail], Acc) ->
	list(Tail, [Block|Acc]).

%% Bulleted/numbered list item of the same type.
item([NextLI={list_item, #{type := T, level := L}, _, _}|Tail],
		CurrentLI={list_item, #{type := T, level := L}, _, _}, Parents, Acc) ->
	item(Tail, NextLI, Parents, [reverse_children(CurrentLI)|Acc]);
%% Labeled list item of the same type.
item([NextLI={list_item, #{type := T, separator := S}, _, _}|Tail],
		CurrentLI={list_item, #{type := T, separator := S}, _, _}, Parents, Acc) ->
	item(Tail, NextLI, Parents, [reverse_children(CurrentLI)|Acc]);
%% Other list items are either parent or children lists.
item(FullTail=[NextLI={list_item, Attrs, _, Ann}|Tail0], CurrentLI, Parents, Acc) ->
	case lists:member(type(Attrs), Parents) of
		%% We have a parent list item. This is the end of this child list.
		true ->
			{lists:reverse([reverse_children(CurrentLI)|Acc]), FullTail};
		%% We have a child list item. This is the beginning of a new list.
		false ->
			{Items, Tail} = item(Tail0, NextLI, [type(Attrs)|Parents], []),
			item(Tail, add_child(CurrentLI, {list, Attrs, Items, Ann}), Parents, Acc)
	end;
%% Ignore multiple contiguous list continuations.
item([LIC={list_item_continuation, _, _, _},
		{list_item_continuation, _, _, _}|Tail], CurrentLI, Parents, Acc) ->
	item([LIC|Tail], CurrentLI, Parents, Acc);
%% Blocks that immediately follow list_item_continuation are children,
%% unless they are list_item themselves in which case it depends on the
%% type and level of the list item.
item([{list_item_continuation, _, _, _}, LI={list_item, _, _, _}|Tail], CurrentLI, Parents, Acc) ->
	item([LI|Tail], CurrentLI, Parents, Acc);
item([{list_item_continuation, _, _, _}, Block|Tail], CurrentLI, Parents, Acc) ->
	item(Tail, add_child(CurrentLI, Block), Parents, Acc);
%% Anything else is the end of the list.
item(Tail, CurrentLI, _, Acc) ->
	{lists:reverse([reverse_children(CurrentLI)|Acc]), Tail}.

type(Attrs) ->
	maps:with([type, level, separator], Attrs).

add_child({list_item, Attrs, Children, Ann}, Child) ->
	{list_item, Attrs, [Child|Children], Ann}.

reverse_children({list_item, Attrs, Children, Ann}) ->
	{list_item, Attrs, lists:reverse(Children), Ann}.

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
-endif.
