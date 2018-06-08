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
run([{block_title, _, Title, _}|Tail0]) ->
	Tail = apply_attributes(Tail0, #{<<"title">> => Title}),
	run(Tail);
run([{attribute_list, Attrs, <<>>, _}|Tail0]) ->
	Tail = apply_attributes(Tail0, Attrs),
	run(Tail);
run([Block|Tail]) ->
	[Block|run(Tail)].

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
			Element = {Type, maps:merge(Attrs0, Attrs), Content, Ann},
			[Element|Tail]
	end.

%% Block macros already come with a mandatory attribute list.
%% Just to play it safe we drop the attributes for now.
can_apply(block_macro) -> drop;
%% If we hit a list item continuation, drop the attributes for now.
can_apply(list_item_continuation) -> drop;
%% We skip attribute lists and alike and let it sort itself out.
can_apply(block_id) -> skip;
can_apply(attribute_list) -> skip;
can_apply(block_title) -> skip;
%% Everything else is a block.
can_apply(_) -> apply.

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
			<<"title">> => <<"Title">>
		}, <<"Hello!">>, #{line => 2}}
	],
	AST = run(AST0),
	ok.
-endif.
