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

%% The purpose of this pass is to parse the contents of compound
%% blocks (example, sidebar, quote, open and admonition blocks)
%% as a list of blocks. Line numbers of the child blocks are
%% offset by the line of the compound block.
-module(asciideck_compound_pass).

-export([run/1]).

run([]) ->
	[];
run([{Type, Attrs, Content, Ann}|Tail])
		when is_binary(Content), (Type =:= example_block orelse Type =:= sidebar_block
			orelse Type =:= quote_block orelse Type =:= open_block) ->
	[{Type, Attrs, parse(Content, Ann), Ann}|run(Tail)];
run([{admonition, Attrs, Content, Ann}|Tail])
		when is_binary(Content), not is_map_key(paragraph, Attrs) ->
	[{admonition, Attrs, parse(Content, Ann), Ann}|run(Tail)];
%% List items may contain compound blocks.
run([{Type, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs, run(Items), Ann}|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

parse(Content, #{line := Line}) ->
	Passes = [
		asciideck_attributes_pass,
		asciideck_transform_pass,
		asciideck_lists_pass,
		asciideck_tables_pass,
		asciideck_compound_pass
	],
	AST = lists:foldl(fun(M, AST) -> M:run(AST) end,
		asciideck_block_parser:parse(Content), Passes),
	offset(AST, Line);
parse(Content, _) ->
	parse(Content, #{line => 0}).

offset(AST, Offset) ->
	[offset_node(Node, Offset) || Node <- AST].

offset_node({Type, Attrs, Content0, Ann0}, Offset) ->
	Ann = case Ann0 of
		#{line := L} -> Ann0#{line => L + Offset};
		_ -> Ann0
	end,
	Content = case Content0 of
		[{_, _, _, _}|_] -> offset(Content0, Offset);
		_ -> Content0
	end,
	{Type, Attrs, Content, Ann};
offset_node(Node, _) ->
	Node.

-ifdef(TEST).
compound_test() ->
	[{example_block, #{}, [
		{paragraph, #{}, <<"One.">>, #{line := 6}},
		{list, _, [{list_item, _, [{paragraph, _, <<"Two.">>, _}], #{line := 8}}], #{line := 8}}
	], #{line := 5}}] = run([{example_block, #{}, <<"One.\n\n* Two.">>, #{line => 5}}]),
	[{sidebar_block, #{}, [
		{quote_block, #{}, [{paragraph, #{}, <<"Nested.">>, #{line := 3}}], #{line := 2}}
	], #{line := 1}}] = run([{sidebar_block, #{}, <<"____\nNested.\n____">>, #{line => 1}}]),
	ok.
-endif.
