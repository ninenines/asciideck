%% Copyright (c) 2016-2018, Loïc Hoguin <essen@ninenines.eu>
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

%% The block parser is the first pass of the parsing of Asciidoc
%% files. It only isolates the different top-level blocks and
%% produces a representation that can then be manipulated.
%%
%% Further passes are necessary to propagate the parsed lists
%% of attributes to their respective blocks, to create actual
%% lists from the parsed list items or to parse the contents
%% of tables. Finally a final pass will parse inline elements.
%%
%% This module may be called again for parsing the content
%% of individual table cells.
-module(asciideck_block_parser).

-export([parse/1]).
-export([parse/2]).

%% @todo Temporary export. Move somewhere else.
-export([trim/1]).
-export([trim/2]).
-export([while/2]).

-type ast() :: list(). %% @todo

-record(state, {
	reader :: pid(),
	%% Line number of the line before this reader's first line.
	%% Zero for a whole document. An open block passes the
	%% opener line so children keep the file line number.
	line_offset = 0 :: non_neg_integer()
}).

-define(IS_WS(C), (C =:= $\s) or (C =:= $\t)).

-ifdef(TEST).
-define(NOT(Type, Value), true = Type =/= element(1, hd(Value))).

define_NOT_test() ->
	%% This succeeds.
	?NOT(block_id, parse(<<"[[block,id]]">>)),
	%% This fails.
	{'EXIT', _} = (catch ?NOT(block_id, parse(<<"[[block_id]]">>))),
	ok.
-endif.

-spec parse(binary() | pid()) -> ast().
parse(Data) when is_binary(Data) ->
	parse(Data, 0);
parse(Data) when is_list(Data) ->
	parse(iolist_to_binary(Data));
parse(ReaderPid) when is_pid(ReaderPid) ->
	blocks(#state{reader=ReaderPid}).

%% Offset is the file line of the line before Data's first line.
-spec parse(binary(), non_neg_integer()) -> ast().
parse(Data, Offset) when is_binary(Data), is_integer(Offset), Offset >= 0 ->
	%% @todo Might want to start it supervised.
	{ok, ReaderPid} = asciideck_line_reader:start_link(Data),
	try blocks(#state{reader=ReaderPid, line_offset=Offset})
	after
		%% Stay linked while parsing so a caller crash kills the
		%% reader. Unlink before stopping so a trap_exit caller
		%% does not see the normal shutdown.
		unlink(ReaderPid),
		gen_server:stop(ReaderPid)
	end.

blocks(St) ->
	case block(St) of
		eof -> [];
		Block -> [Block|blocks(St)]
	end.

%% Asciidoc parsing never fails. If a block is not
%% formatted properly, it will be treated as a paragraph.
block(St) ->
	skip(fun empty_line/1, St),
	oneof([
		fun eof/1,
		%% Section titles.
		fun section_title/1,
		%% Block macros.
		fun block_id/1,
		fun block_macro/1,
		%% Lists.
		fun bulleted_list/1,
		fun numbered_list/1,
		fun labeled_list/1,
		fun callout_list/1,
		fun list_item_continuation/1,
		%% Delimited blocks.
		fun listing_block/1,
		fun literal_block/1,
		fun sidebar_block/1,
		fun comment_block/1,
		fun passthrough_block/1,
		fun quote_block/1,
		fun example_block/1,
		fun open_block/1,
		%% Table.
		fun table/1,
		%% Attributes.
		fun attribute_entry/1,
		fun attribute_list/1,
		%% Block title.
		fun block_title/1,
		%% Comment lines.
		fun comment_line/1,
		%% Paragraphs.
		fun literal_para/1,
		fun admonition_para/1,
		fun para/1
	], St).

eof(St) ->
	eof = read_line(St).

-ifdef(TEST).
eof_test() ->
	[] = parse(<<>>).
-endif.

empty_line(St) ->
	<<>> = trim(read_line(St)).

-ifdef(TEST).
empty_line_test() ->
	[] = parse(<<
		"\n"
		"           \n"
		"			\n"
		"\n"
	>>).
-endif.

%% Asciidoc User Guide 11.2
section_title(St) ->
	{Level, Title0} = case read_line(St) of
		<<"=", C, R/bits>> when ?IS_WS(C) -> {0, R};
		<<"==", C, R/bits>> when ?IS_WS(C) -> {1, R};
		<<"===", C, R/bits>> when ?IS_WS(C) -> {2, R};
		<<"====", C, R/bits>> when ?IS_WS(C) -> {3, R};
		<<"=====", C, R/bits>> when ?IS_WS(C) -> {4, R}
	end,
	Ann = ann(St),
	Title1 = trim(Title0),
	%% Optional: trailing title delimiter.
	Trailer = case Level of
		0 -> <<"=">>;
		1 -> <<"==">>;
		2 -> <<"===">>;
		3 -> <<"====">>;
		4 -> <<"=====">>
	end,
	Len = byte_size(Title1) - Level - 2,
	Title = case Title1 of
		<<Title2:Len/binary, WS, Trailer/binary>> when ?IS_WS(WS) -> trim(Title2);
		_ -> trim(Title1)
	end,
	%% Section titles must be followed by at least one empty line.
	_ = empty_line(St),
	%% Good!
	{section_title, #{level => Level}, Title, Ann}.

-ifdef(TEST).
section_title_test() ->
	%% With trailing title delimiter.
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"= Document Title (level 0) =">>),
	[{section_title, #{level := 1}, <<"Section Title (level 1)">>, _}]
		= parse(<<"== Section Title (level 1) ==">>),
	[{section_title, #{level := 2}, <<"Section Title (level 2)">>, _}]
		= parse(<<"=== Section Title (level 2) ===">>),
	[{section_title, #{level := 3}, <<"Section Title (level 3)">>, _}]
		= parse(<<"==== Section Title (level 3) ====">>),
	[{section_title, #{level := 4}, <<"Section Title (level 4)">>, _}]
		= parse(<<"===== Section Title (level 4) =====">>),
	%% Without trailing title delimiter.
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"= Document Title (level 0)">>),
	[{section_title, #{level := 1}, <<"Section Title (level 1)">>, _}]
		= parse(<<"== Section Title (level 1)">>),
	[{section_title, #{level := 2}, <<"Section Title (level 2)">>, _}]
		= parse(<<"=== Section Title (level 2)">>),
	[{section_title, #{level := 3}, <<"Section Title (level 3)">>, _}]
		= parse(<<"==== Section Title (level 3)">>),
	[{section_title, #{level := 4}, <<"Section Title (level 4)">>, _}]
		= parse(<<"===== Section Title (level 4)">>),
	%% Accept more spaces before/after delimiters.
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"=   Document Title (level 0)">>),
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"=   Document Title (level 0) =">>),
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"= Document Title (level 0)   =">>),
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}]
		= parse(<<"= Document Title (level 0) =  ">>),
	%% A space before the first delimiter is not a title.
	?NOT(section_title, parse(<<" = Document Title (level 0)">>)),
	ok.
-endif.

%% Asciidoc User Guide 21.2.1
%%
%% We currently do not implement the <xreflabel> value.
%% I am also not sure what characters are allowed,
%% so what is here is what I came up with guessing.
block_id(St) ->
	<<"[[", Line0/bits>> = read_line(St),
	Line = trim(Line0),
	Len = byte_size(Line) - 2,
	<<BlockID:Len/binary, "]]">> = Line,
	%% Make sure there are only valid characters.
	{BlockID, <<>>} = while(fun(C) ->
		(C =/= $,) andalso (C =/= $[) andalso (C =/= $])
		andalso (C =/= $\s) andalso (C =/= $\t)
	end, BlockID),
	%% Good!
	{block_id, #{id => BlockID}, <<>>, ann(St)}.

-ifdef(TEST).
block_id_test() ->
	%% Valid.
	[{block_id, #{id := <<"X30">>}, <<>>, _}] = parse(<<"[[X30]]">>),
	%% Invalid.
	?NOT(block_id, parse(<<"[[block,id]]">>)),
	?NOT(block_id, parse(<<"[[block[id]]">>)),
	?NOT(block_id, parse(<<"[[block]id]]">>)),
	?NOT(block_id, parse(<<"[[block id]]">>)),
	?NOT(block_id, parse(<<"[[block\tid]]">>)),
	%% Must be hard on the left of the line.
	?NOT(block_id, parse(<<" [[block_id]]">>)),
	?NOT(block_id, parse(<<"\t[[block_id]]">>)),
	ok.
-endif.

%% Asciidoc User Guide 21.2.3
comment_line(St) ->
	<<"//", Comment0/bits>> = read_line(St),
	Comment = trim(Comment0),
	%% Good!
	{comment_line, #{<<"subs">> => <<"verbatim">>}, Comment, ann(St)}.

-ifdef(TEST).
comment_line_test() ->
	[{comment_line, _, <<"This is a comment.">>, _}] = parse(<<"// This is a comment.">>),
	%% We trim the whitespace around the comment.
	[{comment_line, _, <<"This is a comment.">>, _}] = parse(<<"//   This is a comment.">>),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse(<<"// This is a comment.   ">>),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse(<<"//\tThis is a comment.">>),
	[{comment_line, _, <<"This is a comment.">>, _}] = parse(<<"// This is a comment.\t">>),
	[
		{comment_line, _, <<"First line.">>, _},
		{comment_line, _, <<"Second line.">>, _}
	] = parse(<<
		"// First line.\n"
		"// Second line.\n">>),
	%% Must be hard on the left of the line.
	?NOT(comment_line, parse(<<" // This is a comment.">>)),
	?NOT(comment_line, parse(<<"\t// This is a comment.">>)),
	ok.
-endif.

%% We currently implement the following block macros
%% from the Asciidoc User Guide:
%%
%% - image (21.2.2)
%% - include (21.3.1)
%% - ifdef (21.3.2)
%% - ifndef (21.3.2)
%% - endif (21.3.2)
block_macro(St) ->
	Line0 = read_line(St),
	Ann = ann(St),
	%% Name must contain letters, digits or dash characters.
	{Name, <<"::", Line1/bits>>} = while(fun(C) ->
		((C >= $a) andalso (C =< $z))
		orelse ((C >= $A) andalso (C =< $Z))
		orelse ((C >= $0) andalso (C =< $9))
		orelse (C =:= $-)
	end, Line0),
	%% Name must not begin with a dash.
	true = binary:at(Name, 0) =/= $-,
	%% Target must not contain whitespace characters.
	%% It is followed by an [attribute list].
	{Target, AttrList0 = <<"[", _/bits>>} = while(fun(C) ->
		(C =/= $[) andalso (C =/= $\s) andalso (C =/= $\t)
	end, Line1),
	AttrList1 = trim(AttrList0),
	{attribute_list, AttrList, <<>>, _} = attribute_list(St, AttrList1),
	%% Block macros must be followed by at least one empty line.
	_ = empty_line(St),
	{block_macro, AttrList#{
		name => Name,
		target => Target
	}, <<>>, Ann}.

-ifdef(TEST).
block_macro_image_test() ->
	[{block_macro, #{
		name := <<"image">>,
		target := <<"images/layout.png">>,
		1 := <<"J14P main circuit board">>
	}, <<>>, _}] = parse(<<"image::images/layout.png[J14P main circuit board]">>),
	[{block_macro, #{
		name := <<"image">>,
		target := <<"images/layout.png">>,
		1 := <<"J14P main circuit board">>,
		<<"title">> := <<"Main circuit board">>
	}, <<>>, _}] = parse(
		<<"image::images/layout.png[\"J14P main circuit board\", "
			"title=\"Main circuit board\"]">>),
	ok.

block_macro_include_test() ->
	[{block_macro, #{
		name := <<"include">>,
		target := <<"chapter1.txt">>,
		<<"tabsize">> := <<"4">>
	}, <<>>, _}] = parse(<<"include::chapter1.txt[tabsize=4]">>),
	ok.

block_macro_ifdef_test() ->
	[{block_macro, #{
		name := <<"ifdef">>,
		target := <<"revnumber">>,
		0 := <<>>
	}, <<>>, _}] = parse(<<"ifdef::revnumber[]">>),
	[{block_macro, #{
		name := <<"ifdef">>,
		target := <<"revnumber">>,
		1 := <<"Version number 42">>
	}, <<>>, _}] = parse(<<"ifdef::revnumber[Version number 42]">>),
	ok.

block_macro_ifndef_test() ->
	[{block_macro, #{
		name := <<"ifndef">>,
		target := <<"revnumber">>,
		0 := <<>>
	}, <<>>, _}] = parse(<<"ifndef::revnumber[]">>),
	ok.

block_macro_endif_test() ->
	[{block_macro, #{
		name := <<"endif">>,
		target := <<"revnumber">>,
		0 := <<>>
	}, <<>>, _}] = parse(<<"endif::revnumber[]">>),
	%% Some macros accept an empty target.
	[{block_macro, #{
		name := <<"endif">>,
		target := <<>>,
		0 := <<>>
	}, <<>>, _}] = parse(<<"endif::[]">>),
	ok.
-endif.

%% Asciidoc User Guide 17.1
bulleted_list(St) ->
	Line0 = read_line(St),
	Line1 = trim(Line0),
	{Type0, Level, ListItem} = case Line1 of
		<<"-", C, R/bits>> when ?IS_WS(C) -> {dash, 1, R};
		<<"*", C, R/bits>> when ?IS_WS(C) -> {star, 1, R};
		<<"**", C, R/bits>> when ?IS_WS(C) -> {star, 2, R};
		<<"***", C, R/bits>> when ?IS_WS(C) -> {star, 3, R};
		<<"****", C, R/bits>> when ?IS_WS(C) -> {star, 4, R};
		<<"*****", C, R/bits>> when ?IS_WS(C) -> {star, 5, R}
	end,
	Type = case Type0 of
		dash -> bulleted_alt;
		star -> bulleted
	end,
	list_item(St, #{
		type => Type,
		level => Level
	}, ListItem).

-ifdef(TEST).
bulleted_list_test() ->
	[{list_item, #{
		type := bulleted_alt,
		level := 1
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"- List item.">>),
	[{list_item, #{
		type := bulleted,
		level := 1
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"* List item.">>),
	[{list_item, #{
		type := bulleted,
		level := 2
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"** List item.">>),
	[{list_item, #{
		type := bulleted,
		level := 3
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"*** List item.">>),
	[{list_item, #{
		type := bulleted,
		level := 4
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"**** List item.">>),
	[{list_item, #{
		type := bulleted,
		level := 5
	}, [{paragraph, _, <<"List item.">>, _}], _}] = parse(<<"***** List item.">>),
	%% Two list items one after the other.
	[
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, _, <<"List item 1.">>, _}], _},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, _, <<"List item 2.">>, _}], _}
	] = parse(<<"* List item 1.\n* List item 2.">>),
	ok.
-endif.

%% Asciidoc User Guide 17.2
%%
%% We currently only implement implicit numbering.
numbered_list(St) ->
	Line0 = read_line(St),
	Line1 = trim(Line0),
	{Level, ListItem} = case Line1 of
		<<".", C, R/bits>> when ?IS_WS(C) -> {1, R};
		<<"..", C, R/bits>> when ?IS_WS(C) -> {2, R};
		<<"...", C, R/bits>> when ?IS_WS(C) -> {3, R};
		<<"....", C, R/bits>> when ?IS_WS(C) -> {4, R};
		<<".....", C, R/bits>> when ?IS_WS(C) -> {5, R}
	end,
	list_item(St, #{
		type => numbered,
		level => Level
	}, ListItem).

-ifdef(TEST).
numbered_list_test() ->
	[{list_item, #{
		type := numbered,
		level := 1
	}, [{paragraph, _, <<"Arabic (decimal) numbered list item.">>, _}], _}]
		= parse(<<". Arabic (decimal) numbered list item.">>),
	[{list_item, #{
		type := numbered,
		level := 2
	}, [{paragraph, _, <<"Lower case alpha (letter) numbered list item.">>, _}], _}]
		= parse(<<".. Lower case alpha (letter) numbered list item.">>),
	[{list_item, #{
		type := numbered,
		level := 3
	}, [{paragraph, _, <<"Lower case roman numbered list item.">>, _}], _}]
		= parse(<<"... Lower case roman numbered list item.">>),
	[{list_item, #{
		type := numbered,
		level := 4
	}, [{paragraph, _, <<"Upper case alpha (letter) numbered list item.">>, _}], _}]
		= parse(<<".... Upper case alpha (letter) numbered list item.">>),
	[{list_item, #{
		type := numbered,
		level := 5
	}, [{paragraph, _, <<"Upper case roman numbered list item.">>, _}], _}]
		= parse(<<"..... Upper case roman numbered list item.">>),
	%% Two list items one after the other.
	[
		{list_item, #{type := numbered, level := 1},
			[{paragraph, _, <<"List item 1.">>, _}], _},
		{list_item, #{type := numbered, level := 1},
			[{paragraph, _, <<"List item 2.">>, _}], _}
	] = parse(<<". List item 1.\n. List item 2.">>),
	ok.
-endif.

%% Asciidoc User Guide 17.3
%%
%% The Asciidoc User Guide makes it sound like the
%% label must be hard on the left margin but we don't
%% enforce that to simplify the implementation.
labeled_list(St) ->
	Line0 = read_line(St),
	%% We can't match directly to find the list separator,
	%% we have to search for it.
	{Label0, Sep, ListItem0} = find_labeled_list(Line0),
	Label = trim(Label0),
	ListItem = trim(ListItem0),
	%% The label must not be empty.
	true = trim(Label) =/= <<>>,
	list_item(St, #{
		type => labeled,
		separator => Sep,
		label => Label
	}, ListItem).

find_labeled_list(Line) ->
	find_labeled_list(Line, <<>>).

%% We don't have a final clause with an empty binary because
%% we want to crash if we don't find a labeled list.
find_labeled_list(<<"::">>, Acc) -> {Acc, <<"::">>, <<>>};
find_labeled_list(<<":::">>, Acc) -> {Acc, <<":::">>, <<>>};
find_labeled_list(<<"::::">>, Acc) -> {Acc, <<"::::">>, <<>>};
find_labeled_list(<<";;">>, Acc) -> {Acc, <<";;">>, <<>>};
find_labeled_list(<<"::", C, R/bits>>, Acc) when ?IS_WS(C) -> {Acc, <<"::">>, R};
find_labeled_list(<<":::", C, R/bits>>, Acc) when ?IS_WS(C) -> {Acc, <<":::">>, R};
find_labeled_list(<<"::::", C, R/bits>>, Acc) when ?IS_WS(C) -> {Acc, <<"::::">>, R};
find_labeled_list(<<";;", C, R/bits>>, Acc) when ?IS_WS(C) -> {Acc, <<";;">>, R};
find_labeled_list(<<C, R/bits>>, Acc) -> find_labeled_list(R, <<Acc/binary, C>>).

-ifdef(TEST).
labeled_list_test() ->
	[{list_item, #{type := labeled, separator := <<"::">>, label := <<"Question">>},
		[{paragraph, _, <<"Answer!">>, _}], _}] = parse(<<"Question:: Answer!">>),
	[{list_item, #{type := labeled, separator := <<"::">>, label := <<"Question">>},
		[{paragraph, _, <<"Answer!">>, _}], _}] = parse(<<"Question::\n  Answer!">>),
	%% Long snippet from the Asciidoc User Guide.
	[
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"In">>},
			[{paragraph, _, <<>>, _}], _},
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"Lorem">>},
			[{paragraph, _, <<"Fusce euismod commodo velit.">>, _}], _},
		{literal_paragraph, _, <<"Fusce euismod commodo velit.">>, _},
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"Ipsum">>},
			[{paragraph, _, <<"Vivamus fringilla mi eu lacus.">>, _}], _},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, _, <<"Vivamus fringilla mi eu lacus.">>, _}], _},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, _, <<"Donec eget arcu bibendum nunc consequat lobortis.">>, _}], _},
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"Dolor">>},
			[{paragraph, _, <<"Donec eget arcu bibendum nunc consequat lobortis.">>, _}], _},
		{list_item, #{type := labeled, separator := <<";;">>, label := <<"Suspendisse">>},
			[{paragraph, _, <<"A massa id sem aliquam auctor.">>, _}], _},
		{list_item, #{type := labeled, separator := <<";;">>, label := <<"Morbi">>},
			[{paragraph, _, <<"Pretium nulla vel lorem.">>, _}], _},
		{list_item, #{type := labeled, separator := <<";;">>, label := <<"In">>},
			[{paragraph, _, <<"Dictum mauris in urna.">>, _}], _},
		{list_item, #{type := labeled, separator := <<":::">>, label := <<"Vivamus">>},
			[{paragraph, _, <<"Fringilla mi eu lacus.">>, _}], _},
		{list_item, #{type := labeled, separator := <<":::">>, label := <<"Donec">>},
			[{paragraph, _, <<"Eget arcu bibendum nunc consequat lobortis.">>, _}], _}
	] = parse(<<
		"In::\n"
		"Lorem::\n"
		"  Fusce euismod commodo velit.\n"
		"\n"
		"  Fusce euismod commodo velit.\n"
		"\n"
		"Ipsum:: Vivamus fringilla mi eu lacus.\n"
		"  * Vivamus fringilla mi eu lacus.\n"
		"  * Donec eget arcu bibendum nunc consequat lobortis.\n"
		"Dolor::\n"
		"  Donec eget arcu bibendum nunc consequat lobortis.\n"
		"  Suspendisse;;\n"
		"    A massa id sem aliquam auctor.\n"
		"  Morbi;;\n"
		"    Pretium nulla vel lorem.\n"
		"  In;;\n"
		"    Dictum mauris in urna.\n"
		"    Vivamus::: Fringilla mi eu lacus.\n"
		"    Donec:::   Eget arcu bibendum nunc consequat lobortis.\n">>),
	ok.
-endif.

%% Asciidoc User Guide 20
callout_list(St) ->
	Line0 = read_line(St),
	{Number, ListItem} = callout_marker(Line0),
	list_item(St, #{
		type => callout,
		number => Number
	}, ListItem).

%% <1> annotation
%% 1> annotation
%% > annotation (implicit numbering)
%% User Guide 20: optional <, digits, >, whitespace, then item text.
callout_marker(Line) ->
	{Number, Rest} = callout_prefix(Line),
	<<C, Item/bits>> = Rest,
	true = ?IS_WS(C),
	true = trim(Item) =/= <<>>,
	{Number, Rest}.

callout_prefix(<<"<", C, Rest/bits>>) when C >= $0, C =< $9 ->
	{Num, <<">", R/bits>>} = while(fun is_digit/1, <<C, Rest/bits>>),
	{binary_to_integer(Num), R};
callout_prefix(<<C, Rest/bits>>) when C >= $0, C =< $9 ->
	{Num, <<">", R/bits>>} = while(fun is_digit/1, <<C, Rest/bits>>),
	{binary_to_integer(Num), R};
callout_prefix(<<">", Rest/bits>>) ->
	{auto, Rest}.

is_digit(C) ->
	(C >= $0) andalso (C =< $9).

is_callout_line(Line) ->
	try
		_ = callout_marker(Line),
		true
	catch _:_ ->
		false
	end.

-ifdef(TEST).
callout_list_test() ->
	[{list_item, #{type := callout, number := 1},
		[{paragraph, _, <<"This directory holds MS-DOS.">>, _}], _}]
		= parse(<<"<1> This directory holds MS-DOS.">>),
	[{list_item, #{type := callout, number := 2},
		[{paragraph, _, <<"System startup code for DOS.">>, _}], _}]
		= parse(<<"2> System startup code for DOS.">>),
	[{list_item, #{type := callout, number := auto},
		[{paragraph, _, <<"Some sort of Windows 3.1 hack.">>, _}], _}]
		= parse(<<"> Some sort of Windows 3.1 hack.">>),
	[
		{list_item, #{type := callout, number := 1},
			[{paragraph, _, <<"First.">>, _}], _},
		{list_item, #{type := callout, number := 2},
			[{paragraph, _, <<"Second.">>, _}], _}
	] = parse(<<"<1> First.\n<2> Second.">>),
	%% Must be hard against the left margin.
	?NOT(callout, parse(<<" <1> Indented.">>)),
	%% Whitespace and item text are required.
	?NOT(callout, parse(<<"<1>NoSpace">>)),
	?NOT(callout, parse(<<"3>2 is false">>)),
	?NOT(callout, parse(<<">=">>)),
	ok.
-endif.

%% Asciidoc User Guide 17
%%
%% We do not apply rules about blocks being contained in
%% the list item at this stage of parsing. We only concern
%% ourselves with identifying blocks, and then another pass
%% will build a tree from the result of this pass.
list_item(St, Attrs, ListItem0) ->
	ListItem1 = trim(ListItem0),
	Ann = ann(St),
	%% For labeled lists, we may need to skip empty lines
	%% until the start of the list item contents, since
	%% it can begin on a separate line from the label.
	_ = case {ListItem1, Attrs} of
		{<<>>, #{type := labeled}} ->
			read_while(St, fun skip_empty_lines/1, <<>>);
		_ ->
			ok
	end,
	%% A list item ends on end of file, an empty line, another
	%% list, an attribute list, or a delimited block.
	%% Any indentation is optional and therefore removed.
	ListItem = read_while(St, fun fold_list_item/1, ListItem1),
	{list_item, Attrs, [{paragraph, #{}, ListItem, Ann}], Ann}.

skip_empty_lines(eof) ->
	done;
skip_empty_lines(Line) ->
	case trim(Line) of
		<<>> -> {more, <<>>};
		_ -> done
	end.

fold_list_item(eof) ->
	done;
fold_list_item(Line0) ->
	case trim(Line0) of
		<<>> -> done;
		<<"+">> -> done;
		<<"//", _/bits >> -> done;
		<<"-", C, _/bits>> when ?IS_WS(C) -> done;
		<<"*", C, _/bits>> when ?IS_WS(C) -> done;
		<<"**", C, _/bits>> when ?IS_WS(C) -> done;
		<<"***", C, _/bits>> when ?IS_WS(C) -> done;
		<<"****", C, _/bits>> when ?IS_WS(C) -> done;
		<<"*****", C, _/bits>> when ?IS_WS(C) -> done;
		<<".", C, _/bits>> when ?IS_WS(C) -> done;
		<<"..", C, _/bits>> when ?IS_WS(C) -> done;
		<<"...", C, _/bits>> when ?IS_WS(C) -> done;
		<<"....", C, _/bits>> when ?IS_WS(C) -> done;
		<<".....", C, _/bits>> when ?IS_WS(C) -> done;
		Line ->
			%% Same terminators as a paragraph, except list markers,
			%% which this reader stops for above. A block title is
			%% not one of them.
			case attribute_list_line(Line0) orelse delimited_line(Line0) of
				true ->
					done;
				false ->
					try find_labeled_list(Line) of
						{_, _, _} -> done
					catch _:_ ->
						case is_callout_line(Line0) of
							true -> done;
							false -> {more, Line}
						end
					end
			end
	end.

-ifdef(TEST).
list_item_test() ->
	[
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"List item.">>, _}], _},
		{list_item, #{type := bulleted, level := 2},
			[{paragraph, #{}, <<"List item.">>, _}], _},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"List item.">>, _}], _},
		{list_item, #{type := numbered, level := 1},
			[{paragraph, #{}, <<"List item.">>, _}], _},
		{list_item, #{type := numbered, level := 1},
			[{paragraph, #{}, <<"List item.">>, _}], _},
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"List item.">>, _}], _}
	] = parse(<<
		"* List item.\n"
		"** List item.\n"
		"* List item.\n"
		"  . List item.\n"
		"  . List item.\n"
		"* List item.\n">>),
	%% Properly detect a labeled list.
	[
		{list_item, #{type := bulleted, level := 1},
			[{paragraph, #{}, <<"List item.\nMultiline.">>, _}], _},
		{list_item, #{type := labeled, label := <<"Question">>},
			[{paragraph, #{}, <<"Answer!">>, _}], _}
	] = parse(<<
		"* List item.\n"
		"Multiline.\n"
		"Question:: Answer!\n">>),
	%% An attribute list or a delimited block ends the item.
	[
		{list_item, _, [{paragraph, _, <<"One.">>, _}], _},
		{attribute_list, _, _, _},
		{list_item_continuation, _, _, _},
		{paragraph, _, <<"code">>, _}
	] = parse(<<"* One.\n[source,erlang]\n+\ncode\n">>),
	[
		{list_item, _, [{paragraph, _, <<"One.">>, _}], _},
		{listing_block, _, <<"code">>, _}
	] = parse(<<"* One.\n----\ncode\n----\n">>),
	ok.
-endif.

%% Asciidoc User Guide 17.7
list_item_continuation(St) ->
	%% Continuations are a single + hard against the left margin.
	<<$+, Whitespace/bits>> = read_line(St),
	<<>> = trim(Whitespace),
	Ann = ann(St),
	%% The lists pass may turn this + into a paragraph. Capture
	%% the lines the paragraph reader would keep, then rewind so
	%% those lines are still parsed as blocks for a real continuation.
	{Raw, RawLast} = plus_raw(St, maps:get(line, Ann)),
	{list_item_continuation, #{}, <<>>, Ann#{raw => Raw, raw_last => RawLast}}.

%% Paragraph terminators are a blank line, a + hard against
%% the left margin, an attribute list, or a delimited block.
%% A // comment at the left margin is skipped. List markers
%% and block titles are kept: this reader is not the one
%% that ends a paragraph at a list.
plus_raw(St=#state{reader=ReaderPid}, Line) ->
	Start = asciideck_reader:get_position(ReaderPid),
	{Lines, Count} = collect_plus_lines(St, [], 0),
	asciideck_reader:set_position(ReaderPid, Start),
	{join_plus_lines([<<"+">>|Lines]), Line + Count}.

collect_plus_lines(St, Acc, Count) ->
	case read_line(St) of
		eof ->
			{lists:reverse(Acc), Count};
		Line ->
			case classify_plus_line(Line) of
				stop ->
					{lists:reverse(Acc), Count};
				skip ->
					collect_plus_lines(St, Acc, Count + 1);
				keep ->
					collect_plus_lines(St, [trim(Line, trailing)|Acc], Count + 1)
			end
	end.

classify_plus_line(Line) ->
	case plus_line_end(Line) of
		true ->
			stop;
		false ->
			case is_margin_comment(Line) of
				true -> skip;
				false -> keep
			end
	end.

%% Only a + in column 0 ends the paragraph. Indent stays text.
plus_line_end(Line0) ->
	case trim(Line0, trailing) of
		<<>> -> true;
		<<"+">> -> true;
		_ ->
			attribute_list_line(Line0) orelse delimited_line(Line0)
	end.

%% ^//([^/].*)?$ after trailing space is removed. /// is text.
is_margin_comment(Line0) ->
	case trim(Line0, trailing) of
		<<"//">> ->
			true;
		<<"//", C, _/bits>> when C =/= $/ ->
			true;
		_ ->
			false
	end.

join_plus_lines([Line]) ->
	Line;
join_plus_lines([Line|Rest]) ->
	<<Line/binary, $\n, (join_plus_lines(Rest))/binary>>.

-ifdef(TEST).
list_item_continuation_test() ->
	[{list_item_continuation, _, _, _}] = parse(<<"+">>),
	[{list_item_continuation, _, _, _}] = parse(<<"+   ">>),
	[{list_item_continuation, _, _, _}] = parse(<<"+\n">>),
	%% The following lines stay blocks. raw keeps the paragraph text.
	[
		{list_item_continuation, _, _, #{raw := <<"+\n  foo()">>, raw_last := 2}},
		{literal_paragraph, _, <<"foo()">>, #{line := 2}}
	] = parse(<<"+\n  foo()\n">>),
	%% A comment is not paragraph text, but it is part of the span.
	[
		{list_item_continuation, _, _, #{raw := <<"+\ntext">>, raw_last := 3}},
		{comment_line, _, <<"note">>, #{line := 2}},
		{paragraph, _, <<"text">>, #{line := 3}}
	] = parse(<<"+\n// note\ntext\n">>),
	%% An indented + or // is text. /// is not a comment either.
	[
		{list_item_continuation, _, _, #{raw := <<"+\n  +\nmore">>}},
		{literal_paragraph, _, <<"+">>, _},
		{paragraph, _, <<"more">>, _}
	] = parse(<<"+\n  +\nmore\n">>),
	[
		{list_item_continuation, _, _, #{
			raw := <<"+\n  // not a comment\ntext">>
		}},
		{literal_paragraph, _, <<"// not a comment">>, _},
		{paragraph, _, <<"text">>, _}
	] = parse(<<"+\n  // not a comment\ntext\n">>),
	%% /// matches the comment parser. raw still keeps the line.
	[
		{list_item_continuation, _, _, #{
			raw := <<"+\n/// not comment\ntext">>
		}},
		{comment_line, _, <<"/ not comment">>, _},
		{paragraph, _, <<"text">>, _}
	] = parse(<<"+\n/// not comment\ntext\n">>),
	%% The snapshot keeps the source lines. The blocks are still parsed.
	[
		{list_item_continuation, _, _, #{
			raw := <<"+\nNOTE: careful\nstill">>,
			raw_last := 3
		}},
		{admonition, _, <<"careful\nstill">>, #{line := 2}}
	] = parse(<<"+\nNOTE: careful\nstill\n">>),
	[
		{list_item_continuation, _, _, #{raw := <<"+">>, raw_last := 1}},
		{table, _, _, #{line := 2}}
	] = parse(<<"+\n|===\n|A\n|===\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.2
listing_block(St) ->
	delimited_block(St, listing_block, $-, #{<<"subs">> => <<"verbatim">>}).

-ifdef(TEST).
listing_block_test() ->
	Block = <<
		"#include <stdio.h>\n"
		"\n"
		"int main() {\n"
		"   printf(\"Hello World!\n\");\n"
		"   exit(0);\n"
		"}">>,
	[{listing_block, _, Block, _}] = parse(<<
		"--------------------------------------\n",
		Block/binary, "\n"
		"--------------------------------------\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.3
literal_block(St) ->
	delimited_block(St, literal_block, $., #{<<"subs">> => <<"verbatim">>}).

-ifdef(TEST).
literal_block_test() ->
	Block = <<
		"Consul *necessitatibus* per id,\n"
		"consetetur, eu pro everti postulant\n"
		"homero verear ea mea, qui.">>,
	[{literal_block, _, Block, _}] = parse(<<
		"...................................\n",
		Block/binary, "\n"
		"...................................\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.4
sidebar_block(St) ->
	delimited_block(St, sidebar_block, $*).

-ifdef(TEST).
sidebar_block_test() ->
	Block = <<
		"Any AsciiDoc SectionBody element (apart from\n"
		"SidebarBlocks) can be placed inside a sidebar.">>,
	[{sidebar_block, _, Block, _}] = parse(<<
		"************************************************\n",
		Block/binary, "\n"
		"************************************************\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.5
comment_block(St) ->
	delimited_block(St, comment_block, $/).

-ifdef(TEST).
comment_block_test() ->
	Block = <<
		"CommentBlock contents are not processed by\n"
		"asciidoc(1).">>,
	[{comment_block, _, Block, _}] = parse(<<
		"//////////////////////////////////////////\n",
		Block/binary, "\n"
		"//////////////////////////////////////////\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.6
passthrough_block(St) ->
	delimited_block(St, passthrough_block, $+, #{<<"subs">> => <<"verbatim">>}).

-ifdef(TEST).
passthrough_block_test() ->
	Block = <<
		"<table border=\"1\"><tr>\n"
		"  <td>*Cell 1*</td>\n"
		"  <td>*Cell 2*</td>\n"
		"</tr></table>">>,
	[{passthrough_block, _, Block, _}] = parse(<<
		"++++++++++++++++++++++++++++++++++++++\n",
		Block/binary, "\n"
		"++++++++++++++++++++++++++++++++++++++\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.7
quote_block(St) ->
	delimited_block(St, quote_block, $_).

-ifdef(TEST).
quote_block_test() ->
	Block = <<
		"As he spoke there was the sharp sound of horses' hoofs and\n"
		"grating wheels against the curb, followed by a sharp pull at the\n"
		"bell. Holmes whistled.\n"
		"\n"
		"\"A pair, by the sound,\" said he. \"Yes,\" he continued, glancing\n"
		"out of the window. \"A nice little brougham and a pair of\n"
		"beauties. A hundred and fifty guineas apiece. There's money in\n"
		"this case, Watson, if there is nothing else.\"">>,
	[{quote_block, _, Block, _}] = parse(<<
		"____________________________________________________________________\n",
		Block/binary, "\n"
		"____________________________________________________________________\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.8
example_block(St) ->
	delimited_block(St, example_block, $=).

-ifdef(TEST).
example_block_test() ->
	Block = <<
		"Qui in magna commodo, est labitur dolorum an. Est ne magna primis\n"
		"adolescens.">>,
	[{example_block, _, Block, _}] = parse(<<
		"=====================================================================\n",
		Block/binary, "\n"
		"=====================================================================\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16
delimited_block(St, Name, Char) ->
	delimited_block(St, Name, Char, #{}, <<Char, Char, Char, Char>>).

delimited_block(St, Name, Char, Attrs) ->
	delimited_block(St, Name, Char, Attrs, <<Char, Char, Char, Char>>).

delimited_block(St, Name, Char, Attrs, Four) ->
	%% A delimiter block begins by a series of four or more repeated characters.
	<<Four:4/binary, Line0/bits>> = read_line(St),
	Ann = ann(St),
	Line = trim(Line0, trailing),
	repeats(Line, Char),
	%% Get the content of the block as-is.
	%% A table body must keep leading empty lines so cell line
	%% numbers still count them. Other blocks drop those lines
	%% from their text, as they always have.
	Keep = case Name of
		table -> keep;
		_ -> drop
	end,
	Block = read_while(St, fun(L) -> fold_delimited_block(L, Four, Char) end, <<>>, Keep),
	%% Skip the trailing delimiter line.
	_ = read_line(St),
	{Name, Attrs, Block, Ann}.

%% Accept eof as a closing delimiter.
fold_delimited_block(eof, _, _) ->
	done;
fold_delimited_block(Line0, Four, Char) ->
	case Line0 of
		<<Four:4/binary, Line1/bits>> ->
			try
				Line = trim(Line1, trailing),
				repeats(Line, Char),
				done
			catch _:_ ->
				{more, Line0}
			end;
		_ ->
			{more, Line0}
	end.

-ifdef(TEST).
delimited_block_test() ->
	%% Confirm that the block ends at eof.
	%%
	%% We see an extra line break because asciideck_line_reader adds
	%% one at the end of every files to ease processing.
	[{listing_block, _, <<"Hello!\n\n">>, _}] = parse(<<
		"----\n"
		"Hello!\n">>),
	%% Same without a trailing line break.
	%%
	%% We also see an extra line break for the aforementioned reasons.
	[{listing_block, _, <<"Hello!\n">>, _}] = parse(<<
		"----\n"
		"Hello!">>),
	%% A leading empty line is not part of the listing text.
	[{listing_block, _, <<"Hi">>, _}] = parse(<<
		"----\n"
		"\n"
		"Hi\n"
		"----\n">>),
	ok.
-endif.

%% Asciidoc User Guide 16.10
%%
%% Open blocks are delimited by a line of exactly two hyphens.
%% Their contents are block elements, parsed like the rest of
%% the document.
open_block(St) ->
	<<"--">> = trim(read_line(St), trailing),
	Ann = ann(St),
	%% Keep a leading empty line in the slice. The nested parse
	%% skips it, and the line offset then matches the file.
	%% raw is that slice. listing, literal, and source styles
	%% render it instead of the parsed children.
	Block = read_while(St, fun fold_open_block/1, <<>>, keep),
	%% Skip the closing delimiter when present.
	_ = read_line(St),
	%% The slice starts on the line after the opener.
	Children = case Block of
		<<>> -> [];
		_ -> parse(Block, maps:get(line, Ann))
	end,
	{open_block, #{}, Children, Ann#{raw => Block}}.

fold_open_block(eof) ->
	done;
fold_open_block(Line0) ->
	case trim(Line0, trailing) of
		<<"--">> -> done;
		_ -> {more, Line0}
	end.

-ifdef(TEST).
open_block_test() ->
	[{open_block, _, [
		{paragraph, _, <<"Hello!">>, _}
	], _}] = parse(<<
		"--\n"
		"Hello!\n"
		"--\n">>),
	[{open_block, _, [
		{paragraph, _, <<"Hello!">>, _},
		{paragraph, _, <<"World!">>, _}
	], _}] = parse(<<
		"--\n"
		"Hello!\n"
		"\n"
		"World!\n"
		"--\n">>),
	%% Nested blocks are parsed.
	[{open_block, _, [
		{paragraph, _, <<"Hello!">>, _},
		{listing_block, _, <<"f() -> ok.">>, _}
	], _}] = parse(<<
		"--\n"
		"Hello!\n"
		"\n"
		"----\n"
		"f() -> ok.\n"
		"----\n"
		"--\n">>),
	%% Must be exactly two hyphens, hard against the left margin.
	?NOT(open_block, parse(<<"---\nHello!\n---\n">>)),
	?NOT(open_block, parse(<<"  --\nHello!\n  --\n">>)),
	ok.

%% Parsing a binary must not leave the line reader running.
open_block_reader_test() ->
	Before = line_readers(),
	_ = parse(<<"--\nHello!\n\n----\nHi\n----\n--\n">>),
	Started = [Pid || Pid <- line_readers(), not lists:member(Pid, Before)],
	[] = Started,
	ok.

%% The caller owns a reader pid (stdin). Parsing must not stop it.
reader_pid_kept_test() ->
	{ok, Pid} = asciideck_line_reader:start_link(<<"Hello\n">>),
	[{paragraph, _, <<"Hello">>, _}] = parse(Pid),
	true = is_process_alive(Pid),
	gen_server:stop(Pid),
	ok.

%% Stopping a reader must not deliver an exit to the caller.
reader_stop_silent_test() ->
	process_flag(trap_exit, true),
	_ = asciideck:parse(<<
		"--\n"
		"\n"
		"Hello\n"
		"--\n"
		"\n"
		"|===\n"
		"|A |B\n"
		"|===\n"
	>>),
	receive
		{'EXIT', _, Reason} -> error({reader_exit, Reason})
	after 0 -> ok
	end.

line_readers() ->
	[Pid || Pid <- processes(),
		case process_info(Pid, dictionary) of
			{dictionary, Dict} ->
				proplists:get_value('$initial_call', Dict)
					=:= {asciideck_line_reader, init, 1};
			_ ->
				false
		end].
-endif.

%% Asciidoc User Guide 23
%%
%% We do not parse the table in this pass. Instead we
%% treat it like any other delimited block.
table(St) ->
	delimited_block(St, table, $=, #{}, <<"|===">>).

-ifdef(TEST).
table_test() ->
	Block = <<
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C">>,
	[{table, _, Block, _}] = parse(<<
		"|=======\n",
		Block/binary, "\n"
		"|=======\n">>),
	ok.
-endif.

%% Asciidoc User Guide 28
attribute_entry(St) ->
	%% Attribute entries begin with a colon hard against the left margin.
	<<$:, Line0/bits>> = read_line(St),
	Ann = ann(St),
	{Name0, <<$:, Value0/bits>>} = while(fun(C) -> C =/= $: end, Line0),
	true = Name0 =/= <<>>,
	{Name1, Unset} = case Name0 of
		<<>> ->
			{Name0, false};
		_ ->
			PrefixLen = byte_size(Name0) - 1,
			case Name0 of
				<<Prefix:PrefixLen/binary, $!>> when Prefix =/= <<>> ->
					{Prefix, true};
				_ ->
					{Name0, false}
			end
	end,
	Name = sanitize_attr_name(Name1),
	true = Name =/= <<>>,
	Value = attribute_entry_value(St, Value0),
	{attribute_entry, #{name => Name, unset => Unset}, Value, Ann}.

sanitize_attr_name(Name) ->
	<< <<(attr_name_char(C))/binary>> || <<C>> <= Name >>.

attr_name_char(C) when C >= $A, C =< $Z -> <<(C + 32)>>;
attr_name_char(C) when C >= $a, C =< $z; C >= $0, C =< $9; C =:= $-; C =:= $_ ->
	<<C>>;
attr_name_char(_) ->
	<<>>.

attribute_entry_value(St, Value0) ->
	Value1 = trim(Value0, trailing),
	Len = byte_size(Value1),
	case Value1 of
		<<Prefix:(Len - 2)/binary, $\s, $+>> when Len >= 2 ->
			case read_line(St) of
				eof ->
					trim(Prefix);
				Next ->
					Continued = <<Prefix/binary, $\s, (trim(Next))/binary>>,
					attribute_entry_value(St, Continued)
			end;
		_ ->
			trim(Value1)
	end.

-ifdef(TEST).
attribute_entry_test() ->
	[{attribute_entry, #{name := <<"author">>, unset := false},
		<<"Stuart Rackham">>, _}] = parse(<<":author: Stuart Rackham">>),
	[{attribute_entry, #{name := <<"authorinitials">>, unset := false},
		<<"JB">>, _}] = parse(<<":Author Initials: JB">>),
	[{attribute_entry, #{name := <<"revnumber">>, unset := false},
		<<"5.1.1">>, _}] = parse(<<":revnumber: 5.1.1">>),
	%% Empty value.
	[{attribute_entry, #{name := <<"toc">>, unset := false},
		<<>>, _}] = parse(<<":toc:">>),
	%% Unset an attribute.
	[{attribute_entry, #{name := <<"toc">>, unset := true},
		<<>>, _}] = parse(<<":toc!:">>),
	%% Continuation lines end with space and plus.
	[{attribute_entry, #{name := <<"description">>, unset := false},
		<<"AsciiDoc is a text document format for writing notes, "
		  "documentation, articles, books, slideshows, web pages "
		  "and man pages.">>, _}] = parse(<<
			":description: AsciiDoc is a text document format for writing notes, +\n"
			"              documentation, articles, books, slideshows, web pages +\n"
			"              and man pages.">>),
	%% Must be hard against the left margin and have a closing colon.
	?NOT(attribute_entry, parse(<<":author Stuart Rackham">>)),
	?NOT(attribute_entry, parse(<<" :author: Stuart Rackham">>)),
	ok.
-endif.

%% Asciidoc User Guide 14, 29
attribute_list(St) ->
	AttrList = read_line(St),
	attribute_list(St, AttrList).

attribute_list(St, AttrList0) ->
	%% First we remove the enclosing square brackets.
	<<$[, AttrList1/bits>> = AttrList0,
	AttrList2 = trim(AttrList1),
	Len = byte_size(AttrList2) - 1,
	<<AttrList3:Len/binary, $]>> = AttrList2,
	AttrList = asciideck_attributes_parser:parse(AttrList3),
	{attribute_list, AttrList, <<>>, ann(St)}.

-ifdef(TEST).
attribute_list_test() ->
	[{attribute_list, #{0 := <<"Hello">>, 1 := <<"Hello">>}, <<>>, _}]
		= parse(<<"[Hello]">>),
	[{attribute_list, #{
		1 := <<"quote">>,
		2 := <<"Bertrand Russell">>,
		3 := <<"The World of Mathematics (1956)">>
	}, <<>>, _}]
		= parse(<<"[quote, Bertrand Russell, The World of Mathematics (1956)]">>),
	[{attribute_list, #{
		1 := <<"22 times">>,
		<<"backcolor">> := <<"#0e0e0e">>,
		<<"options">> := <<"noborders,wide">>
	}, <<>>, _}]
		= parse(<<"[\"22 times\", backcolor=\"#0e0e0e\", options=\"noborders,wide\"]">>),
	[{attribute_list, #{
		1 := <<"A footnote&#44; &#34;with an image&#34; image:smallnew.png[]">>
	}, <<>>, _}]
		= parse(<<"[A footnote&#44; &#34;with an image&#34; image:smallnew.png[]]">>),
	ok.
-endif.

%% Asciidoc User Guide 12
block_title(St) ->
	%% A block title line begins with a period and is followed by the title text.
	<<$., Title0/bits>> = read_line(St),
	Ann = ann(St),
	Title = trim(Title0),
	{block_title, #{}, Title, Ann}.

-ifdef(TEST).
block_title_test() ->
	%% Valid.
	[{block_title, _, <<"Notes">>, _}] = parse(<<".Notes">>),
	[{block_title, _, <<"Notes">>, _}] = parse(<<".Notes   ">>),
	%% Invalid.
	?NOT(block_title, parse(<<". Notes">>)),
	ok.
-endif.

%% Asciidoc User Guide 15.2
%%
%% An indented paragraph is implicitly a literal paragraph.
%% The [literal] style on a normal paragraph is handled later
%% by the transform pass.
literal_para(St) ->
	%% First line must be indented by one or more spaces or tabs.
	<<C, _/bits>> = Line0 = read_line(St),
	true = ?IS_WS(C),
	Ann = ann(St),
	%% Keep every indented line, then strip the minimum indent.
	%% A later line may be indented less than the first.
	Lines = literal_lines(St, [trim(Line0, trailing)]),
	{literal_paragraph, #{<<"subs">> => <<"verbatim">>},
		literal_text(Lines), Ann}.

literal_lines(St=#state{reader=ReaderPid}, Acc) ->
	Ln = asciideck_reader:get_position(ReaderPid),
	case read_line(St) of
		eof ->
			lists:reverse(Acc);
		Line0 ->
			case trim(Line0) of
				<<>> ->
					asciideck_reader:set_position(ReaderPid, Ln),
					lists:reverse(Acc);
				_ ->
					<<C, _/bits>> = Line0,
					case ?IS_WS(C) of
						true ->
							literal_lines(St, [trim(Line0, trailing)|Acc]);
						false ->
							asciideck_reader:set_position(ReaderPid, Ln),
							lists:reverse(Acc)
					end
			end
	end.

%% Default tab size is 8. Tabs are expanded before the minimum
%% indent is measured, so a tab and spaces agree on one column.
%% A column is one Unicode code point, as in str.expandtabs.
literal_text(Lines) ->
	Expanded = [expand_tabs(Line, 0, <<>>) || Line <- Lines],
	Min = lists:min([count_indent(Line) || Line <- Expanded]),
	join_lines([strip_n(Line, Min) || Line <- Expanded]).

expand_tabs(<<$\t, Rest/bits>>, Col, Acc) ->
	N = 8 - (Col rem 8),
	expand_tabs(Rest, Col + N, <<Acc/binary, (spaces(N))/binary>>);
expand_tabs(<<C/utf8, Rest/bits>>, Col, Acc) ->
	expand_tabs(Rest, Col + 1, <<Acc/binary, C/utf8>>);
expand_tabs(<<C, Rest/bits>>, Col, Acc) ->
	expand_tabs(Rest, Col + 1, <<Acc/binary, C>>);
expand_tabs(<<>>, _, Acc) ->
	Acc.

spaces(N) ->
	binary:copy(<<" ">>, N).

count_indent(<<$\s, Rest/bits>>) ->
	1 + count_indent(Rest);
count_indent(_) ->
	0.

strip_n(<<$\s, Rest/bits>>, N) when N > 0 ->
	strip_n(Rest, N - 1);
strip_n(Line, _) ->
	Line.

join_lines([Line]) ->
	Line;
join_lines([Line|Rest]) ->
	<<Line/binary, $\n, (join_lines(Rest))/binary>>.

-ifdef(TEST).
literal_para_test() ->
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"Consul *necessitatibus* per id,">>, _}]
		= parse(<<"  Consul *necessitatibus* per id,">>),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"Consul *necessitatibus* per id,\n"
		  "consetetur, eu pro everti postulant\n"
		  "homero verear ea mea, qui.">>, _}] = parse(<<
			"  Consul *necessitatibus* per id,\n"
			"  consetetur, eu pro everti postulant\n"
			"  homero verear ea mea, qui.">>),
	%% Tabs also count as indentation.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>}, <<"Hello!">>, _}]
		= parse(<<"\tHello!">>),
	%% Relative indentation is preserved, including a shorter later line.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"foo() ->\n    ok.">>, _}] = parse(<<
			"  foo() ->\n"
			"      ok.">>),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"  foo\nbar\n  baz">>, _}] = parse(<<
			"    foo\n"
			"  bar\n"
			"    baz">>),
	%% A tab advances to the next column that is a multiple of 8.
	%% Eight spaces and a tab are the same indent. A tab after
	%% four spaces also stops at column 8.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"foo\n    bar">>, _}] = parse(<<
			"\tfoo\n"
			"\t    bar">>),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"foo\nbar">>, _}] = parse(<<
			"\tfoo\n"
			"        bar">>),
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"foo\nbar">>, _}] = parse(<<
			"    \tfoo\n"
			"        bar">>),
	%% A tab in the text also stops at the next column.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"a     b">>, _}] = parse(<<"  a\tb">>),
	%% The column is a code point. U+00E9 is two bytes; the tab
	%% is at column 6 and expands to two spaces.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"caf", 195, 169, "  X">>, _}] = parse(<<
			"  caf", 195, 169, "\tX">>),
	%% U+20AC is three bytes. The tab is at column 2.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<226, 130, 172, "      X">>, _}] = parse(<<
			" ", 226, 130, 172, "\tX">>),
	%% Indented // and + are literal text, not comments or continuations.
	[{literal_paragraph, #{<<"subs">> := <<"verbatim">>},
		<<"x = 1\n// increment\nx = x + 1\n+">>, _}] = parse(<<
			"  x = 1\n"
			"  // increment\n"
			"  x = x + 1\n"
			"  +">>),
	%% A non-indented line starts a new paragraph.
	[
		{literal_paragraph, _, <<"Hello!">>, _},
		{paragraph, _, <<"World!">>, _}
	] = parse(<<"  Hello!\nWorld!">>),
	%% Must actually be indented.
	?NOT(literal_paragraph, parse(<<"Hello!">>)),
	ok.
-endif.

%% Asciidoc User Guide 15.4
admonition_para(St) ->
	Line0 = read_line(St),
	Ann = ann(St),
	{Style, Para0} = admonition_style(Line0),
	Para1 = trim(Para0),
	%% Admonition paragraphs end like normal paragraphs.
	Para = read_while(St, fun fold_para/1, Para1),
	{admonition, #{style => Style}, Para, Ann}.

admonition_style(<<"NOTE:", C, R/bits>>) when ?IS_WS(C) -> {<<"NOTE">>, R};
admonition_style(<<"TIP:", C, R/bits>>) when ?IS_WS(C) -> {<<"TIP">>, R};
admonition_style(<<"IMPORTANT:", C, R/bits>>) when ?IS_WS(C) -> {<<"IMPORTANT">>, R};
admonition_style(<<"WARNING:", C, R/bits>>) when ?IS_WS(C) -> {<<"WARNING">>, R};
admonition_style(<<"CAUTION:", C, R/bits>>) when ?IS_WS(C) -> {<<"CAUTION">>, R}.

-ifdef(TEST).
admonition_para_test() ->
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _}]
		= parse(<<"NOTE: This is a note.">>),
	[{admonition, #{style := <<"TIP">>}, <<"This is a tip.">>, _}]
		= parse(<<"TIP: This is a tip.">>),
	[{admonition, #{style := <<"IMPORTANT">>}, <<"This is important.">>, _}]
		= parse(<<"IMPORTANT: This is important.">>),
	[{admonition, #{style := <<"WARNING">>}, <<"This is a warning.">>, _}]
		= parse(<<"WARNING: This is a warning.">>),
	[{admonition, #{style := <<"CAUTION">>}, <<"This is a caution.">>, _}]
		= parse(<<"CAUTION: This is a caution.">>),
	%% Extra whitespace after the prefix is trimmed.
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _}]
		= parse(<<"NOTE:   This is a note.">>),
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.">>, _}]
		= parse(<<"NOTE:\tThis is a note.">>),
	%% Multi-line paragraph body.
	[{admonition, #{style := <<"NOTE">>}, <<"This is a note.\nIt spans two lines.">>, _}]
		= parse(<<"NOTE: This is a note.\nIt spans two lines.">>),
	%% A space after the colon is required.
	?NOT(admonition, parse(<<"NOTE:This is a note.">>)),
	%% The style must be uppercase and hard against the left margin.
	?NOT(admonition, parse(<<"Note: This is a note.">>)),
	?NOT(admonition, parse(<<"note: This is a note.">>)),
	?NOT(admonition, parse(<<" NOTE: This is a note.">>)),
	?NOT(admonition, parse(<<"NOTES: This is a note.">>)),
	ok.
-endif.

%% Asciidoc User Guide 15.1
para(St) ->
	%% Paragraph must be hard against the left margin.
	<<C, _/bits>> = Para0 = read_line(St),
	Ann = ann(St),
	false = ?IS_WS(C),
	Para1 = trim(Para0),
	%% Paragraph ends at blank line, end of file or start of delimited block or list.
	Para = read_while(St, fun fold_para/1, Para1),
	{paragraph, #{}, Para, Ann}.

fold_para(eof) ->
	done;
fold_para(Line) ->
	case ends_paragraph(Line) of
		true -> done;
		false -> {more, Line}
	end.

%% Asciidoc User Guide 15.1: blank line, end of file, delimited
%% block, or list. An attribute list does too, so [NOTE] can
%% follow the previous paragraph. Comment lines already did.
ends_paragraph(Line) ->
	case trim(Line) of
		<<>> -> true;
		<<"+">> -> true;
		<<"//", _/bits>> -> true;
		_ ->
			attribute_list_line(Line)
				orelse delimited_line(Line)
				orelse list_line(Line)
	end.

attribute_list_line(<<$[, Rest/bits>>) ->
	case trim(Rest) of
		<<>> -> false;
		Trimmed -> binary:last(Trimmed) =:= $]
	end;
attribute_list_line(_) ->
	false.

delimited_line(Line0) ->
	Line = trim(Line0, trailing),
	case Line of
		<<"--">> ->
			true;
		<<$|, $=, $=, $=, Rest/bits>> ->
			same_char(Rest, $=);
		<<C, C, C, C, Rest/bits>> when C =:= $-; C =:= $.; C =:= $*;
				C =:= $/; C =:= $+; C =:= $_; C =:= $= ->
			same_char(Rest, C);
		_ ->
			false
	end.

same_char(<<>>, _) ->
	true;
same_char(<<C, Rest/bits>>, C) ->
	same_char(Rest, C);
same_char(<<_, _/bits>>, _) ->
	false.

list_line(Line0) ->
	Line = trim(Line0),
	case Line of
		<<"-", C, _/bits>> when ?IS_WS(C) -> true;
		<<"*", C, _/bits>> when ?IS_WS(C) -> true;
		<<"**", C, _/bits>> when ?IS_WS(C) -> true;
		<<"***", C, _/bits>> when ?IS_WS(C) -> true;
		<<"****", C, _/bits>> when ?IS_WS(C) -> true;
		<<"*****", C, _/bits>> when ?IS_WS(C) -> true;
		<<".", C, _/bits>> when ?IS_WS(C) -> true;
		<<"..", C, _/bits>> when ?IS_WS(C) -> true;
		<<"...", C, _/bits>> when ?IS_WS(C) -> true;
		<<"....", C, _/bits>> when ?IS_WS(C) -> true;
		<<".....", C, _/bits>> when ?IS_WS(C) -> true;
		_ ->
			labeled_line(Line) orelse is_callout_line(Line0)
	end.

labeled_line(Line) ->
	try find_labeled_list(Line) of
		{Label, _, _} -> trim(Label) =/= <<>>
	catch _:_ ->
		false
	end.

-ifdef(TEST).
para_test() ->
	LoremIpsum = <<
		"Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n"
		"sed do eiusmod tempor incididunt ut labore et dolore\n"
		"magna aliqua. Ut enim ad minim veniam, quis nostrud\n"
		"exercitation ullamco laboris nisi ut aliquip ex ea\n"
		"commodo consequat. Duis aute irure dolor in reprehenderit\n"
		"in voluptate velit esse cillum dolore eu fugiat nulla\n"
		"pariatur. Excepteur sint occaecat cupidatat non proident,\n"
		"sunt in culpa qui officia deserunt mollit anim id est laborum."
	>>,
	%% Paragraph followed by end of file.
	[{paragraph, _, LoremIpsum, _}] = parse(<< LoremIpsum/binary, "\n">>),
	%% Paragraph followed by end of file with no trailing line break..
	[{paragraph, _, LoremIpsum, _}] = parse(LoremIpsum),
	%% Paragraph followed by list continuation.
	[{paragraph, _, LoremIpsum, _}, {list_item_continuation, _, _, _}]
		= parse(<<LoremIpsum/binary, "\n+">>),
	%% Paragraph followed by comment.
	[{paragraph, _, LoremIpsum, _}, {comment_line, _, <<"@todo Double check.">>, _}]
		= parse(<<LoremIpsum/binary, "\n// @todo Double check.">>),
	%% Two paragraphs.
	[{paragraph, _, LoremIpsum, _}, {paragraph, _, LoremIpsum, _}]
		= parse(<<
			LoremIpsum/binary,
			"\n\n",
			LoremIpsum/binary >>),
	ok.
-endif.

%% Control functions.

oneof([], St=#state{reader=ReaderPid}) ->
	throw({error, St, sys:get_state(ReaderPid)});
oneof([Parse|Tail], St=#state{reader=ReaderPid}) ->
	Ln = asciideck_reader:get_position(ReaderPid),
	try
		Parse(St)
	catch _:_ ->
		asciideck_reader:set_position(ReaderPid, Ln),
		oneof(Tail, St)
	end.

skip(Parse, St=#state{reader=ReaderPid}) ->
	Ln = asciideck_reader:get_position(ReaderPid),
	try
		_ = Parse(St),
		skip(Parse, St)
	catch _:_ ->
		asciideck_reader:set_position(ReaderPid, Ln),
		ok
	end.

%% Line functions.

read_line(#state{reader=ReaderPid}) ->
	asciideck_reader:read_line(ReaderPid).

read_while(St, F, Acc) ->
	read_while(St, F, Acc, drop).

read_while(St, F, Acc, Keep) ->
	do_read_while(St, F, start_acc(Acc), Keep).

do_read_while(St=#state{reader=ReaderPid}, F, Acc, Keep) ->
	Ln = asciideck_reader:get_position(ReaderPid),
	case F(read_line(St)) of
		done ->
			asciideck_reader:set_position(ReaderPid, Ln),
			finish_lines(Acc);
		{more, Line} ->
			do_read_while(St, F, append_line(Acc, Line, Keep), Keep)
	end.

%% <<>> means both "nothing read" and "an empty line", so the
%% accumulator uses none until the first line is kept. drop still
%% throws away leading empty lines. keep remembers them and emits
%% one leading newline per empty line once real text follows.
start_acc(<<>>) ->
	none;
start_acc(Acc) ->
	Acc.

append_line(none, <<>>, drop) ->
	none;
append_line(none, <<>>, keep) ->
	{pending, 1};
append_line(none, Line, _) ->
	Line;
append_line({pending, N}, <<>>, keep) ->
	{pending, N + 1};
append_line({pending, N}, Line, _) ->
	<<(binary:copy(<<$\n>>, N))/binary, Line/binary>>;
append_line(Acc, Line, _) when is_binary(Acc) ->
	<<Acc/binary, $\n, Line/binary>>.

finish_lines(none) ->
	<<>>;
finish_lines({pending, _}) ->
	<<>>;
finish_lines(Bin) when is_binary(Bin) ->
	Bin.

%% The reader position is the next line to be read.
%% line_offset shifts a nested slice back onto the file line.
ann(#state{reader=ReaderPid, line_offset=Offset}) ->
	#{line => asciideck_reader:get_position(ReaderPid) - 1 + Offset}.

trim(Line) ->
	trim(Line, both).

trim(Line, Direction) ->
	Regex = case Direction of
		both -> "^[ \\t\\r\\n]+|[ \\t\\r\\n]+$";
		trailing -> "[ \\t\\r\\n]+$"
	end,
	iolist_to_binary(re:replace(Line, Regex, <<>>, [global])).

repeats(<<>>, _) -> ok;
repeats(<<C, Rest/bits>>, C) -> repeats(Rest, C).

while(F, Bin) ->
	while(Bin, F, <<>>).

while(<<>>, _, Acc) ->
	{Acc, <<>>};
while(<<C, R/bits>>, F, Acc) ->
	case F(C) of
		true -> while(R, F, <<Acc/binary, C>>);
		false -> {Acc, <<C, R/bits>>}
	end.
