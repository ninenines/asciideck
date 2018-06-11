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

%% @todo Temporary export. Move somewhere else.
-export([trim/1]).
-export([trim/2]).
-export([while/2]).

-type ast() :: list(). %% @todo

-record(state, {
	reader :: pid()
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
	%% @todo Might want to start it supervised.
	%% @todo Might want to stop it also.
	{ok, ReaderPid} = asciideck_line_reader:start_link(Data),
	parse(ReaderPid);
parse(Data) when is_list(Data) ->
	parse(iolist_to_binary(Data));
parse(ReaderPid) when is_pid(ReaderPid) ->
	blocks(#state{reader=ReaderPid}).

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
		fun long_section_title/1,
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

%% Asciidoc User Guide 11.1
long_section_title(St) ->
	%% Title must be hard against the left margin.
	<<C, _/bits>> = Title0 = read_line(St),
	Ann = ann(St),
	false = ?IS_WS(C),
	Title = trim(Title0),
	%% Read the underline.
	{Level, Char, Underline0} = case read_line(St) of
		U = <<"=", _/bits >> -> {0, $=, U};
		U = <<"-", _/bits >> -> {1, $-, U};
		U = <<"~", _/bits >> -> {2, $~, U};
		U = <<"^", _/bits >> -> {3, $^, U};
		U = <<"+", _/bits >> -> {4, $+, U}
	end,
	Underline = trim(Underline0, trailing),
	%% Underline must be the same character repeated over the entire line.
	repeats(Underline, Char),
	%% Underline must be the same size as the title, +/- 2 characters.
	TLen = byte_size(Title),
	ULen = byte_size(Underline),
	true = (TLen >= ULen - 2) andalso (TLen =< ULen + 2),
	%% Good!
	{section_title, #{level => Level}, Title, Ann}.

-ifdef(TEST).
long_section_title_test() ->
	%% Same amount of characters for the underline.
	[{section_title, #{level := 0}, <<"Document Title (level 0)">>, _}] = parse(<<
		"Document Title (level 0)\n"
		"========================">>),
	[{section_title, #{level := 1}, <<"Section Title (level 1)">>, _}] = parse(<<
		"Section Title (level 1)\n"
		"-----------------------">>),
	[{section_title, #{level := 2}, <<"Section Title (level 2)">>, _}] = parse(<<
		"Section Title (level 2)\n"
		"~~~~~~~~~~~~~~~~~~~~~~~">>),
	[{section_title, #{level := 3}, <<"Section Title (level 3)">>, _}] = parse(<<
		"Section Title (level 3)\n"
		"^^^^^^^^^^^^^^^^^^^^^^^">>),
	[{section_title, #{level := 4}, <<"Section Title (level 4)">>, _}] = parse(<<
		"Section Title (level 4)\n"
		"+++++++++++++++++++++++">>),
	%% A shorter title to confirm we are not cheating.
	[{section_title, #{level := 0}, <<"Hello!">>, _}] = parse(<<
		"Hello!\n"
		"======">>),
	%% Underline can be +/- 2 characters.
	[{section_title, #{level := 0}, <<"Hello!">>, _}] = parse(<<
		"Hello!\n"
		"====">>),
	[{section_title, #{level := 0}, <<"Hello!">>, _}] = parse(<<
		"Hello!\n"
		"=====">>),
	[{section_title, #{level := 0}, <<"Hello!">>, _}] = parse(<<
		"Hello!\n"
		"=======">>),
	[{section_title, #{level := 0}, <<"Hello!">>, _}] = parse(<<
		"Hello!\n"
		"========">>),
	%% Underline too short/long results in a different block.
	?NOT(section_title, parse(<<
		"Hello!\n"
		"===">>)),
	?NOT(section_title, parse(<<
		"Hello!\n"
		"=========">>)),
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
	%% Long snippet from the Asciidoc User Guide, minus literal paragraph.
	%% @todo Add the literal paragraph back once they are implemented.
	[
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"In">>},
			[{paragraph, _, <<>>, _}], _},
		{list_item, #{type := labeled, separator := <<"::">>, label := <<"Lorem">>},
			[{paragraph, _, <<"Fusce euismod commodo velit.">>, _}], _},
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
		%% @todo Add literal paragraph back here.
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
-spec callout_list(_) -> no_return().
callout_list(St) -> throw({not_implemented, St}). %% @todo

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
	%% A list item ends on end of file, empty line or when a new list starts.
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
			try find_labeled_list(Line) of
				{_, _, _} -> done
			catch _:_ ->
				{more, Line}
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
	ok.
-endif.

%% Asciidoc User Guide 17.7
list_item_continuation(St) ->
	%% Continuations are a single + hard against the left margin.
	<<$+, Whitespace/bits>> = read_line(St),
	<<>> = trim(Whitespace),
	{list_item_continuation, #{}, <<>>, ann(St)}.

-ifdef(TEST).
list_item_continuation_test() ->
	[{list_item_continuation, _, _, _}] = parse(<<"+">>),
	[{list_item_continuation, _, _, _}] = parse(<<"+   ">>),
	[{list_item_continuation, _, _, _}] = parse(<<"+\n">>),
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
	delimited_block(St, passthrough_block, $+).

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
	Block = read_while(St, fun(L) -> fold_delimited_block(L, Four, Char) end, <<>>),
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
	ok.
-endif.

%% Asciidoc User Guide 16.10
-spec open_block(_) -> no_return().
open_block(St) -> throw({not_implemented, St}). %% @todo

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
-spec attribute_entry(_) -> no_return().
attribute_entry(St) -> throw({not_implemented, St}). %% @todo

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
-spec literal_para(_) -> no_return().
literal_para(St) -> throw({not_implemented, St}). %% @todo

%% Asciidoc User Guide 15.4
-spec admonition_para(_) -> no_return().
admonition_para(St) -> throw({not_implemented, St}). %% @todo

%% Asciidoc User Guide 15.1
para(St) ->
	%% Paragraph must be hard against the left margin.
	<<C, _/bits>> = Para0 = read_line(St),
	Ann = ann(St),
	%% @todo Uncomment this line once everything else has been implemented.
	_ = ?IS_WS(C), % false = ?IS_WS(C),
	Para1 = trim(Para0),
	%% Paragraph ends at blank line, end of file or start of delimited block or list.
	Para = read_while(St, fun fold_para/1, Para1),
	{paragraph, #{}, Para, Ann}.

fold_para(eof) ->
	done;
fold_para(Line0) ->
	case trim(Line0) of
		<<>> -> done;
		<<"+">> -> done;
		%% @todo Detect delimited block or list.
		Line -> {more, Line}
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

read_while(St=#state{reader=ReaderPid}, F, Acc) ->
	Ln = asciideck_reader:get_position(ReaderPid),
	case F(read_line(St)) of
		done ->
			asciideck_reader:set_position(ReaderPid, Ln),
			Acc;
		{more, Line} ->
			case Acc of
				<<>> -> read_while(St, F, Line);
				_ -> read_while(St, F, <<Acc/binary, $\n, Line/binary>>)
			end
	end.

ann(#state{reader=ReaderPid}) ->
	#{line => asciideck_reader:get_position(ReaderPid)}.

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
