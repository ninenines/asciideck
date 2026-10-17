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

%% This pass parses and builds a table from the contents
%% of a table block.
%%
%% Asciidoc User Guide 23
%%
%% @todo Rows and cells are currently not annotated.
-module(asciideck_tables_pass).

-export([run/1]).

run([]) ->
	[];
run([Table={table, _, _, _}|Tail]) ->
	[table(Table)|run(Tail)];
run([{open_block, Attrs, Children, Ann}|Tail]) ->
	[{open_block, Attrs, run(Children), Ann}|run(Tail)];
run([{sidebar_block, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[{sidebar_block, Attrs, run(Children), Ann}|run(Tail)];
run([{quote_block, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[{quote_block, Attrs, run(Children), Ann}|run(Tail)];
run([{example_block, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[{example_block, Attrs, run(Children), Ann}|run(Tail)];
%% A NOTE open block is an admonition by this pass. Its body is
%% a list of blocks, unlike an admonition paragraph.
run([{admonition, Attrs, Children, Ann}|Tail]) when is_list(Children) ->
	[{admonition, Attrs, run(Children), Ann}|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

table({table, Attrs, Contents, Ann}) ->
	%% The body starts on the line after the opening delimiter.
	Line = maps:get(line, Ann, 0) + 1,
	{Cells, NumCols, ColSpecs} = parse_table(Contents, Attrs, Line),
	Children = rows(Cells, NumCols, ColSpecs, header_row(Attrs)),
	Attrs1 = case ColSpecs of
		[] -> Attrs;
		_ -> Attrs#{columns => ColSpecs}
	end,
	{table, Attrs1, Children, Ann}.

-ifdef(TEST).
table_test() ->
	{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"1">>, _}], _},
			{cell, _, [{paragraph, _, <<"2">>, _}], _},
			{cell, _, [{paragraph, _, <<"A">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"3">>, _}], _},
			{cell, _, [{paragraph, _, <<"4">>, _}], _},
			{cell, _, [{paragraph, _, <<"B">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"5">>, _}], _},
			{cell, _, [{paragraph, _, <<"6">>, _}], _},
			{cell, _, [{paragraph, _, <<"C">>, _}], _}
		], _}
	], _} = table({table, #{}, <<
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C">>, #{line => 1}}),
	ok.

sidebar_block_test() ->
	[{sidebar_block, _, [
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"1">>, _}], _},
				{cell, _, [{paragraph, _, <<"2">>, _}], _}
			], _}
		], _}
	], _}] = run([
		{sidebar_block, #{}, [
			{table, #{}, <<"|1 |2">>, #{line => 2}}
		], #{line => 1}}
	]),
	ok.

quote_block_test() ->
	[{quote_block, _, [
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"1">>, _}], _},
				{cell, _, [{paragraph, _, <<"2">>, _}], _}
			], _}
		], _}
	], _}] = run([
		{quote_block, #{}, [
			{table, #{}, <<"|1 |2">>, #{line => 2}}
		], #{line => 1}}
	]),
	ok.

example_block_test() ->
	[{example_block, _, [
		{table, _, [
			{row, _, [
				{cell, _, [{paragraph, _, <<"1">>, _}], _},
				{cell, _, [{paragraph, _, <<"2">>, _}], _}
			], _}
		], _}
	], _}] = run([
		{example_block, #{}, [
			{table, #{}, <<"|1 |2">>, #{line => 2}}
		], #{line => 1}}
	]),
	ok.
-endif.

%% If the cols attribute is not specified, the number of
%% columns is the number of columns occupied on the first line.
%% A colspan or a repeated cell counts as more than one column.
%% Leading empty lines were kept in the body so they count. They are
%% not a row, and must not set the column count.
parse_table(<<$\n, Rest/bits>>, Attrs, Line) ->
	parse_table(Rest, Attrs, Line + 1);
%% cols= gives the column count, so the body is one sequence of cells.
%% A cell on the first line may continue on the next line.
parse_table(Contents, #{<<"cols">> := Cols}, Line) ->
	{Count, Specs} = column_specs(Cols),
	{parse_cells(Contents, [], Line), Count, Specs};
parse_table(Contents, _Attrs, Line) ->
	{Cells, AutoCols} = parse_body(Contents, Line),
	{Cells, AutoCols, []}.

%% The first line only sets the column count. The body is one
%% sequence of cells, so a cell may continue on the next line.
parse_body(Contents, Line) ->
	{parse_cells(Contents, [], Line), columns_on_first_line(Contents, Line)}.

columns_on_first_line(Contents, Line) ->
	First = case binary:split(Contents, <<$\n>>) of
		[Line0] ->
			Line0;
		[Line0, _] ->
			Line0
	end,
	columns_occupied(parse_cells(First, [], Line)).

columns_occupied(Cells) ->
	lists:sum([maps:get(colspan, Attrs, 1) || {cell, Attrs, _, _} <- Cells]).

%% The cols attribute is one specifier per column, or an integer count.
column_specs(Cols) ->
	try binary_to_integer(Cols) of
		N when N > 0 ->
			{N, lists:duplicate(N, default_column())};
		_ ->
			{0, []}
	catch _:_ ->
		Parts = [parse_specs(trim_ws(Spec))
			|| Spec <- binary:split(Cols, <<$,>>, [global])],
		Specs = lists:append([lists:duplicate(maps:get(multiplier, Spec, 1),
			maps:remove(multiplier, Spec)) || Spec <- Parts]),
		{length(Specs), Specs}
	end.

default_column() ->
	#{
		multiplier => 1,
		horizontal => left,
		vertical => top,
		width => 1,
		width_unit => proportional,
		style => default
	}.

%% Column styles are not applied to a header row. Alignment still is.
header_row(#{<<"options">> := Opts}) ->
	lists:member(<<"header">>, [trim_ws(Part)
		|| Part <- binary:split(Opts, <<$,>>, [global])]);
header_row(_) ->
	false.

-ifdef(TEST).
num_cols_test_() ->
	Tests = [
		{<<"4">>, 4},
		{<<">s,^m,e">>, 3},
		{<<"3,^2,^2,10">>, 4},
		{<<"^1,4*2">>, 5},
		{<<"e,m,^,>s">>, 4},
		{<<"2<d,2*,4d,>">>, 5},
		{<<"4*<">>, 4},
		{<<"3*.^">>, 3},
		{<<"2*,.>">>, 3},
		{<<".<,.^,.>">>, 3},
		{<<".<,.^,^.>">>, 3}
	],
	[{V, fun() -> {R, _} = column_specs(V) end} || {V, R} <- Tests].
-endif.

%% Asciidoc User Guide 23.4
%%
%% [<multiplier>*][<horizontal>][.<vertical>][<width>][<style>]
parse_specs(Bin0) ->
	{ok, Bin1, Spec1} = parse_specs_multiplier(Bin0, #{}),
	%% Width and alignment positions may be switched.
	{ok, Bin4, Spec4} = case Bin1 of
		<<C, _/bits>> when C >= $0, C =< $9 ->
			{ok, Bin2, Spec2} = parse_specs_width(Bin1, Spec1),
			{ok, Bin3, Spec3} = parse_specs_horizontal(Bin2, Spec2),
			parse_specs_vertical(Bin3, Spec3);
		_ ->
			{ok, Bin2, Spec2} = parse_specs_horizontal(Bin1, Spec1),
			{ok, Bin3, Spec3} = parse_specs_vertical(Bin2, Spec2),
			parse_specs_width(Bin3, Spec3)
	end,
	parse_specs_style(Bin4, Spec4).

parse_specs_multiplier(Bin, Spec) ->
	case binary:split(Bin, <<"*">>) of
		[_] ->
			{ok, Bin, Spec#{multiplier => 1}};
		[Multiplier, Rest] ->
			{ok, Rest, Spec#{multiplier => binary_to_integer(Multiplier)}}
	end.

parse_specs_horizontal(Bin, Spec) ->
	case Bin of
		<<"<", Rest/bits>> -> {ok, Rest, Spec#{horizontal => left}};
		<<"^", Rest/bits>> -> {ok, Rest, Spec#{horizontal => center}};
		<<">", Rest/bits>> -> {ok, Rest, Spec#{horizontal => right}};
		_ -> {ok, Bin, Spec#{horizontal => left}}
	end.

parse_specs_vertical(Bin, Spec) ->
	case Bin of
		<<".<", Rest/bits>> -> {ok, Rest, Spec#{vertical => top}};
		<<".^", Rest/bits>> -> {ok, Rest, Spec#{vertical => middle}};
		<<".>", Rest/bits>> -> {ok, Rest, Spec#{vertical => bottom}};
		_ -> {ok, Bin, Spec#{vertical => top}}
	end.

parse_specs_width(Bin, Spec) ->
	case binary:split(Bin, <<"%">>) of
		[_] ->
			case binary_take_while_integer(Bin, <<>>) of
				{<<>>, _} ->
					{ok, Bin, Spec#{width => 1, width_unit => proportional}};
				{Width, Rest} ->
					{ok, Rest, Spec#{width => binary_to_integer(Width), width_unit => proportional}}
			end;
		[Percent, Rest] ->
			{ok, Rest, Spec#{width => binary_to_integer(Percent), width_unit => percent}}
	end.

binary_take_while_integer(<<C, R/bits>>, Acc) when C >= $0, C =< $9 ->
	binary_take_while_integer(R, <<Acc/binary, C>>);
binary_take_while_integer(Rest, Acc) ->
	{Acc, Rest}.

parse_specs_style(<<>>, Spec) ->
	Spec#{style => default};
parse_specs_style(Bin, Spec) ->
	Style = parse_specs_match_style(Bin, [
		<<"default">>, <<"emphasis">>, <<"monospaced">>, <<"strong">>,
		<<"header">>, <<"asciidoc">>, <<"literal">>, <<"verse">>
	]),
	Spec#{style => Style}.

parse_specs_match_style(Prefix, [Style|Tail]) ->
	case binary:longest_common_prefix([Prefix, Style]) of
		0 -> parse_specs_match_style(Prefix, Tail);
		_ -> binary_to_atom(Style, latin1)
	end.

-ifdef(TEST).
parse_specs_test_() ->
	Res = fun(Override) ->
		maps:merge(#{
			multiplier => 1,
			horizontal => left,
			vertical => top,
			width => 1,
			width_unit => proportional,
			style => default
		}, Override)
	end,
	Tests = [
		{<<"3">>, Res(#{width => 3})},
		{<<"10">>, Res(#{width => 10})},
		{<<">s">>, Res(#{horizontal => right, style => strong})},
		{<<"^m">>, Res(#{horizontal => center, style => monospaced})},
		{<<"e">>, Res(#{style => emphasis})},
		{<<"^2">>, Res(#{horizontal => center, width => 2})},
		{<<"4*2">>, Res(#{multiplier => 4, width => 2})},
		{<<"^">>, Res(#{horizontal => center})},
		{<<">">>, Res(#{horizontal => right})},
		{<<"2<h">>, Res(#{width => 2, horizontal => left, style => header})},
		{<<"2*">>, Res(#{multiplier => 2})},
		{<<"4*<">>, Res(#{multiplier => 4, horizontal => left})},
		{<<"3*.^">>, Res(#{multiplier => 3, vertical => middle})},
		{<<".>">>, Res(#{vertical => bottom})}
	],
	[{V, fun() -> R = parse_specs(V) end} || {V, R} <- Tests].
-endif.

parse_cells(Contents, Acc, Line) ->
	Cells = split_cells(Contents),%binary:split(Contents, [<<$|>>], [global]),
	do_parse_cells(Cells, Acc, Line).
	%% Split on |
	%% Look at the end of each element see if there's a cell specifier
	%% Add it as an attribute to the cell for now and consolidate
	%% when processing rows.

split_cells(Contents) ->
	split_cells(Contents, <<>>, []).

split_cells(<<>>, Cell, Acc) ->
	lists:reverse([Cell|Acc]);
split_cells(<<$\\, $|, R/bits>>, Cell, Acc) ->
	split_cells(R, <<Cell/binary, $|>>, Acc);
split_cells(<<$|, R/bits>>, Cell, Acc) ->
	split_cells(R, <<>>, [Cell|Acc]);
split_cells(<<C, R/bits>>, Cell, Acc) ->
	split_cells(R, <<Cell/binary, C>>, Acc).

%% Malformed table (no pipe before cell). Process it like it is a single cell.
do_parse_cells([Contents], Acc, _Line) ->
	%% @todo Annotations.
	lists:reverse([{cell, #{}, Contents, #{}}|Acc]);
%% Last cell. There is no following specifier to peel off.
do_parse_cells([Specs, Contents0], Acc, Line) ->
	CellLine = Line + newlines(Specs),
	Cell = make_cell(Specs, Contents0, CellLine),
	lists:reverse(add_cell(Cell, Acc));
%% A cell specifier prefixes the next '|' and sits in the tail of
%% this cell, after the last whitespace. Only a real specifier is
%% removed. Any other last word stays in the cell.
do_parse_cells([Specs, Contents0|Tail], Acc, Line) ->
	{NextSpecs, Contents1} = take_next_spec(Contents0),
	CellLine = Line + newlines(Specs),
	Cell = make_cell(Specs, Contents1, CellLine),
	NextLine = CellLine + newlines(Contents1),
	do_parse_cells([NextSpecs|Tail], add_cell(Cell, Acc), NextLine).

make_cell(Specs, Contents, Line) ->
	Attrs = case trim_ws(Specs) of
		<<>> -> #{};
		Token ->
			case parse_cell_spec(Token) of
				nomatch -> #{};
				Map -> Map
			end
	end,
	%% @todo Annotations. The raw text is kept until the cell style
	%% is known. Literal and verse need the original line breaks.
	{cell, Attrs, parse_cell(Contents, Line), #{raw => Contents}}.

%% '*' replicates the cell. Each copy occupies one column.
add_cell({cell, Attrs, Body, Ann}, Acc) ->
	N = maps:get(repeat, Attrs, 1),
	Cell = {cell, maps:remove(repeat, Attrs), Body, Ann},
	add_cell(Cell, N, Acc).

add_cell(_Cell, N, Acc) when N =< 0 ->
	Acc;
add_cell(Cell, N, Acc) ->
	add_cell(Cell, N - 1, [Cell|Acc]).

%% The specifier is only the last token when whitespace separates
%% it from the cell text. "|e|styled" is the text e, not a style.
take_next_spec(Contents) ->
	case last_token(Contents) of
		<<>> ->
			{<<>>, Contents};
		Token ->
			case parse_cell_spec(Token) of
				nomatch ->
					{<<>>, Contents};
				_ ->
					Len = byte_size(Contents) - byte_size(Token),
					<<Keep:Len/binary, _/bits>> = Contents,
					case spec_boundary(Keep) of
						true ->
							{Token, Keep};
						false ->
							{<<>>, Contents}
					end
			end
	end.

spec_boundary(<<>>) ->
	false;
spec_boundary(Keep) ->
	Len = byte_size(Keep) - 1,
	<<_:Len/binary, C>> = Keep,
	C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r.

last_token(Bin) ->
	last_token(Bin, <<>>).

last_token(<<>>, Token) ->
	Token;
last_token(<<C, Rest/bits>>, _) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
	last_token(Rest, <<>>);
last_token(<<C, Rest/bits>>, Token) ->
	last_token(Rest, <<Token/binary, C>>).

%% User Guide 23.5. The whole token must match.
%% [<span>*|+][<align>][<style>]
parse_cell_spec(Bin) ->
	case parse_span(Bin) of
		nomatch ->
			nomatch;
		{Rest, SpanAttrs} ->
			case parse_align(Rest) of
				nomatch ->
					nomatch;
				{Rest2, AlignAttrs} ->
					case parse_style(Rest2) of
						{<<>>, StyleAttrs} ->
							maps:merge(maps:merge(SpanAttrs, AlignAttrs), StyleAttrs);
						_ ->
							nomatch
					end
			end
	end.

%% A dot without a rowspan count is vertical alignment (".^"),
%% not a failed span. A dot after a colspan still needs digits.
parse_span(Bin0) ->
	{Col, Bin1} = take_digits(Bin0),
	case Bin1 of
		<<$., Rest/bits>> ->
			case take_digits(Rest) of
				{none, _} when Col =:= none ->
					{Bin0, #{}};
				{none, _} ->
					nomatch;
				{Row, Bin2} ->
					span_operator(Col, Row, Bin2, Bin0)
			end;
		_ ->
			span_operator(Col, none, Bin1, Bin0)
	end.

span_operator(Col, Row, Bin, Bin0) ->
	HasNum = (is_integer(Col) andalso Col > 0)
		orelse (is_integer(Row) andalso Row > 0),
	case Bin of
		<<$+, Rest/bits>> when HasNum ->
			{Rest, span_attrs(Col, Row)};
		<<$*, Rest/bits>> when HasNum ->
			{Rest, #{repeat => repeat_count(Col, Row)}};
		_ when HasNum =:= false ->
			{Bin0, #{}};
		_ ->
			nomatch
	end.

span_attrs(Col, Row) ->
	Attrs0 = case Col of
		N when is_integer(N), N > 1 -> #{colspan => N};
		_ -> #{}
	end,
	case Row of
		M when is_integer(M), M > 1 -> Attrs0#{rowspan => M};
		_ -> Attrs0
	end.

%% The repeat count is the colspan. A rowspan on '*' is not a copy count.
repeat_count(Col, _) when is_integer(Col), Col > 0 ->
	Col;
repeat_count(_, _) ->
	1.

take_digits(Bin) ->
	take_digits(Bin, <<>>).

take_digits(<<C, Rest/bits>>, Acc) when C >= $0, C =< $9 ->
	take_digits(Rest, <<Acc/binary, C>>);
take_digits(Rest, <<>>) ->
	{none, Rest};
take_digits(Rest, Acc) ->
	{binary_to_integer(Acc), Rest}.

parse_align(Bin) ->
	case Bin of
		<<C, Rest/bits>> when C =:= $<; C =:= $^; C =:= $> ->
			case parse_valign(Rest) of
				nomatch ->
					nomatch;
				{Rest2, VAttrs} ->
					{Rest2, VAttrs#{halign => halign_name(C)}}
			end;
		_ ->
			parse_valign(Bin)
	end.

parse_valign(<<$., C, Rest/bits>>) when C =:= $<; C =:= $^; C =:= $> ->
	{Rest, #{valign => valign_name(C)}};
parse_valign(<<$., _/bits>>) ->
	nomatch;
parse_valign(Bin) ->
	{Bin, #{}}.

halign_name($<) -> <<"left">>;
halign_name($^) -> <<"center">>;
halign_name($>) -> <<"right">>.

valign_name($<) -> <<"top">>;
valign_name($^) -> <<"middle">>;
valign_name($>) -> <<"bottom">>.

parse_style(<<>>) ->
	{<<>>, #{}};
parse_style(<<C>>) ->
	case style_name(C) of
		nomatch -> nomatch;
		Name -> {<<>>, #{style => Name}}
	end;
parse_style(_) ->
	nomatch.

style_name($d) -> <<"default">>;
style_name($e) -> <<"emphasis">>;
style_name($m) -> <<"monospaced">>;
style_name($s) -> <<"strong">>;
style_name($h) -> <<"header">>;
style_name($a) -> <<"asciidoc">>;
style_name($l) -> <<"literal">>;
style_name($v) -> <<"verse">>;
style_name(_) -> nomatch.

trim_ws(Bin) ->
	trim_right(trim_left(Bin)).

trim_left(<<C, Rest/bits>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
	trim_left(Rest);
trim_left(Bin) ->
	Bin.

trim_right(Bin) ->
	trim_right(Bin, byte_size(Bin)).

trim_right(Bin, Size) when Size > 0 ->
	Len = Size - 1,
	case Bin of
		<<Keep:Len/binary, C>> when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
			trim_right(Keep, Len);
		_ ->
			Bin
	end;
trim_right(Bin, _) ->
	Bin.

parse_cell(Contents, Line) ->
	asciideck_block_parser:parse(Contents, Line - 1).

newlines(Bin) ->
	newlines(Bin, 0).

newlines(<<$\n, Rest/bits>>, N) ->
	newlines(Rest, N + 1);
newlines(<<_, Rest/bits>>, N) ->
	newlines(Rest, N);
newlines(<<>>, N) ->
	N.

-ifdef(TEST).
parse_table_test() ->
	{[
		{cell, _, [{paragraph, _, <<"1">>, _}], _},
		{cell, _, [{paragraph, _, <<"2">>, _}], _},
		{cell, _, [{paragraph, _, <<"A">>, _}], _},
		{cell, _, [{paragraph, _, <<"3">>, _}], _},
		{cell, _, [{paragraph, _, <<"4">>, _}], _},
		{cell, _, [{paragraph, _, <<"B">>, _}], _},
		{cell, _, [{paragraph, _, <<"5">>, _}], _},
		{cell, _, [{paragraph, _, <<"6">>, _}], _},
		{cell, _, [{paragraph, _, <<"C">>, _}], _}
	], 3, []} = parse_table(<<
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C">>, #{}, 1),
	ok.

parse_table_escape_pipe_test() ->
	{[
		{cell, _, [{paragraph, _, <<"1">>, _}], _},
		{cell, _, [{paragraph, _, <<"2">>, _}], _},
		{cell, _, [{paragraph, _, <<"3 |4">>, _}], _},
		{cell, _, [{paragraph, _, <<"5">>, _}], _}
	], 2, []} = parse_table(<<
		"|1 |2\n"
		"|3 \\|4 |5">>, #{}, 1),
	ok.

%% User Guide 23.5. Spans, alignment, and styles.
cell_spec_test() ->
	[{table, _, [
		{row, _, [
			{cell, #{style := <<"emphasis">>}, [{paragraph, _, <<"1">>, _}], _},
			{cell, #{halign := <<"right">>, style := <<"strong">>},
				[{paragraph, _, <<"2">>, _}], _},
			{cell, #{halign := <<"center">>}, [{paragraph, _, <<"3">>, _}], _},
			{cell, #{halign := <<"right">>, style := <<"strong">>},
				[{paragraph, _, <<"4">>, _}], _}
		], _},
		{row, _, [
			{cell, #{halign := <<"center">>, style := <<"emphasis">>},
				[{paragraph, _, <<"5">>, _}], _},
			{cell, #{colspan := 2, rowspan := 2, halign := <<"center">>,
				valign := <<"middle">>, style := <<"monospaced">>},
				[{paragraph, _, <<"6">>, _}], _},
			{cell, #{rowspan := 3, halign := <<"left">>, valign := <<"bottom">>,
				style := <<"monospaced">>},
				[{paragraph, _, <<"7">>, _}], _}
		], _},
		{row, _, [
			{cell, #{halign := <<"center">>, style := <<"emphasis">>},
				[{paragraph, _, <<"8">>, _}], _}
		], _},
		{row, _, [
			{cell, #{style := <<"emphasis">>}, [{paragraph, _, <<"9">>, _}], _},
			{cell, #{colspan := 2, halign := <<"right">>, style := <<"monospaced">>},
				[{paragraph, _, <<"10">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"e,m,^,>s\"]\n"
		"|===\n"
		"|1 >s|2 |3 |4\n"
		"^|5 2.2+^.^|6 .3+<.>m|7\n"
		"^|8\n"
		"|9 2+>|10\n"
		"|===\n">>),
	%% A last word is not a specifier. A single style letter is.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"hello world">>, _}], _},
			{cell, _, [{paragraph, _, <<"next">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"the">>, _}], _},
			{cell, #{style := <<"strong">>}, [{paragraph, _, <<"styled">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|hello world|next\n"
		"|the s|styled\n"
		"|===\n">>),
	%% '*' repeats the cell. A short final row is dropped.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"1">>, _}], _},
			{cell, _, [{paragraph, _, <<"2">>, _}], _},
			{cell, _, [{paragraph, _, <<"2">>, _}], _},
			{cell, _, [{paragraph, _, <<"3">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|1 2*|2 |3\n"
		"|4 |5 |6\n"
		"|===\n">>),
	%% A colspan that does not fit is dropped and ends the row.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"1">>, _}], _},
			{cell, _, [{paragraph, _, <<"2">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"kept">>, _}], _},
			{cell, _, [{paragraph, _, <<"4">>, _}], _},
			{cell, _, [{paragraph, _, <<"5">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"3\"]\n"
		"|===\n"
		"|1 |2 2+|dropped |kept\n"
		"|4 |5 |6\n"
		"|===\n">>),
	%% Column styles skip the header row. Alignment does not.
	[{table, _, [
		{row, _, [
			{cell, H1, [{paragraph, _, <<"H1">>, _}], _},
			{cell, H2, [{paragraph, _, <<"H2">>, _}], _},
			{cell, H3, [{paragraph, _, <<"H3">>, _}], _}
		], _},
		{row, _, [
			{cell, #{halign := <<"right">>, style := <<"strong">>},
				[{paragraph, _, <<"1">>, _}], _},
			{cell, #{halign := <<"center">>, style := <<"monospaced">>},
				[{paragraph, _, <<"Item">>, _}], _},
			{cell, #{style := <<"emphasis">>}, [{paragraph, _, <<"Word">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\">s,^m,e\",options=\"header\"]\n"
		"|===\n"
		"|H1 |H2 |H3\n"
		"|1 |Item |Word\n"
		"|===\n">>),
	<<"right">> = maps:get(halign, H1),
	false = maps:is_key(style, H1),
	<<"center">> = maps:get(halign, H2),
	false = maps:is_key(style, H2),
	false = maps:is_key(style, H3),
	false = maps:is_key(halign, H3),
	%% Literal keeps the source lines. Verse keeps them and formats.
	[{table, _, [
		{row, _, [
			{cell, #{style := <<"literal">>},
				[{literal_block, #{<<"subs">> := <<"verbatim">>},
					<<"*one*\n*two*\n\n*three* ">>, _}], _},
			{cell, #{style := <<"verse">>},
				[{quote_block, #{<<"style">> := <<"verse">>}, [
					<<"verse ">>,
					{strong, _, <<"a">>, _},
					<<"\n">>,
					{strong, _, <<"b">>, _}
				], VerseAnn}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"l,v\"]\n"
		"|===\n"
		"|*one*\n"
		"*two*\n"
		"\n"
		"*three* |verse *a*\n"
		"*b*\n"
		"|===\n">>),
	false = maps:is_key(raw, VerseAnn),
	%% A vertical alignment on its own is a specifier.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"top">>, _}], _},
			{cell, #{valign := <<"middle">>}, [{paragraph, _, <<"mid">>, _}], _},
			{cell, #{valign := <<"bottom">>}, [{paragraph, _, <<"bot">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|top .^|mid .>|bot\n"
		"|===\n">>),
	%% A token with no whitespace before it is the cell text.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"^">>, _}], _},
			{cell, _, [{paragraph, _, <<"top">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"e">>, _}], _},
			{cell, _, [{paragraph, _, <<"styled">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|^|top\n"
		"|e|styled\n"
		"|===\n">>),
	%% No full row: the table remains, with its columns, and no cells.
	[{table, #{columns := [_, _, _]}, [], _}] = asciideck:parse(<<
		"[cols=\"3\"]\n"
		"|===\n"
		"|only |two\n"
		"|===\n">>),
	%% Without cols the first line sets the count. Later lines stay in the cell.
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"1">>, _}], _},
			{cell, _, [{paragraph, _, <<"2 second line of cell 2">>, _}], _}
		], _},
		{row, _, [
			{cell, _, [{paragraph, _, <<"3">>, _}], _},
			{cell, _, [{paragraph, _, <<"4">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|1 |2\n"
		"second line of cell 2\n"
		"|3 |4\n"
		"|===\n">>),
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"only cell more text">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"|===\n"
		"|only cell\n"
		"more text\n"
		"|===\n">>),
	%% A blank line is a paragraph break. Both paragraphs stay in the cell.
	[{table, _, [
		{row, _, [
			{cell, _, [
				{paragraph, _, <<"para one">>, _},
				{paragraph, _, <<"para two">>, _}
			], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"1\"]\n"
		"|===\n"
		"|para one\n"
		"\n"
		"para two\n"
		"|===\n">>),
	%% A rowspan on '*' does not copy the cell. The colspan does.
	[{table, _, [
		{row, _, [
			{cell, _, [], _},
			{cell, _, [{paragraph, _, <<"A">>, _}], _},
			{cell, _, [{paragraph, _, <<"B">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"3\"]\n"
		"|===\n"
		"| .2*|A |B\n"
		"|===\n">>),
	[{table, _, [
		{row, _, [
			{cell, _, [{paragraph, _, <<"A">>, _}], _},
			{cell, _, [{paragraph, _, <<"B">>, _}], _},
			{cell, _, [{paragraph, _, <<"C">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"3\"]\n"
		"|===\n"
		".2*|A |B\n"
		"|C |D |E\n"
		"|===\n">>),
	%% Spaces after commas in cols are not part of the specifier.
	[{table, _, [
		{row, _, [
			{cell, #{style := <<"emphasis">>}, [{paragraph, _, <<"1">>, _}], _},
			{cell, #{style := <<"monospaced">>}, [{paragraph, _, <<"2">>, _}], _}
		], _}
	], _}] = asciideck:parse(<<
		"[cols=\"e, m\"]\n"
		"|===\n"
		"|1 |2\n"
		"|===\n">>),
	ok.
-endif.

%% Cells fill the grid from left to right. A rowspan reserves its
%% columns on the following rows. A cell whose colspan does not fit
%% in the columns that are still free is dropped, and the row ends.
%% A trailing row that never fills is dropped. User Guide 23.5.
rows(_Cells, NumCols, _ColSpecs, _Header) when NumCols =< 0 ->
	[];
rows(Cells, NumCols, ColSpecs, Header) ->
	lists:reverse(layout(Cells, NumCols, ColSpecs, Header, [], [], 0, #{}, 0)).

layout(Cells, NumCols, ColSpecs, Header, Acc, RowAcc, Col, Reserved, RowIndex)
		when Col >= NumCols ->
	Row = {row, #{}, lists:reverse(RowAcc), #{}},
	layout(Cells, NumCols, ColSpecs, Header, [Row|Acc], [], 0,
		tick(Reserved), RowIndex + 1);
layout(Cells, NumCols, ColSpecs, Header, Acc, RowAcc, Col, Reserved, RowIndex) ->
	case maps:is_key(Col, Reserved) of
		true ->
			layout(Cells, NumCols, ColSpecs, Header, Acc, RowAcc, Col + 1,
				Reserved, RowIndex);
		false when Cells =:= [] ->
			Acc;
		false ->
			[Cell|Tail] = Cells,
			Width = maps:get(colspan, element(2, Cell), 1),
			case fits(Reserved, Col, Width, NumCols) of
				true ->
					Placed = place_cell(Cell, Col, ColSpecs, Header andalso RowIndex =:= 0),
					Rowspan = maps:get(rowspan, element(2, Placed), 1),
					layout(Tail, NumCols, ColSpecs, Header, Acc, [Placed|RowAcc],
						Col + Width, reserve(Reserved, Col, Width, Rowspan), RowIndex);
				false when Col =:= 0 andalso RowAcc =:= [] ->
					layout(Tail, NumCols, ColSpecs, Header, Acc, RowAcc, Col,
						Reserved, RowIndex);
				false ->
					Row = {row, #{}, lists:reverse(RowAcc), #{}},
					layout(Tail, NumCols, ColSpecs, Header, [Row|Acc], [], 0,
						tick(Reserved), RowIndex + 1)
			end
	end.

fits(Reserved, Col, Width, NumCols) ->
	Col + Width =< NumCols andalso span_free(Reserved, Col, Col + Width).

span_free(_Reserved, Col, End) when Col >= End ->
	true;
span_free(Reserved, Col, End) ->
	case maps:is_key(Col, Reserved) of
		true -> false;
		false -> span_free(Reserved, Col + 1, End)
	end.

reserve(Reserved, _Col, _Width, Rowspan) when Rowspan =< 1 ->
	Reserved;
reserve(Reserved, Col, Width, Rowspan) ->
	reserve_cols(Reserved, Col, Width, Rowspan).

reserve_cols(Reserved, _Col, 0, _Rowspan) ->
	Reserved;
reserve_cols(Reserved, Col, Width, Rowspan) ->
	reserve_cols(Reserved#{Col => Rowspan}, Col + 1, Width - 1, Rowspan).

tick(Reserved) ->
	maps:from_list([{Col, N - 1} || {Col, N} <- maps:to_list(Reserved), N > 1]).

place_cell({cell, CAttrs, Body, Ann}, Col, ColSpecs, IsHeader) ->
	Merged = maps:merge(column_cell_attrs(column_at(ColSpecs, Col), IsHeader), CAttrs),
	Raw = maps:get(raw, Ann, <<>>),
	Style = maps:get(style, Merged, <<"default">>),
	{cell, Merged#{col => Col}, style_body(Body, Raw, Style),
		maps:remove(raw, Ann)}.

column_at(Specs, Col) when Col >= length(Specs) ->
	#{};
column_at(Specs, Col) ->
	lists:nth(Col + 1, Specs).

column_cell_attrs(Spec, IsHeader) ->
	Attrs0 = put_axis(#{}, halign, maps:get(horizontal, Spec, left)),
	Attrs1 = put_axis(Attrs0, valign, maps:get(vertical, Spec, top)),
	case IsHeader of
		true -> Attrs1;
		false -> put_style(Attrs1, maps:get(style, Spec, default))
	end.

put_axis(Attrs, _Key, left) -> Attrs;
put_axis(Attrs, _Key, top) -> Attrs;
put_axis(Attrs, halign, center) -> Attrs#{halign => <<"center">>};
put_axis(Attrs, halign, right) -> Attrs#{halign => <<"right">>};
put_axis(Attrs, valign, middle) -> Attrs#{valign => <<"middle">>};
put_axis(Attrs, valign, bottom) -> Attrs#{valign => <<"bottom">>};
put_axis(Attrs, _, _) -> Attrs.

put_style(Attrs, default) -> Attrs;
put_style(Attrs, emphasis) -> Attrs#{style => <<"emphasis">>};
put_style(Attrs, monospaced) -> Attrs#{style => <<"monospaced">>};
put_style(Attrs, strong) -> Attrs#{style => <<"strong">>};
put_style(Attrs, header) -> Attrs#{style => <<"header">>};
put_style(Attrs, asciidoc) -> Attrs#{style => <<"asciidoc">>};
put_style(Attrs, literal) -> Attrs#{style => <<"literal">>};
put_style(Attrs, verse) -> Attrs#{style => <<"verse">>};
put_style(Attrs, _) -> Attrs.

%% User Guide 23, table styles. Literal is a literal block.
%% Verse is a verse paragraph: the inline pass keeps its breaks.
style_body(_Body, Raw, <<"literal">>) ->
	[{literal_block, #{<<"subs">> => <<"verbatim">>}, literal_text(Raw), #{}}];
style_body(_Body, Raw, <<"verse">>) ->
	[{quote_block, #{<<"style">> => <<"verse">>}, [], #{raw => Raw}}];
style_body(Body, _Raw, _Style) ->
	Body.

literal_text(Raw) ->
	case drop_blank_lines(binary:split(Raw, <<"\n">>, [global])) of
		[] ->
			<<>>;
		Lines ->
			iolist_to_binary(lists:join(<<"\n">>, Lines))
	end.

drop_blank_lines(Lines) ->
	lists:reverse(drop_leading_blanks(lists:reverse(drop_leading_blanks(Lines)))).

drop_leading_blanks([Line|Rest]) ->
	case trim_ws(Line) of
		<<>> ->
			drop_leading_blanks(Rest);
		_ ->
			[Line|Rest]
	end;
drop_leading_blanks([]) ->
	[].
