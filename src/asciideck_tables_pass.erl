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

-define(IS_WS(C), (C =:= $\s) or (C =:= $\t) or (C =:= $\n).

run([]) ->
	[];
run([Table={table, _, _, _}|Tail]) ->
	[table(Table)|run(Tail)];
run([Block|Tail]) ->
	[Block|run(Tail)].

table({table, Attrs, Contents, Ann}) ->
	{Cells, NumCols} = parse_table(Contents, Attrs),
	Children = rows(Cells, NumCols),
	{table, Attrs, Children, Ann}.

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
-endif.

%% If the cols attribute is not specified, the number of
%% columns is the number of cells on the first line.
parse_table(Contents, #{<<"cols">> := Cols}) ->
	{parse_cells(Contents, []), num_cols(Cols)};
%% We get the first line, parse the cells in it then
%% count the number of columns in the table. Finally
%% we parse all the remaining cells.
parse_table(Contents, _) ->
	case binary:split(Contents, <<$\n>>) of
		%% We only have the one line. Who writes tables like this?
		[Line] ->
			Cells = parse_cells(Line, []),
			{Cells, length(Cells)};
		%% We have a useful table with more than one line. Good user!
		[Line, Rest] ->
			Cells0 = parse_cells(Line, []),
			Cells = parse_cells(Rest, lists:reverse(Cells0)),
			{Cells, length(Cells0)}
	end.

%% @todo Don't discard Specs.
num_cols(Cols) ->
	try binary_to_integer(Cols) of
		Int -> Int
	catch _:_ ->
		Specs0 = binary:split(Cols, <<$,>>, [global]),
		Specs = [parse_specs(Spec) || Spec <- Specs0],
		lists:sum([M || #{multiplier := M} <- Specs])
	end.

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
	[{V, fun() -> R = num_cols(V) end} || {V, R} <- Tests].
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

parse_cells(Contents, Acc) ->
	Cells = split_cells(Contents),%binary:split(Contents, [<<$|>>], [global]),
	do_parse_cells(Cells, Acc).
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
do_parse_cells([Contents], Acc) ->
	%% @todo Annotations.
	lists:reverse([{cell, #{specifiers => <<>>}, Contents, #{}}|Acc]);
%% Last cell. There are no further cell specifiers.
do_parse_cells([Specs, Contents0], Acc) ->
	Contents = asciideck_block_parser:parse(Contents0),
	%% @todo Annotations.
	Cell = {cell, #{specifiers => Specs}, Contents, #{}},
	lists:reverse([Cell|Acc]);
%% If there are cell specifiers we need to extract them from the cell
%% contents. Cell specifiers are everything from the last whitespace
%% until the end of the binary.
do_parse_cells([Specs, Contents0|Tail], Acc) ->
	NextSpecs = <<>>, %% @todo find_r(Contents0, <<>>),
	Len = byte_size(Contents0) - byte_size(NextSpecs),
	<<Contents1:Len/binary, _/bits>> = Contents0,
	Contents = asciideck_block_parser:parse(Contents1),
	%% @todo Annotations.
	Cell = {cell, #{specifiers => Specs}, Contents, #{}},
	do_parse_cells([NextSpecs|Tail], [Cell|Acc]).

%% @todo This is not correct. Not all remaining data is specifiers.
%% In addition, for columns at the end of the line this doesn't apply.
%% Find the remaining data after the last whitespace character.
%find_r(<<>>, Acc) ->
%	Acc;
%find_r(<<C, Rest/bits>>, _) when ?IS_WS(C) ->
%	find_r(Rest, Rest);
%find_r(<<_, Rest/bits>>, Acc) ->
%	find_r(Rest, Acc).

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
	], 3} = parse_table(<<
		"|1 |2 |A\n"
		"|3 |4 |B\n"
		"|5 |6 |C">>, #{}),
	ok.

parse_table_escape_pipe_test() ->
	{[
		{cell, _, [{paragraph, _, <<"1">>, _}], _},
		{cell, _, [{paragraph, _, <<"2">>, _}], _},
		{cell, _, [{paragraph, _, <<"3 |4">>, _}], _},
		{cell, _, [{paragraph, _, <<"5">>, _}], _}
	], 2} = parse_table(<<
		"|1 |2\n"
		"|3 \\|4 |5">>, #{}),
	ok.
-endif.

%% @todo We currently don't handle colspans and rowspans.
rows(Cells, NumCols) ->
	rows(Cells, [], NumCols, [], NumCols).

%% End of row.
rows(Tail, Acc, NumCols, RowAcc, CurCol) when CurCol =< 0 ->
	%% @todo Annotations.
	Row = {row, #{}, lists:reverse(RowAcc), #{}},
	rows(Tail, [Row|Acc], NumCols, [], NumCols);
%% Add a cell to the row.
rows([Cell|Tail], Acc, NumCols, RowAcc, CurCol) ->
	rows(Tail, Acc, NumCols, [Cell|RowAcc], CurCol - 1);
%% End of a properly formed table.
rows([], Acc, _, [], _) ->
	lists:reverse(Acc);
%% Malformed table. Even if we expect more columns,
%% if there are no more cells there's nothing we can do.
rows([], Acc, _, RowAcc, _) ->
	%% @todo Annotations.
	Row = {row, #{}, lists:reverse(RowAcc), #{}},
	lists:reverse([Row|Acc]).
