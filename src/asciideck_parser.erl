%% Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
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

-module(asciideck_parser).

-export([parse/2]).

%% @todo
%% All nodes in the AST are of type {Type, Attrs, Text | Nodes, Ann}
%% except for text formatting nodes at the moment. Text formatting
%% nodes will be converted to this form in a future change.

%% Parsing occurs in a few passes:
%%
%% * p1: Line-based parsing of the raw Asciidoc document
%% * p2: Deal with more compp1 structures like lists and tables

parse(Data, St) ->
	Lines0 = binary:split(Data, <<"\n">>, [global]),
	%% Ensure there's an empty line at the end, to simplify parsing.
	Lines1 = lists:append(Lines0, [<<>>]),
	LineNumbers = lists:seq(1, length(Lines1)),
	Lines = lists:zip(LineNumbers, Lines1),
	%% @todo Document header, if any. Recognized by the author info/doc attributes?
	%% Alternatively, don't recognize it, and only use attribute entries for the same info.
	p2(p1(Lines, [], St), []).

%% First pass.

%% @todo When a block element is encountered asciidoc(1) determines the type of block by checking in the following order (first to last): (section) Titles, BlockMacros, Lists, DelimitedBlocks, Tables, AttributeEntrys, AttributeLists, BlockTitles, Paragraphs.

%% @todo And this function is parsing, not p1ing.
p1([], AST, _St) ->
	lists:reverse(AST);
%% Extra empty lines.
p1([{_, <<>>}|Tail], AST, St) ->
	p1(Tail, AST, St);
%% Comments.
p1([{LN, <<"//", Comment/bits >>}|Tail], AST, St) ->
	p1(Tail, [comment(trim_ws(Comment), ann(LN, St))|AST], St);
%% Section titles.
p1([{LN, <<"= ", Title/bits >>}, {_, <<>>}|Tail], AST, St) ->
	p1_title_short(Tail, AST, St, LN, Title, 0);
p1([{LN, <<"== ", Title/bits >>}, {_, <<>>}|Tail], AST, St) ->
	p1_title_short(Tail, AST, St, LN, Title, 1);
p1([{LN, <<"=== ", Title/bits >>}, {_, <<>>}|Tail], AST, St) ->
	p1_title_short(Tail, AST, St, LN, Title, 2);
p1([{LN, <<"==== ", Title/bits >>}, {_, <<>>}|Tail], AST, St) ->
	p1_title_short(Tail, AST, St, LN, Title, 3);
p1([{LN, <<"===== ", Title/bits >>}, {_, <<>>}|Tail], AST, St) ->
	p1_title_short(Tail, AST, St, LN, Title, 4);
%% Block titles.
p1([{_LN, <<".", Title/bits >>}|Tail], AST, St) ->
	p1(Tail, [{block_title, Title}|AST], St);
%% Attribute lists.
p1([{_LN, <<"[", Attrs/bits >>}|Tail], AST, St) ->
	p1(Tail, [{attribute_list, p1_attr_list(Attrs)}|AST], St);
%% Listing blocks.
p1([{LN, <<"----", _/bits >>}|Tail], AST, St) ->
	p1_listing(Tail, AST, St, LN, []);
%% Lists.
p1([{LN, <<"* ", Text/bits >>}|Tail], AST, St) ->
	p1_li(Tail, AST, St, uli1, {LN, Text});
p1([{LN, <<"** ", Text/bits >>}|Tail], AST, St) ->
	p1_li(Tail, AST, St, uli2, {LN, Text});
p1([{LN, <<"*** ", Text/bits >>}|Tail], AST, St) ->
	p1_li(Tail, AST, St, uli3, {LN, Text});
p1([{LN, <<"**** ", Text/bits >>}|Tail], AST, St) ->
	p1_li(Tail, AST, St, uli4, {LN, Text});
p1([{LN, <<"***** ", Text/bits >>}|Tail], AST, St) ->
	p1_li(Tail, AST, St, uli5, {LN, Text});
%% Tables.
p1([{LN, <<"|===", _/bits >>}|Tail], AST, St) ->
	p1_table(Tail, AST, St, LN);
p1([{LN, <<"|", Text/bits >>}|Tail], AST, St) ->
	p1_cell(Tail, AST, St, LN, Text);
%% Prefix-based or paragraph.
p1(Lines, AST, St) ->
	p1_text(Lines, AST, St).

p1_title_short(Tail, AST, St, LN, Text0, Level) ->
	%% Remove the trailer, if any.
	Text1 = trim_ws(Text0),
	Trailer = case Level of
		0 -> <<" =">>;
		1 -> <<" ==">>;
		2 -> <<" ===">>;
		3 -> <<" ====">>;
		4 -> <<" =====">>
	end,
	TrailerSize = byte_size(Trailer),
	Size = byte_size(Text1) - TrailerSize,
	Text3 = case Text1 of
		<< Text2:Size/binary, Trailer:TrailerSize/binary >> -> Text2;
		_ -> Text1
	end,
	Text = trim_ws(Text3),
	p1(Tail, [title(Text, #{level => Level}, ann(LN, St))|AST], St).

p1_attr_list(AttrList0) ->
	[AttrList|_] = binary:split(AttrList0, <<"]">>),
	binary:split(AttrList, <<",">>).

%% @todo Parse attributes properly.
p1_table(Tail, [{attribute_list, Attrs}, {block_title, Title}|AST], St, LN) ->
	p1(Tail, [{begin_table, #{title => Title, todo => Attrs}, ann(LN, St)}|AST], St);
p1_table(Tail, [{attribute_list, Attrs}|AST], St, LN) ->
	p1(Tail, [{begin_table, #{todo => Attrs}, ann(LN, St)}|AST], St);
p1_table(Tail, AST=[nl, {cell, _, _, _}|_], St, _) ->
	p1(Tail, [end_table|AST], St);
p1_table(Tail, AST=[{cell, _, _, _}|_], St, _) ->
	p1(Tail, [end_table|AST], St);
p1_table(Tail, AST, St, LN) ->
	p1(Tail, [{begin_table, #{}, ann(LN, St)}|AST], St).

%% @todo Multiline cells.
%% @todo Styled cells.
%% @todo Strip whitespace at the beginning of the cell if on the same line.
p1_cell(Tail=[{_, NextLine}|_], AST0, St, LN, Text) ->
	case p1_cell_split(Text, <<>>) of
		[_] ->
			AST1 = [nl, cell(p1([{LN, trim_ws(Text)}, {LN, <<>>}], [], St), ann(LN, St))|AST0],
			AST = case NextLine of
				<<>> -> [nl|AST1];
				_ -> AST1
			end,
			p1(Tail, AST, St);
		[Cell, Rest] ->
			p1_cell(Tail, [cell(p1([{LN, trim_ws(Cell)}, {LN, <<>>}], [], St), ann(LN, St))|AST0], St, LN, Rest)
	end.

p1_cell_split(<<>>, Acc) ->
	[Acc];
p1_cell_split(<< $\\, $|, Rest/bits >>, Acc) ->
	p1_cell_split(Rest, << Acc/binary, $\\, $| >>);
p1_cell_split(<< $|, Rest/bits >>, Acc) ->
	[Acc, Rest];
p1_cell_split(<< C, Rest/bits >>, Acc) ->
	p1_cell_split(Rest, << Acc/binary, C >>).

p1_listing([{_, <<"----", _/bits >>}, {_, <<>>}|Tail], AST0, St, LN, [_|Acc]) ->
	Text = iolist_to_binary(lists:reverse(Acc)),
	case AST0 of
		[{attribute_list, [<<"source">>, Lang]}, {block_title, Title}|AST] ->
			p1(Tail, [listing(Text, #{title => Title, language => Lang}, ann(LN, St))|AST], St);
		[{block_title, Title}, {attribute_list, [<<"source">>, Lang]}|AST] ->
			p1(Tail, [listing(Text, #{title => Title, language => Lang}, ann(LN, St))|AST], St);
		[{attribute_list, [<<"source">>, Lang]}|AST] ->
			p1(Tail, [listing(Text, #{language => Lang}, ann(LN, St))|AST], St);
		[{block_title, Title}|AST] ->
			p1(Tail, [listing(Text, #{title => Title}, ann(LN, St))|AST], St);
		AST ->
			p1(Tail, [listing(Text, #{}, ann(LN, St))|AST], St)
	end;
p1_listing([{_, Line}|Tail], AST, St, LN, Acc) ->
	p1_listing(Tail, AST, St, LN, [<<"\n">>, Line|Acc]).

p1_li(Lines, AST, St, Type, FirstLine = {LN, _}) ->
	{Tail, Glob} = p1_li_glob(Lines, []),
	p1(Tail, [{Type, p1([FirstLine|Glob], [], St), ann(LN, St)}|AST], St).

%% Glob everything until next list or empty line.
p1_li_glob(Tail = [{LN, << "*", _/bits >>}|_], Acc) ->
	{Tail, lists:reverse([{LN, <<>>}|Acc])};
p1_li_glob(Tail = [{LN, <<>>}|_], Acc) ->
	{Tail, lists:reverse([{LN, <<>>}|Acc])};
p1_li_glob([{_, <<"+">>}|Tail], Acc) ->
	p1_li_glob(Tail, [<<>>|Acc]);
p1_li_glob([Line|Tail], Acc) ->
	p1_li_glob(Tail, [Line|Acc]).

p1_text(Lines=[{LN, Line}|Tail], AST, St) ->
	case binary:split(<< Line/binary, $\s >>, <<":: ">>) of
		%% Nothing else on the line.
		[Label, <<>>] ->
			p1(Tail, [{label, Label, ann(LN, St)}|AST], St);
		%% Text on the same line.
		[Label, Text0] ->
			Size = byte_size(Text0) - 1,
			<< Text:Size/binary, _ >> = Text0,
			p1([{LN, Text}|Tail], [{label, Label, ann(LN, St)}|AST], St);
		%% Not a labeled list.
		_ ->
			p1_maybe_p(Lines, AST, St)
	end.

%% @todo Literal paragraphs.
p1_maybe_p([{_LN, << " ", Line/bits >>}|Tail], AST, St) ->
	<<>> = trim_ws(Line),
	p1(Tail, AST, St);
p1_maybe_p(Lines=[{LN, _}|_], AST, St) ->
	p1_p(Lines, AST, St, LN, []).

p1_p([{_, <<>>}|Tail], AST0, St, LN, [_|Acc]) ->
	Text = format(iolist_to_binary(lists:reverse(Acc))),
	case AST0 of
		[{block_title, Title}|AST] ->
			p1(Tail, [paragraph(Text, #{title => Title}, ann(LN, St))|AST], St);
		AST ->
			p1(Tail, [paragraph(Text, #{}, ann(LN, St))|AST], St)
	end;
p1_p([{_, Line}|Tail], AST, St, LN, Acc) ->
	p1_p(Tail, AST, St, LN, [<<" ">>, Line|Acc]).

%% Inline formatting.

%% @todo Probably do it as part of the node functions that require it.
format(Text) ->
	case format(Text, [], <<>>, $\s) of
		[Bin] when is_binary(Bin) -> Bin;
		Formatted -> Formatted
	end.

format(<<>>, Acc, <<>>, _) ->
	lists:reverse(Acc);
format(<<>>, Acc, BinAcc, _) ->
	lists:reverse([BinAcc|Acc]);
format(<< "link:", Rest0/bits >>, Acc, BinAcc, Prev) when Prev =:= $\s ->
	case re:run(Rest0, "^([^[]*)\\[([^]]*)\\](.*)", [{capture, all, binary}]) of
		nomatch -> format(Rest0, Acc, << BinAcc/binary, "link:" >>, $:);
		{match, [_, Link, Text, Rest]} -> format(Rest, [{link, Link, Text}, BinAcc|Acc], <<>>, $])
	end;
format(<< $*, Rest0/bits >>, Acc, BinAcc, Prev) when Prev =:= $\s ->
	case binary:split(Rest0, << $* >>) of
		[_] -> format(Rest0, Acc, << BinAcc/binary, $* >>, $*);
		[Em, Rest] -> format(Rest, [{em, Em}, BinAcc|Acc], <<>>, $*)
	end;
format(<< $`, Rest0/bits >>, Acc, BinAcc, Prev) when Prev =:= $\s ->
	case binary:split(Rest0, << $` >>) of
		[_] -> format(Rest0, Acc, << BinAcc/binary, $` >>, $`);
		[Mono, Rest] -> format(Rest, [{mono, Mono}, BinAcc|Acc], <<>>, $`)
	end;
format(<< C, Rest/bits >>, Acc, BinAcc, _) ->
	format(Rest, Acc, << BinAcc/binary, C >>, C).

%% Second pass.

p2([], Acc) ->
	lists:reverse(Acc);
p2([{label, Label, Ann}, Item|Tail], Acc) ->
	%% @todo Handle this like other lists.
	p2(Tail, [ll([li([Item], #{label => Label}, Ann)], #{}, Ann)|Acc]);
p2(Tail0=[{uli1, _, UlAnn}|_], Acc) ->
	{LIs0, Tail} = lists:splitwith(fun({uli1, _, _}) -> true; (_) -> false end, Tail0),
	LIs = [li(I, LiAnn) || {uli1, I, LiAnn} <- LIs0],
	p2(Tail, [ul(LIs, #{}, UlAnn)|Acc]);
p2([{begin_table, Attrs, Ann}|Tail0], Acc) ->
	%% @todo Can also get them from Attrs?
	N = count_table_columns(Tail0),
	{Rows, Tail} = p2_rows(Tail0, [], [], N, 1),
	p2(Tail, [table(Rows, Attrs, Ann)|Acc]);
p2([Item|Tail], Acc) ->
	p2(Tail, [Item|Acc]).

%% @todo One cell per line version.
count_table_columns(Cells) ->
	length(lists:takewhile(fun({cell, _, _, _}) -> true; (_) -> false end, Cells)).

p2_rows([nl|Tail], Rows, Cols, NumCols, N) ->
	p2_rows(Tail, Rows, Cols, NumCols, N);
p2_rows([Cell = {cell, _, _, Ann}|Tail], Rows, Cols, NumCols, NumCols) ->
	p2_rows(Tail, [row(lists:reverse([Cell|Cols]), Ann)|Rows], [], NumCols, 1);
p2_rows([Cell = {cell, _, _, _}|Tail], Rows, Cols, NumCols, N) ->
	p2_rows(Tail, Rows, [Cell|Cols], NumCols, N + 1);
p2_rows([end_table|Tail], Rows, [], _, _) ->
	{lists:reverse(Rows), Tail}.

%% Annotations.

ann(Line, St) ->
	ann(Line, 1, St).

%% @todo Take filename too, if any.
ann(Line, Col, _St) ->
	#{line => Line, col => Col}.

%% Nodes.

cell(Nodes, Ann) ->
	{cell, #{}, Nodes, Ann}.

comment(Text, Ann) ->
	{comment, #{}, Text, Ann}.

li(Nodes, Ann) ->
	li(Nodes, #{}, Ann).

li(Nodes, Attrs, Ann) ->
	{li, Attrs, Nodes, Ann}.

listing(Text, Attrs, Ann) ->
	{listing, Attrs, Text, Ann}.

ll(Nodes, Attrs, Ann) ->
	{ll, Attrs, Nodes, Ann}.

paragraph(Text, Attrs, Ann) ->
	{p, Attrs, Text, Ann}.

row(Nodes, Ann) ->
	{row, #{}, Nodes, Ann}.

table(Nodes, Attrs, Ann) ->
	{table, Attrs, Nodes, Ann}.

title(Text, Attrs, Ann) ->
	{title, Attrs, Text, Ann}.

ul(Nodes, Attrs, Ann) ->
	{ul, Attrs, Nodes, Ann}.

%% Utility functions.

trim_ws(Text) ->
	iolist_to_binary(re:replace(Text, "^[ \\t]+|[ \\t]+$", <<>>, [global])).
