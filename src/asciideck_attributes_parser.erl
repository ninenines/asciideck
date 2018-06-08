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

%% Asciidoc User Guide 29
-module(asciideck_attributes_parser).

-export([parse/1]).

-type attributes() :: #{
	%% The raw attribute list.
	0 := binary(),
	%% Positional attributes.
	pos_integer() => binary(),
	%% Named attributes.
	binary() => binary()
}.
-export_type([attributes/0]).

-define(IS_WS(C), (C =:= $\s) or (C =:= $\t)).

-spec parse(binary()) -> attributes().
parse(Data) ->
	parse(Data, #{0 => Data}, 1).

parse(<<>>, Attrs, _) ->
	Attrs;
parse(Data, Attrs, Nth) ->
	case parse_attr(Data, <<>>) of
		{Value, Rest} when Nth =/= undefined ->
			parse(Rest, Attrs#{Nth => Value}, Nth + 1);
		{Name, Value, Rest} ->
			parse(Rest, Attrs#{Name => Value}, undefined)
	end.

parse_attr(<<>>, Acc) ->
	{Acc, <<>>};
%% Skip preceding whitespace.
parse_attr(<<C, R/bits>>, <<>>) when ?IS_WS(C) ->
	parse_attr(R, <<>>);
%% Parse quoted positional attributes in their own function.
parse_attr(<<$", R/bits>>, <<>>) ->
	parse_quoted_attr(R, <<>>);
%% We have a named attribute, parse the value.
parse_attr(<<$=, R/bits>>, Name) when Name =/= <<>> ->
	parse_attr_value(R, asciideck_block_parser:trim(Name, trailing), <<>>);
%% We have a positional attribute.
parse_attr(<<$,, R/bits>>, Value) ->
	{asciideck_block_parser:trim(Value, trailing), R};
%% Continue.
parse_attr(<<C, R/bits>>, Acc) when C =/= $= ->
	parse_attr(R, <<Acc/binary, C>>).

%% Get everything until the next double quote.
parse_quoted_attr(<<$", R/bits>>, Acc) ->
	parse_quoted_attr_end(R, Acc);
parse_quoted_attr(<<$\\, $", R/bits>>, Acc) ->
	parse_quoted_attr(R, <<Acc/binary, $">>);
parse_quoted_attr(<<C, R/bits>>, Acc) ->
	parse_quoted_attr(R, <<Acc/binary, C>>).

%% Skip the whitespace until the next comma or eof.
parse_quoted_attr_end(<<>>, Value) ->
	{Value, <<>>};
parse_quoted_attr_end(<<$,, R/bits>>, Value) ->
	{Value, R};
parse_quoted_attr_end(<<C, R/bits>>, Value) when ?IS_WS(C) ->
	parse_quoted_attr_end(R, Value).

parse_attr_value(<<>>, Name, Acc) ->
	{Name, Acc, <<>>};
%% Skip preceding whitespace.
parse_attr_value(<<C, R/bits>>, Name, <<>>) when ?IS_WS(C) ->
	parse_attr_value(R, Name, <<>>);
%% Parse quoted positional attributes in their own function.
parse_attr_value(<<$", R/bits>>, Name, <<>>) ->
	{Value, Rest} = parse_quoted_attr(R, <<>>),
	{Name, Value, Rest};
%% Done.
parse_attr_value(<<$,, R/bits>>, Name, Value) ->
	{Name, asciideck_block_parser:trim(Value, trailing), R};
%% Continue.
parse_attr_value(<<C, R/bits>>, Name, Acc) ->
	parse_attr_value(R, Name, <<Acc/binary, C>>).

-ifdef(TEST).
attribute_0_test() ->
	#{0 := <<"Hello,world,width=\"50\"">>} = parse(<<"Hello,world,width=\"50\"">>),
	ok.

parse_test() ->
	#{} = parse(<<>>),
	#{
		1 := <<"Hello">>
	} = parse(<<"Hello">>),
	#{
		1 := <<"quote">>,
		2 := <<"Bertrand Russell">>,
		3 := <<"The World of Mathematics (1956)">>
	} = parse(<<"quote, Bertrand Russell, The World of Mathematics (1956)">>),
	#{
		1 := <<"22 times">>,
		<<"backcolor">> := <<"#0e0e0e">>,
		<<"options">> := <<"noborders,wide">>
	} = parse(<<"\"22 times\", backcolor=\"#0e0e0e\", options=\"noborders,wide\"">>),
	#{
		1 := <<"A footnote&#44; &#34;with an image&#34; image:smallnew.png[]">>
	} = parse(<<"A footnote&#44; &#34;with an image&#34; image:smallnew.png[]">>),
	ok.
-endif.
