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

-module(asciideck_stdin_reader).
-behaviour(gen_server).

%% The API is defined in asciideck_reader.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	lines = [] :: [binary()],
	pos = 1 :: non_neg_integer()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(read_line, _From, State=#state{lines=Lines, pos=Pos})
		when length(Lines) >= Pos ->
	{reply, lists:nth(Pos, lists:reverse(Lines)), State#state{pos=Pos + 1}};
handle_call(read_line, _From, State=#state{lines=Lines, pos=Pos}) ->
	case io:get_line('') of
		eof ->
			{reply, eof, State};
		Line0 ->
			Line1 = string:strip(Line0, right, $\n),
			Line = unicode:characters_to_binary(Line1),
			{reply, Line, State#state{lines=[Line|Lines], pos=Pos + 1}}
	end;
handle_call(get_position, _From, State=#state{pos=Pos}) ->
	{reply, Pos, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({set_position, Pos}, State) ->
	{noreply, State#state{pos=Pos}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
