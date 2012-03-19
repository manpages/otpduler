%%%-------------------------------------------------------------------
%%% @author Jonn Mostovoy
%%% @copyright MIT License
%%% @doc
%%%   Logger lorem-ipsum-ish module.
%%% @end
%%%-------------------------------------------------------------------
-module(rd_log).

-include_lib("eunit/include/eunit.hrl").

-export([
	info/2
]).

-spec info(term(), list()) -> ok.
info(Fmt, Args) ->
	?debugFmt(Fmt, Args),
	ok
.
