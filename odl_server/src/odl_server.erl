-module(odl_server).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%api
%resource registry
-export([
	start/0,
	hi/3,
	get/2
]).

%gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%state
-record(state, {
	resources
}).


start() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%private shit
return(R, Type, Node, State) ->
	{reply, R, #state{resources = add_resource(Type, Node, State#state.resources)}}
.

add_resource(Type, Identifier, Dict) ->
	?debugFmt("<<PRIV>> add_resource -> ~p (~p)~n", [Identifier, Type]),
	case dict:find(Type, Dict) of
		{ok, ResourceList} ->
			NewList = [Identifier|lists:delete(Identifier, ResourceList)],
			dict:store(Type, NewList, Dict);
		error ->
			dict:store(Type, [Identifier], Dict)
	end
.

%api shit
hi(P,N,T)->gen_server:call({?SERVER, P}, {hi , {N,T}}).
get(P,T) ->gen_server:call({?SERVER, P}, {get,   T  }).



%implementation, kinda
init([]) ->
	?debugMsg("<<INIT>>~n"),
	{ok, #state{		
		resources = dict:new()
	}}.

handle_call({hi, {Node, Type}}, _F, State) ->
	?debugFmt("<<CALL>> hi -> ~p of type ~p~n", [Node, Type]),
	return({hello, self()}, Type, Node, State)
;

handle_call({get, Type}, _F, State) ->
	?debugFmt("<<CALL>> get -> request for ~p reulted in~n<<<<>>>> ~p~n", [Type, dict:find(Type, State)]),
	{reply, dict:find(Type, State), State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
