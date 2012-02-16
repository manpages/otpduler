-module(odl_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%resource registry
-export([
	hi/2
]).

%node
-export([
	node_idle/1,
	node_working/2,
	node_done/3,
	node_wont_handle/2
]).

%task_manager
-export([
	manager_assign/3
]).

%frontend
-export([
	task_set/2,
	task_swap/3
]).

%api
-export([
	start_link/0
]).

%gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%state
-record(state, {
	resources
}).

%private shit
return(Type, Node, State) ->
	{noreply, #state{resources = add_resource(Type, Node, State#state.resources)}}
.
return(R, Type, Node, State) ->
	{reply, R, #state{resources = add_resource(Type, Node, State#state.resources)}}
.

add_resource(Type, Identifier, Dict) ->
	io:format("<<PRIV>> add_resource -> ~p (~p)~n", [Identifier, Type]),
	case dict:find(Type, Dict) of
		{ok, ResourceList} ->
			NewList = [Identifier|lists:delete(Identifier, ResourceList)],
			dict:store(Type, NewList, Dict);
		error ->
			dict:store(Type, [Identifier], Dict)
	end
.

bcall(M, [H|T],D) -> bcall(M,H,D), bcall(M,T,D);
bcall(M, [],   D) -> ok;
bcall(M, S,    D) -> broadcast(M,S,D,call).

bcast(M, [H|T],D) -> bcast(M,H,D), bcast(M,T,D);
bcast(M, [],   D) -> ok;
bcast(M, S,    D) -> broadcast(M,S,D,cast).

broadcast(Message, ServerT, State, CastOrCall) ->
	case dict:find(ServerT, State#state.resources) of
		{ok, ResourceList} -> lists:foreach(fun(X) ->
			case net_adm:ping(X) of 
				pong -> 
					io:format("<<SEND>> ~p to ~p at ~p~n", [Message, ServerT, X]),
					case CastOrCall of
						call -> gen_server:call({ServerT, X}, Message);
						_    -> gen_server:cast({ServerT, X}, Message)
					end;
				pang -> false
			end
		end,
		ResourceList),ok;
		error -> false
	end
.

%interface-ish
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


hi(Node, Type) ->
	gen_server:call(?SERVER, {hi, {Node, Type}}).


node_idle(Node) -> 
	gen_server:call(?SERVER, {node_idle, Node}).

node_working(Node, TaskID) ->
	gen_server:call(?SERVER, {node_working, {Node, TaskID}}).

node_done(Node, TaskID, Result) ->
	gen_server:call(?SERVER, {node_done, {Node, TaskID, Result}}).

node_wont_handle(Node, TaskID) ->
	gen_server:call(?SERVER, {wont_handle, {Node, TaskID}}). 


manager_assign(Manager, Node, TaskID) ->
	gen_server:call(?SERVER, {assign, {Manager, Node, TaskID}}).


task_set(Frontend, Task) ->
	gen_server:call(?SERVER, {task_set, {Frontend, Task}}).

task_swap(Frontend, TaskID1, TaskID2) ->
	gen_server:call(?SERVER, {task_swap, {Frontend, TaskID1, TaskID2}}).

%implementation, kinda
init([]) ->
	io:format("<<INIT>>~n"),
	{ok, #state{		
		resources = dict:new()
	}}.

handle_call({hi, {Node, Type}}, F, State) ->
	io:format("<<CALL>> hi -> ~p of type ~p~n", [Node, Type]),
	return(hello, Type, Node, State)
.

handle_cast({node_idle, Node}, State) ->
	io:format("<<CALL>> node_idle -> ~p~n", [Node]),
	bcast({node_idle, Node}, [odl_manager, odl_frontend], State),
	{noreply, State}
;


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
