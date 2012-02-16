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
	frontends,
	managers,
	nodes
}).

%interface-ish
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
	{ok, []}.

handle_call({hi, {Node, Type}}, F, T) ->
	io:format("HELLO ~p of type ~p (from ~p)", [Node, Type, F]),
	{reply, T,T}
.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
