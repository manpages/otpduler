-module(odl_manager).

-include("../../include/node.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%api
-export([
	start_link/0
]).

%node
-export([
	node_idle/1,
	node_working/2,
	node_done/3,
	node_wont_handle/2
]).

%frontend
-export([
	task_set/2,
	task_swap/3
]).

%gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%private
get_overseer() ->
	X = 'overseer@memorici.de',
	case net_adm:ping(X) of
		pong -> X;
		_    -> false
	end
.

%interface-ish
start_link() ->
	get_overseer(),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

node_idle(Node) -> 
	gen_server:call(?SERVER, {node_idle, Node}).

node_working(Node, TaskID) ->
	gen_server:call(?SERVER, {node_working, {Node, TaskID}}).

node_done(Node, TaskID, Result) ->
	gen_server:call(?SERVER, {node_done, {Node, TaskID, Result}}).

node_wont_handle(Node, TaskID) ->
	gen_server:call(?SERVER, {wont_handle, {Node, TaskID}}). 



task_set(Frontend, Task) ->
	gen_server:call(?SERVER, {task_set, {Frontend, Task}}).

task_swap(Frontend, TaskID1, TaskID2) ->
	gen_server:call(?SERVER, {task_swap, {Frontend, TaskID1, TaskID2}}).



manager_assign(Manager, Node, TaskID) ->
	gen_server:call(?SERVER, {assign, {Manager, Node, TaskID}}).

%implementation
init([]) ->
	net_kernel:monitor_nodes(true),
	?debugFmt("<<INIT>> ~p~n", [[gen_server:call({global, odl_server}, {hi, {node(), ?SERVER}})]]),
	{ok, []}
.

handle_cast({node_idle, Node}, State) ->
	?debugFmt("<<CAST>> node_idle -> ~p~n", [Node]),
	{noreply, State}
;

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_X, _Y, State) -> {noreply, State}.
handle_info(Msg, State) -> 
	?debugFmt("<<INFO>> ! ~p~n", [Msg]),
	{noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
