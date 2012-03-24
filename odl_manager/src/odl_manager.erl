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
	node_idle/2,
	node_working/3,
	node_done/4,
	node_wont_handle/3
]).

%frontend
-export([
	task_set/3,
	task_swap/4
]).

%gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%private
%get_overseer() ->
%	X = 'overseer@memorici.de',
%	case net_adm:ping(X) of
%		pong -> X;
%		_    -> false
%	end
%.

%interface-ish
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

node_idle(PID, NodePID) -> 
	gen_server:call(PID, {node_idle, NodePID}).

node_working(PID, NodePID, TaskID) ->
	gen_server:call(PID, {node_working, {NodePID, TaskID}}).

node_done(PID, NodePID, TaskID, Result) ->
	gen_server:call(PID, {node_done, {NodePID, TaskID, Result}}).

node_wont_handle(PID, NodePID, TaskID) ->
	gen_server:call(PID, {wont_handle, {NodePID, TaskID}}). 



task_set(PID, FrontendPID, Task) ->
	gen_server:call(PID, {task_set, {FrontendPID, Task}}).

task_swap(PID, FrontendPID, TaskID1, TaskID2) ->
	gen_server:call(PID, {task_swap, {FrontendPID, TaskID1, TaskID2}}).


%implementation
init([]) ->
	application:start(resource_discovery),
	%resource_discovery:add_local_resource_tuple([{odl_manager, self()}]), 
	%%% REMOVED AS NOW IT'S ASSERTED BY THE APP
	lists:foreach(
		fun(X) -> 
			resource_discovery:add_target_resource_type(X) 
		end, 
		[odl_unix_server, odl_erlang_server, odl_callback_server]
	),
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
