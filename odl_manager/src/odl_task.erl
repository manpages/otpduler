-module(odl_task).

-include("../../include/task.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
	get/1
]).

%get_first(QID) -> 
%	case fission_zset:part_left({queue,QID}, 0, 1) of 
%		{result, [{Seed, {EntityT, EID}]} -> {Seed, TaskID};
%		_ -> false
%	end
%.
%get_top_task() -> 
%	case get_top() of
%		{_,ID} -> sch_task:get(ID);
%		false -> false
%	end
%.
get(TID) ->
	fission_syn:get({task, TID}).
