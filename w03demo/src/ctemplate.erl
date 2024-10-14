-module(ctemplate).
-export([start/0,rpc/2,loop/0]). % loop must be exported so it can be spawned.
start() -> % specialize this to the function being spawned
	spawn(?MODULE,loop,[]). % this is a very effective way of not having to type this over and over in your code.

rpc(Pid, Request) -> % rename this to match what will be done by the spawned process
	Pid ! {self(),Request},
	receive
		{Pid,Response} ->
			Response -> Response % just sending the response back to the caller
	end.

% rename this to something other than loop.
loop()->
	receive
	{Pid,multiply,List} ->
		Pid ! {ok,lists:foldl(fun(X,Y)->X*Y end,1,List)};
  	{Pid,add,List} ->
		Pid ! {ok,lists:foldl(fun(X,Y)->X+Y end,0,List)};
  	{Pid,divide,Dividend,Divisor} -> 
		Pid ! {ok,Dividend div Divisor}
	end,
	loop().
