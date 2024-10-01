-module(w03demo).

-export([add/2, average_two_numbers/2, print_parts/1,factorial/1, len/1, double/1]).


add(X, Y) ->
    X + Y.


average_two_numbers(X, Y) ->
    (X+Y)/2.

print_parts([])->
    io:format("Empty List");
  print_parts([H|T]) ->
    io:format("Head: ~p~n",[H]),
    io:format("Tail: ~p~n",[T]).

factorial(0) -> 1;
  factorial(N) -> N * factorial(N-1).

len([]) -> 0;
  len([_|T]) ->
    1 + len(T).

  double([]) -> [];
  double([H|T]) ->
    io:format("Head: ~w~n", [H]), 
    io:format("Tail: ~w~n", [T]),
    [2 * H | double(T)].