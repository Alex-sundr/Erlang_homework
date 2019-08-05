-module(p04).
-export([len/1, length/1]).

len([])->
	0;

len([_|T])->
	1+len(T).

%хвостовая рекурсия
length([]) -> 
	0;
length(L) -> 
	length(L, 0);

length([], N) -> 
	N;

length([_H|T], N) -> 
	length(T, N + 1).