-module (p004).

-export([length/1]).

length(L) -> 
	length(L, 0);

length([], Total) -> 
	Total;

length([_|T], Total) -> 
	length(T, Total + 1).