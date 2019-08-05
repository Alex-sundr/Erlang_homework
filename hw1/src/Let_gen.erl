-module(Let_gen).
-export([letter_list/2]).

letter_list(0,Letter)->
	Letter;

letter_list(Number,Letter)->
	[Letter|letter_list(Number-1,Letter)].


%timesof(Number,Char) ->
    %timesof(Number,Char,[]).
%timesof(Number,Char,Acc) when Number > 0 ->
  %  timesof(Number-1,Char,[Char|Acc]);
%timesof(_Number,_Char,Acc) ->
   % Acc.

