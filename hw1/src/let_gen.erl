-module(let_gen).
-export([letter_list/2]).

letter_list(1,Letter)->
	[Letter];

letter_list(Number,Letter)->
	[Letter|letter_list(Number-1,Letter)].

% считаем что число всегда больше нуля и не отрицательно иначе зацикливает. 

