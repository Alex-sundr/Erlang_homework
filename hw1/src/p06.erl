-module(p06).
-export([is_palindrome/1]).


is_palindrome([])-> 
    false;

is_palindrome([_|[]])-> 
    false;

is_palindrome(List)->
	is_palindrome(List,reverse(List)).

is_palindrome(List, Rlist)->
	 List == Rlist.

%----------------------------------------------
	reverse([])->
	    [];
	reverse([H|[]])->
	    [H|[]];

	reverse(List)-> 
		reverse(List,[]).

	reverse([],Acc)->
		Acc;	

	reverse([H|T], Acc)->
	    reverse(T,[H|Acc]).