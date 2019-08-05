
-module(p02).
-export([but_last/1]).

but_last([A,B|[]])->
	[A,B];
but_last([A,B|T])->
	but_last(T).