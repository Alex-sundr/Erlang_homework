-module(demo2).
-export([double/1,double2/1]).

double([H|T])->
	NewH = 2*H,
	NewT = double(T),
	[NewH|NewT];
double([])->
	[].

double2([H|T])->
	[H*2|double2(T)];
double2([])->
	[].