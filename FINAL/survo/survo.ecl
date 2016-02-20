:- lib(ic).
:- lib(ic_global).
:- lib(matrix_util).
:- import sumlist/2, alldifferent/1 from ic_global.

%a survo puzzle! see http://en.wikipedia.org/wiki/Survo_Puzzle
survo([X,Y], Board) :-
	length(Y,N),length(X,M), Max is N*M,
	matrix(N, M, Board, Cols),     %check the format of Board or create it and also make list of cols
	flatten(Board,B),
	B :: 1..Max, 
	alldifferent(B),
	
	(foreach(Row,Board), foreach(RowSum,Y) do
		sumlist(Row,RowSum)
	),
	(foreach(Col,Cols), foreach(ColSum,X) do
		sumlist(Col,ColSum)
	),
	
	term_variables(B,Vars),
	labeling(Vars).


%% solves all example puzzles, should take ~ 1 min
test :- findall((Id,P,[X,Y]),problem(Id,P,[X,Y]), List),
		( foreach((Id,P,[X,Y]),List) do
			printf("*** Puzzle %d, Horizontal %w,  Vertical %w\n",[Id,X,Y]),
			statistics(times, [T0|_]),	survo([X,Y],P),	statistics(times, [T1|_]),
    		Secs is T1-T0,
    		printf("Took %.2f seconds to solve this one:\n", [Secs]),
			pretty_print(P)
		).
		
%%%some test data
%problem(Id, Initial, [Horizontal, Vertical])

problem(1, _, [[21,10,18,29], [24,15,39]]).
problem(2, P, [[21,10,18,29], [24,15,39]]) :-
	P = [ [7,_,5,_],
		  [_,1,_,8],
		  [_,_,11,_]
		].

problem(3, _, [[51,42,26,17], [51,36,32,17]]).

%some more difficult http://www.survo.fi/puzzles/
problem(4, _, [[42,51,63,21,33],[38,63,83,26]]).
problem(5, _, [[49,31,38,18],[17,54,40,25]]).
problem(6, _, [[48,54,30,35,43],[17,38,85,70]]).
problem(7, _, [[48,30,69,40,23],[62,27,80,41]]). %this one is very difficult
problem(8, _, [[41,19,51,33,66],[72,63,19,56]]).
problem(9, _, [[24,33,38,57,58],[20,44,65,81]]).
problem(10, _, [[12,17,28,37,40,45,52],[50,67,114]]).

%%

pretty_print(P) :-
	( foreach(Row,P) do
		writeln(Row)
	), nl.
