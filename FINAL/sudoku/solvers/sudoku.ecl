:- lib(ic).
:- lib(matrix_util).
:- import alldifferent/1 from ic_global. %if omitted use from standard ic library

solve(P) :-
	% domain
	P :: 1..9,
	% rows
	( foreach(Row,P) do
		alldifferent(Row)
	),
	% columns
	transpose(P, Columns),
	( foreach(Col,Columns) do
		alldifferent(Col)
	),
	% blocks
	[A,B,C,D,E,F,G,H,I] = P,
  	blocks(A, B, C), 
  	blocks(D, E, F), 
  	blocks(G, H, I),
  	
	term_variables(P,AllVars),
	labeling(AllVars).%,
	%pretty_print(P).

%checks each block, see http://stackoverflow.com/questions/1768274/prolog-learning-by-example and swipl documentation
blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  	% all values in a block are distinct
  	alldifferent([A,B,C,D,E,F,G,H,I]),
  	blocks(Bs1, Bs2, Bs3).	

%helper predicate for printing
%pretty_print(P) :-
%	( foreach(Row,P) do
%		write(Row),	nl
%	).

