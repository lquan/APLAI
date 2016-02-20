:- lib(ic).
:- lib(matrix_util).
:- import all_different/1 from ic_global.
:- [sudoku_puzzles].

sudoku(P, VarOrder) :-
	statistics(times, [T0|_]),
	model(P),
	term_variables(P,AllVars),
	findall(AllVars, (search(AllVars, 0, VarOrder, indomain, complete, [backtrack(BT)]), setval(backtracks, BT)), Solutions),

	%search(AllVars, 0, VarOrder, indomain, complete, [backtrack(BT)]), setval(backtracks, BT),
    
    statistics(times, [T1|_]),
    Secs is T1-T0,
	length(Solutions, NrSolutions),
    getval(backtracks, BTs),

    %pretty_print(P),
    print("VarOrder; NrSols; Seconds; Backtracks\n"),
    printf("%a; %d; %w; %d\n", [VarOrder, NrSolutions, Secs, BTs]).

    %printf("%.2f; %d\n", [Secs, BTs]).

model(P) :-
	P :: 1..9,
	( foreach(Row,P) do
		alldifferent(Row)
	),
	transpose(P, Columns),
	( foreach(Col,Columns) do
		alldifferent(Col)
	),
	[A,B,C,D,E,F,G,H,I] = P,
  	blocks(A, B, C), 
  	blocks(D, E, F), 
  	blocks(G, H, I).
  	
%checks each block, see http://stackoverflow.com/questions/1768274/prolog-learning-by-example and swipl documentation
blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  	% all values in a block are distinct
  	alldifferent([A,B,C,D,E,F,G,H,I]),
  	blocks(Bs1, Bs2, Bs3).	

%helper predicate for printing
pretty_print(P) :-
	( foreach(Row,P) do
		write(Row),	nl
	).

