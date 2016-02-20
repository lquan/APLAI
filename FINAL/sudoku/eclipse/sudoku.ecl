:- lib(ic).
:- lib(matrix_util).
:- import all_different/1 from ic_global. %if omitted use from standard ic library

:- [sudoku_puzzles].

/**
 * Sudoku( ?Input )
 * ----------------
 * Input:	The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * ----------------
 * A sudoku is a number puzzle in which the numbers 1 through 9 must be placed into a grid 
 * of cells so that each row,column and box contains only one of each number.
 */
sudoku(P) :-
	sudoku(P,first_fail),
	pretty_print(P)
.

/**
 * Sudoku( ?Input, +VarOrder )
 * ---------------------------
 * Input:	The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * VarOrder: The Option used in the search.
 * ---------------------------
 * A is a number puzzle in which the numbers 1 through 9 must be placed into a grid 
 * of cells so that each row,column and box contains only one of each number.
 */
sudoku(P, VarOrder) :-
	statistics(times, [T0|_]),
	model(P),
	term_variables(P,AllVars),
	
	search(AllVars, 0, VarOrder, indomain, complete, [backtrack(BT)]), setval(backtracks, BT),
    
    statistics(times, [T1|_]),
    Secs is T1-T0,
    getval(backtracks, BTs),

    %pretty_print(P),
    printf("%.2f; %d\n", [Secs, BTs])
.

/**
 * model( ?P )
 * -----------
 * P:	The sudoku to be solved. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * -----------
 * The method models the sudoku, it forms the constraints used by the 'ic-solver':
 * - The possible values lay between 1 and 9.
 * - In every row, the values have to be unique.
 * - In every column, the values have to be unique.
 * - In every block, the values have to be unique.
 */
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
  	blocks(G, H, I)
.

/**
 * blocks( ?Row1 , ?Row2 , ?Row3 )
 * -------------------------------
 * Row1:	The first row of the blocks.
 * Row2:	The second row of the blocks.
 * Row3:	The third row of the blocks.
 * -------------------------------
 * The method checks every block of three, block per block, formed by the 3 given rows.
 * In every block, the values have to be unique.
 *
 * For more information, you are directed to the following url and the SWI-Prolog documentation;
 * http://stackoverflow.com/questions/1768274/prolog-learning-by-example
 */
blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  	alldifferent([A,B,C,D,E,F,G,H,I]),	  	% all values in a block are distinct
  	blocks(Bs1, Bs2, Bs3)
.	

/**
 * pretty_print( ?P )
 * ------------------
 * P:	The sudoku to be printed. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * ------------------
 * The method display the given input in a simple manner for the eye; every row under eachother.
 */
pretty_print(P) :-
	( foreach(Row,P) do
		writeln(Row)
	)
.
