:- lib(ic_sets).
:- lib(ic_global).
:- import sumlist/2 from ic_global.
:- [sudoku_puzzles].

:- dynamic(cell/2).

/**
 * collectIndices( +Val , -List )
 * ------------------------------
 * Val: 	The value to search for the corresponding indices.
 * List:	A list containing the indices, which matches to the given value.
 * ------------------------------
 * The method collects all the indices from the dynamic predicate cell/2 where the value matches the given value <Val>.
 */
collectIndices(Val,L) :-
	findall(Idx, (cell(Idx,Val2), nonvar(Val2), Val =:= Val2), L)
.

/**
 * find_cells( -Cells )
 * --------------------
 * Cells:	A list of all the dynamic cell/2 in the program.
 * --------------------
 * The method gathers all the dynamic cell/2 in the program and returns then in the list <Cells>.
 */
find_cells(Cells) :-
	findall(C, C = cell(_,_), Cells)
.

/**
 * fill_in( ?P , +Vars, +Array )
 * -------------------------
 * P:		The sudoku to be solved. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * Vars: 	The set of position corresponding to the values in the sudoku. The first list corresponds to the value 1, the second to the value 2, enc..
 * Array:	The solved sudoku to be converted.
 * -------------------------
 * This method converts the alternative viewpoint of a sudoku, cf input <Array>, to the normal viewpoint, cf input <P>.
 * Every value of <Array> is converted and added to <P>.
 */
fill_in(P,Vars,Array) :-
	length(Vars,N),
	%first fill in the values in the array
	( for(I,1,N), foreach(Var,Vars), param(Array)	do
		( foreach(El,Var), param(Array,I) do
			I is Array[El]
		)
	),
	%then in the original list of lists
	( for(X,1,N), foreach(Row,P), param(Array,N) do
		( for(Y,1,N), foreach(El,Row), param(Array,N,X) do
			Z is Y + N * (X-1),  %formula to convert from (i,j) to (k) 
			El is Array[Z]
		)
	)
.

/**
 * sudoku( ?P )
 * ------------
 * P:	The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * ------------
 * A sudoku is a number puzzle in which the numbers 1 through 9 must be placed into a grid 
 * of cells so that each row,column and box contains only one of each number.
 */
sudoku(P) :-
	sudoku(P,_)
.	

/**
 * sudoku( ?P , ?Vars)
 * ------------
 * P:		The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9.
 * Vars:	The set of position corresponding to the values in the sudoku. The first list corresponds to the value 1, the second to the value 2, enc..
 * ------------
 * A sudoku is a number puzzle in which the numbers 1 through 9 must be placed into a grid 
 * of cells so that each row,column and box contains only one of each number.
 * 
 * It also returns the solution in the alternative viewpoint
 */
sudoku(P,Vars) :-
	statistics(times, [T0|_]),
	
	model(P,Vars,Array),	
	label_sets(Vars),
	
	statistics(times, [T1|_]),
	Secs is T1-T0,
	printf("%.2f\n", Secs),
	
	fill_in(P,Vars,Array),
	retractall(cell(_,_))  		%cleanup of cell predicates	
. 

/**
 * model( +P, ?Vars , -Array )
 * --------------------------
 * P:		The sudoku to be solved. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * Vars:	The set of position corresponding to the values in the sudoku. The first list corresponds to the value 1, the second to the value 2, enc..
 * 			( Vars is a list of 9 sets containing numbers between 1 and 81 )
 * Array	An array used to processes the in and output of the program. The switch between the two viewpoints.
 * --------------------------
 * The method models the constraints for the sudoku;
 * For more information of how the method works, we direct you to the subsection 1.3.1 of the report.
 */ 
model(P,Vars,Array):- 
	length(P,N), N is 9, N2 is N*N, 	%input must be 9 list of lists, i.e. standard sudoku
	flatten(P, P2),	Array =.. [[]|P2],	%flattened list is easier to work with usually, except for direct indexing
	intsets(Vars,N,1,N2),				%make list of length 9 with vars representing our numbers 1,..,9 with domain 1..81
										%Vars is a list of N sets containing numbers between 1 and 81 
	
	(for(I,1,N2), foreach(El,P2) do
		( nonvar(El) -> assert(cell(I,El)) ; true )		%initial values	
	),
	
	all_disjoint(Vars),
	( for(I,1,N), foreach(Var,Vars), param(N) do
		collectIndices(I,L),             % initial filled in 
		( foreach(J,L), param(Var) do
			in(J,Var) 					 % power set of {1..81} with already filled in
		),	
		#(Var,N),			 			 % cardinality constraint
				
		in(1,Var,A1),  in(2,Var,A2),	in(3,Var,A3),
		in(10,Var,A4), in(11,Var,A5),   in(12,Var,A6),
		in(19,Var,A7), in(20,Var,A8),   in(21,Var,A9),

		in(4,Var,B1),  in(5,Var,B2),	in(6,Var,B3),
		in(13,Var,B4), in(14,Var,B5),   in(15,Var,B6),
		in(22,Var,B7), in(23,Var,B8),   in(24,Var,B9),

		in(7,Var,C1),  in(8,Var,C2),	in(9,Var,C3),
		in(16,Var,C4), in(17,Var,C5),   in(18,Var,C6),
		in(25,Var,C7), in(26,Var,C8),   in(27,Var,C9),

		in(28,Var,D1), in(29,Var,D2),	in(30,Var,D3),
		in(37,Var,D4), in(38,Var,D5),   in(39,Var,D6),
		in(46,Var,D7), in(47,Var,D8),   in(48,Var,D9),
		
		in(31,Var,E1), in(32,Var,E2),	in(33,Var,E3),
		in(40,Var,E4), in(41,Var,E5),   in(42,Var,E6),
		in(49,Var,E7), in(50,Var,E8),   in(51,Var,E9),

		in(34,Var,F1), in(35,Var,F2),	in(36,Var,F3),
		in(43,Var,F4), in(44,Var,F5),   in(45,Var,F6),
		in(52,Var,F7), in(53,Var,F8),   in(54,Var,F9),

		in(55,Var,G1), in(56,Var,G2),	in(57,Var,G3),
		in(64,Var,G4), in(65,Var,G5),   in(66,Var,G6),
		in(73,Var,G7), in(74,Var,G8),   in(75,Var,G9),

		in(58,Var,H1), in(59,Var,H2),	in(60,Var,H3),
		in(67,Var,H4), in(68,Var,H5),   in(69,Var,H6),
		in(76,Var,H7), in(77,Var,H8),   in(78,Var,H9),
		
		in(61,Var,I1), in(62,Var,I2),	in(63,Var,I3),
		in(70,Var,I4), in(71,Var,I5),   in(72,Var,I6),
		in(79,Var,I7), in(80,Var,I8),   in(81,Var,I9),
		
		%blocks
		sumlist([A1,A2,A3,A4,A5,A6,A7,A8,A9],1),
		sumlist([B1,B2,B3,B4,B5,B6,B7,B8,B9],1),
		sumlist([C1,C2,C3,C4,C5,C6,C7,C8,C9],1),
		
		sumlist([D1,D2,D3,D4,D5,D6,D7,D8,D9],1),
		sumlist([E1,E2,E3,E4,E5,E6,E7,E8,E9],1),
		sumlist([F1,F2,F3,F4,F5,F6,F7,F8,F9],1),
		
		sumlist([G1,G2,G3,G4,G5,G6,G7,G8,G9],1),
		sumlist([H1,H2,H3,H4,H5,H6,H7,H8,H9],1),
		sumlist([I1,I2,I3,I4,I5,I6,I7,I8,I9],1),
		
		%rows
		sumlist([A1,A2,A3,B1,B2,B3,C1,C2,C3],1),
		sumlist([A4,A5,A6,B4,B5,B6,C4,C5,C6],1),
		sumlist([A7,A8,A9,B7,B8,B9,C7,C8,C9],1),
		
		sumlist([D1,D2,D3,E1,E2,E3,F1,F2,F3],1),
		sumlist([D4,D5,D6,E4,E5,E6,F4,F5,F6],1),
		sumlist([D7,D8,D9,E7,E8,E9,F7,F8,F9],1),
		
		sumlist([G1,G2,G3,H1,H2,H3,I1,I2,I3],1),
		sumlist([G4,G5,G6,H4,H5,H6,I4,I5,I6],1),
		sumlist([G7,G8,G9,H7,H8,H9,I7,I8,I9],1),

		%columns
		sumlist([A1,A4,A7,D1,D4,D7,G1,G4,G7],1),
		sumlist([A2,A5,A8,D2,D5,D8,G2,G5,G8],1),
		sumlist([A3,A6,A9,D3,D6,D9,G3,G6,G9],1),
		
		sumlist([B1,B4,B7,E1,E4,E7,H1,H4,H7],1),
		sumlist([B2,B5,B8,E2,E5,E8,H2,H5,H8],1),
		sumlist([B3,B6,B9,E3,E6,E9,H3,H6,H9],1),
		
		sumlist([C1,C4,C7,F1,F4,F7,I1,I4,I7],1),
		sumlist([C2,C5,C8,F2,F5,F8,I2,I5,I8],1),
		sumlist([C3,C6,C9,F3,F6,F9,I3,I6,I9],1)
	)
.


/**
 * label_sets( +Set )
 * ------------------
 * Set: The set to be labelled.
 * ------------------
 * The method lebals every variable in the set in the ic_set library.
 */
label_sets([]).
label_sets([S|Ss]) :-
	insetdomain(S,_,_,_), %insetdomain(?Set, ?CardSel, ?ElemSel, ?Order)  elemsel = random is maybe choice
	label_sets(Ss)
.	
	
