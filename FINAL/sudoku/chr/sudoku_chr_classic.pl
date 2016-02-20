:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_constraint cell/3, maybe/4, ff/1, solved/0.

/* If two cells are the same, keep one */
rememberCell @ cell(X,Y,Val) , cell(X,Y,Val2) <=> Val = Val2 | cell(X,Y,Val).

/* If there is a maybe whose possible values are empty, then fail */
maybe(_,_,_,0) <=> fail.

/* If a cell is in the same row|column|box with an other cell who has the same value, then fail */
cell(X,Y,V),cell(A,B,V) <=> nonvar(V), sees(X,Y,A,B) | fail.
/* If an unassigned cell and a 'maybe' for that cell exist which has only one possible value left, assign the value to the cell */
cell(X,Y,V), maybe(X,Y,[T],1) <=> var(V) | V is T, cell(X,Y,T).
/* A cell, who has a value assigned, removes his value out of the maybe's which are in the same row|column|box */
cell(X,Y,V) \ maybe(A,B,T,C) <=> nonvar(V), sees(X,Y,A,B), member(V,T) | select(V,T,R), C1 is C-1, maybe(A,B,R,C1).


/* When solving, a choice is to be made. It's chances the maybe with the least possible values to a member\2 assign */
choice :- ff(1).
cell(X,Y,V) \ ff(LL), maybe(X,Y,L,LL) <=> member(V,L) .
ff(LL) <=> LL1 is LL+1, ff(LL1).

/* If the program is solved, but there are still 'maybees', then fail */
maybe(_,_,_,_) \ solved <=> fail.
/* If the program is solved, do a cleanup, remove all cells out the chr pool */
solved \ cell(_,_,_) <=> true.
/* A cleanup necessary for the predicate solveAll\/1 , if not done the predicate would return false*/
solved <=> true.

/* If solved return true, else make a choice and try to solve again */
solve :- (solved -> true ; choice, solve,! ),!.


/**
 * Sudoku( ?Input )
 * ----------------
 * Input:	The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * ----------------
 * A sudoku is a number puzzle in which the numbers 1 through 9 must be placed into a grid 
 * of cells so that each row,column and box contains only one of each number.
 */
sudoku(Input) :-
	length(Input,N),
	make_vals(Input,1,1,N)
.

/**
 * make_vals( ?Input, +X, +Y, +Dimension )
 * ---------------------------------------
 * Input: The sudoku to solve. Form: [[..],[..],..,[..]]. Dimension: 9x9
 * X: The X coordinate of the current cell handled.
 * Y: The Y coordinate of the current cell handled.
 * Dimension: The row dimension of the sudoku, should be 9 to work properly.
 * ---------------------------------------
 * The method iterates over the coordinates of 'Input'.
 * For each coordinate, a 'chr cell\/3' is created,
 * and for every unknown value in the input a 'chr maybe\/3' is created with
 * a list of possible values (these numlist is 1..9 where chr will 
 * remove impossible values).
 */
make_vals([],_,_,_).
make_vals([[]|L],_,Y,N) :- 
        Y1 is Y+1,
        make_vals(L,1,Y1,N)
.
make_vals([[E|Es]|L],X,Y,N) :-
	cell(X,Y,E),
	( var(E) ->
		numlist(1,N,D),
		maybe(X,Y,D,N)
	;
		true
	),
    X1 is X+1,
    make_vals([Es|L],X1,Y,N)
.

/**
 * sees( +X1, +Y1 , +X2 , +Y2 )
 * ----------------------------
 * X1: The X-coordinate of the first cell
 * Y1: The Y-coordinate of the first cell
 * X2: The X-coordinate of the second cell
 * Y2: The Y-coordinate of the second cell
 * ---------------------------------------
 * The method checks if the first cell is in the same row,
 * or the same column, of the same 9grid-box as the second cell.
 * If so, it returns true, else false.
 */
sees(X,_,X,_) :- !.     % same row
sees(_,Y,_,Y) :- !.     % same column
sees(X,Y,A,B) :-        % same box
        (X-1)//3 =:= (A-1)//3, 
        (Y-1)//3 =:= (B-1)//3
.

/**
 * printOut( ?Output )
 * -------------------
 * Output: The output to be displayed.
 * -------------------
 * The method is a simple output of the given output,
 * for each array it prints on a new line.
 */
printOut([]).
printOut([Output|Rest]) :-
	writeln(Output),
	printOut(Rest)
.
        


%%%%%%%%%%%%%%%
% solves all the puzzles in sudoku_puzzles and print the runtime in milliseconds
:- [sudoku_puzzles].

/**
 * solveAll
 * --------
 * Solves all the puzzles in the given file (sudoku_puzzles.pl).
 * and displays the runtime in milliseconds for every puzzle .
 */
solveAll :-
	findall(P,puzzles(P),Puzzles),
	solveAll(Puzzles).
solveAll([]).
solveAll([P|Ps]) :-
	statistics(runtime, [T0|_]),
	sudoku(P), solve,
	statistics(runtime, [T1|_]),
	Secs is T1-T0,
	writeln(Secs),
	solveAll(Ps).
        
/*
Some examples where on which the sudoku program is tested and manually checked. 
---------------------

medium(P),sudoku(P),solve
9 4 5  6 2 1  3 8 7 
2 1 3  5 8 7  9 4 6 
7 6 8  4 3 9  2 5 1 

4 3 7  9 5 6  8 1 2 
1 8 9  3 4 2  6 7 5 
6 5 2  7 1 8  4 9 3 

8 7 6  1 9 3  5 2 4 
5 9 1  2 6 4  7 3 8 
3 2 4  8 7 5  1 6 9
-------------------
difficult(P)
5 9 4  1 6 7  8 2 3 
7 2 3  8 4 5  1 9 6 
8 6 1  9 3 2  5 4 7 

2 4 7  5 1 6  3 8 9 
9 3 6  7 2 8  4 1 5 
1 5 8  3 9 4  6 7 2 

4 1 2  6 7 3  9 5 8 
3 8 9  2 5 1  7 6 4 
6 7 5  4 8 9  2 3 1

-------------------
verydifficult(P)
5 9 8  7 3 1  2 4 6
4 3 6  2 8 9  5 7 1
1 2 7  5 4 6  9 3 8

6 8 2  1 9 3  4 5 7
9 4 3  8 5 7  6 1 2
7 1 5  6 2 4  3 8 9

3 7 1  4 6 2  8 9 5
8 6 4  9 7 5  1 2 3
2 5 9  3 1 8  7 6 4

--------------------
expert(P)
6 7 2  1 4 9  8 3 5
3 9 5  8 2 6  1 4 7
4 8 1  7 5 3  6 2 9

5 2 3  6 1 8  9 7 4
7 4 6  5 9 2  3 1 8
9 1 8  3 7 4  5 6 2

2 3 4  9 6 5  7 8 1
8 5 7  4 3 1  2 9 6
1 6 9  2 8 7  4 5 3

--------------------
lambda(P)
1 8 4  9 6 3  7 2 5
5 6 2  7 4 8  3 1 9
3 9 7  5 1 2  8 6 4

2 3 9  6 5 7  1 4 8
7 5 6  1 8 4  2 9 3
4 1 8  2 3 9  6 5 7 

9 4 1  3 7 6  5 8 2
6 2 3  8 9 5  4 7 1
8 7 5  4 2 1  9 3 6

-------------------
skip
-------------------
hardest(P)
1 7 4  3 8 5  9 6 2
2 9 3  4 6 7  1 5 8
5 8 6  1 9 2  7 3 4

4 5 1  9 2 3  8 7 6
9 2 8  6 7 4  3 1 5
3 6 7  8 5 1  2 4 9

7 1 9  5 4 8  6 2 3
6 3 5  2 1 9  4 8 7
8 4 2  7 3 6  5 9 1

-------------------
skip
-------------------
extra4(P)
1 8 4  9 6 3  7 2 5
5 6 2  7 4 8  3 1 9
3 9 7  5 1 2  8 6 4

2 3 9  6 5 7  1 4 8
7 5 6  1 8 4  2 9 3
4 1 8  2 3 9  6 5 7

9 4 1  3 7 6  5 8 2
6 2 3  8 9 5  4 7 1
8 7 5  4 2 1  9 3 6

*/
