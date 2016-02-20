:- use_module(library(chr)).
:- chr_constraint cell/2, maybe/3, ff/1, solved/0, missing/3.


%rememberCell @ cell(Num,Val) , cell(Num2,Val) <=> Num = Num2 | cell(Num,Val).


maybe(_,_,0) <=> fail.

cell(V,Cell), cell(V,Cell2) <=> nonvar(Cell),nonvar(Cell2),Cell \== Cell2, sees(Cell,Cell2) | fail.
cell(V,Cell) \ maybe(V,[T],1) <=> var(Cell) | Cell is T.
cell(V,Cell) \ maybe(V,Cell2,C) <=> nonvar(Cell), select(Cell3,Cell2,R), sees(Cell,Cell3) | C1 is C-1, maybe(V,R,C1).
cell(_,Cell) \ maybe(V,Cell2,C) <=> nonvar(Cell) , member(Cell,Cell2) | select(Cell, Cell2,R), C1 is C-1, maybe(V,R,C1).

%maybe(V,Cell,_) \ maybe(V,Cell2,C2) <=> select(Cell3,Cell,R), member(Cell3,Cell2) | CC2 is C2 -1, maybe(V,R,CC2).


choice :- ff(1).

%cell(V,Cell) \ ff(LL), maybe(V,L,LL) <=> member(Cell,L) .
%cell(V,Cell) \ ff(LL), maybe(V,L,LL) <=> var(Cell),select(Cell,L,_)| cell(V,Cell).%, RL is LL-1, maybe(V,R,RL).
%cell(V,Cell) \ ff(LL), maybe(V,L,LL) <=> var(Cell)| member(Cell,L).
cell(V,Cell) \ ff(C), maybe(V,L,C)  <=> var(Cell) | select(Cell, L, R), C1 is C-1, maybe(V,R,C1).
%grmbl maybe(V,Cell,_) , ff(_) <=> nonvar(Cell) | select(Cell2, Cell,_) , cell(V,Cell2).
ff(LL) <=> LL1 is LL+1, ff(LL1).

maybe(_,_,_) \ solved <=> fail.
solved \ cell(_,_) <=> true.
solved <=> true.

solve :- (solved -> true ; choice, solve,! ),!.

/* Sudoku */
sudoku(Input,Output) :-
	length(Input,L),
	N is L * L,
    forCell(1,L,Output),	% zet het om naar andere viewpoint
	matchInputWithOutput(Input,1,Output),
    make_vals(Output,1,N),
	!
%	trySolution(Output,1),
%	trySolution(Output,2),
%	trySolution(Output,3),
%	trySolution(Output,4),
%	trySolution(Output,5),
%	trySolution(Output,6),
%	trySolution(Output,7),
%	trySolution(Output,8),
%	trySolution(Output,9)
.

trySolution([],_).
trySolution([[E|Es]|R],Count) :-
	( solved ->
		true
	;
		( nonvar(E) ->
			trySolution([Es|R],Count)
		;
			maybe(Count,Cell,_),
			select(E,Cell,_),
			write(Count),write(' '),writeln(E),
			cell(Count,E)
		)
	)
.

matchInputWithOutput([],_,_).
matchInputWithOutput([[]|L],Count,Output) :-
	matchInputWithOutput(L,Count,Output),
	!
.
matchInputWithOutput([[E|Es]|L],Count,Output) :-
	( var(E) ->
		true
	;
		nth1(E,Output,PositionList),
		addPositionToList(Count,PositionList)
	),
	Count2 is Count +1,
	matchInputWithOutput([Es|L],Count2,Output)
.

addPositionToList(_,[]) :- fail.
addPositionToList(Position,[L|List]) :-
	( var(L) ->
		L is Position
	;
		addPositionToList(Position,List)
	)
.

/* per value een lijst aanmaken met mogelijke posities */
forCell(N,N,[Cell]) :- length(Cell,N).
forCell(Cur,N,[H|T]) :-  %forCell(1,2,Out).  >>> Out = [[_G1068156, _G1068159], [_G1068147, _G1068150]]
	Curn is Cur+1,
	forCell(Curn,N,T),
	length(H,N),
	!
.	



make_vals([],_,_).
make_vals([[]|L],Count,N) :-
	Count2 is Count +1,
	numlist(1,N,D),
	maybe(Count,D,N),
	make_vals(L,Count2,N)
.
make_vals([[E|Es]|L],Count,N) :-
	cell(Count,E),
	make_vals([Es|L],Count,N)
.




%assignLocation(Cell,Value) :-
%	cell(Cell,Vals),
%	X is (Value+8)//9,
%	nth1(X,Vals,Value)
%.

sees(X,Y) :-
	toXY(X,X1,Y1),
	toXY(Y,X2,Y2),
	sees(X1,Y1,X2,Y2)
.

sees(X,_,X,_) :- !.          % same row
sees(_,Y,_,Y) :- !.          % same column
sees(X,Y,A,B) :-        % same box
	(X-1)//3 =:= (A-1)//3,
    (Y-1)//3 =:= (B-1)//3
.


toXY(Val,X,Y) :-
	X is (Val+8)//9,
	Y is Val-((Val-1)//9)*9
.



writeOutput([]).
writeOutput([Output|Rest]) :-
	writeln(Output),
	writeOutput(Rest)
.

printOutput(Out) :- length(Out,N), N2 is N*N, length(Out2,N2), printOut(Out,Out2,1).
printOut([],Out2,Count):- printOut2(Out2,1,Count).
printOut([[]|L],Out2,Count) :- Count2 is Count+1, printOut(L,Out2,Count2).
printOut([[Out|R]|Rest],Out2,Count) :-
	( nonvar(Out) -> nth1(Out,Out2,Count) ; true ),
	printOut([R|Rest],Out2,Count)
.

printOut2([],_,_) :- !.
printOut2(Es,N,N) :- nl, printOut2(Es,1,N),!.
printOut2([E|Es],Count,N) :-
	( var(E) ->
		write( ?)
	;
		write(E)
	),
	write(' '),
	Count2 is Count+1,
	printOut2(Es,Count2,N)
.


%%%%%%%%%%%%%%%
% solves all the puzzles in sudoku_puzzles and print the runtime in milliseconds
:- [sudoku_puzzles].

solveAll :-
	findall(P,puzzles(P),Puzzles),
	solveAll(Puzzles).
solveAll([]).
solveAll([First|Rest]) :-
	statistics(runtime, [T0|_]),
	sudoku(First,_), solve,
	statistics(runtime, [T1|_]),
	Secs is T1-T0,
	writeln(Secs),
	solveAll(Rest).


/*
consider(_,10).

consider(Input,Val) :-
	ValN is Val+1,
	consider(Input,ValN),
	cell(Val,Locs),
	getGrounded(Locs,Vals),
	consider(Input,Vals,1,Val).

consider([],_,_,_).
consider(_,[],_,_).
consider([[]|Rest],T,Count,Value) :-
	!,consider(Rest,T,Count,Value).
consider([[Es|E]|Rest],[H|T],H,Value) :-
	!,
	Es=Value,
	NewC is H+1,
	consider([E|Rest],T,NewC,Value).

consider([[_|Tail]|Rest],[H|T],Count,Value) :-
	Count \= H,!,
	NewC is Count+1,
	consider([Tail|Rest],[H|T],NewC,Value).

getGrounded([],[]).
getGrounded([H|T],Tmp) :-
	(   ground(H) ->
	    [H1|Out] = Tmp,
	    H1=H,
	    NO = Out
	;
	NO = Tmp
	),
	getGrounded(T,NO)
.
*/
%


/*

medium(P),sudoku(P,O),solve, printOutput(O).
--------------------------------------------
medium(P)			>>> zit dus nog ergens foutje in , zet aantal zelde waarden in zelfde output!
9 4 5  6 8 2  1 7 3   missing: 	3x 4
2 1 3  5 x 7  9 4 6 			2x 1
6 7 8  1 3 9  2 5 x				3x 7
								1x 3
1 3 6  9 5 x  8 ? 2 			1x 8
8 x 9  2 1 x  6 x 5 
4 5 2  3 6 8  x 9 1 

7 8 1  x 9 3  5 2 4 
5 9 x  x 2 4  7 3 8 
3 2 4  8 7 5  x 6 9


medium(P);
regel 23:
cell(V,Cell) \ ff(C), maybe(V,L,C)  <=> var(Cell) | select(Cell, L, R), C1 is C-1, maybe(V,R,C1).
ipv
cell(V,Cell) \ ff(_), maybe(V,L,C)  <=> var(Cell) | select(Cell, L, R), C1 is C-1, maybe(V,R,C1).
9 4 3  6 2 8  1 7 x   missing:	1x 3
2 1 5  x x 7  9 4 6 			1x 6
6 7 8  4 3 9  2 5 x				2x 8
								3x 1
4 3 6  9 5 1  8 x 2 			1x 7
8 x 9  3 4 2  6 1 7 			2x 5
7 5 2  x 8 6  3 9 4 

5 2 7  1 9 3  4 8 x 
1 9 x  2 6 4  7 3 5 
3 6 4  8 7 5  x 2 9


medium(P)
OPLOSSING
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
*/


