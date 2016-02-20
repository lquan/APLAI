
:- use_module(library(chr)).
:- chr_constraint cell/2, maybe/3, ff/1, solved/0, cleanup/0.


rememberCell @ cell(N,Vals) \ cell(N,Vals2) <=> Vals=Vals2.


cell(X,V) \ maybe(X,[L],_) <=> getGrounded(V,O),length(O,8)| assignLocation(X,L).
cell(X,V) \ maybe(X,_,_) <=> getGrounded(V,O),length(O,9) | true.
maybe(_,_,0) <=> fail.
cell(X,V) \ maybe(X,L,LL) <=>
        member(Elem,V),nonvar(Elem),
	member(Diff,L),
	sees(Elem,Diff), select(Diff,L,R) |
	RL is LL-1, maybe(X,R,RL).

cell(_,V) \ maybe(A,L,LL) <=>
	member(Elem,V),nonvar(Elem),
	select(Elem,L,R) |
	RL is LL-1, maybe(A,R,RL).

cell(X,_) \ ff(LL), maybe(X,L,LL) <=>
	 select(D,L,R),assignLocation(X,D)|
	 RL is LL-1,maybe(X,R,RL).

ff(LL) <=> LL1 is LL+1, ff(LL1).


maybe(_,_,_) \ solved <=> fail.
solved <=> true.
cleanup \ cell(_,_) <=> true.

solve(P) :- get_single_char(_),writeOutput(P),(solved -> true; choice,solve(P)).
solve :- (solved ->  true ; choice,solve).
choice :- ff(1).

sudoku(Input,Output) :-
    numlist(1,81,D),
    forCell(1,9,Output),
    make_vals(Input,1,D),!.
writeOutput([]).
writeOutput([Output|Rest]) :-
	getGrounded(Output,O),
	writeln(O),
	writeOutput(Rest).


forCell(Max,Max,[Cell]) :- createCell(Max,Cell).
forCell(Cur,Max,[H|T]) :-
	Curn is Cur+1,
	forCell(Curn,Max,T),
	createCell(Cur,H).

createCell(Cell,D) :-
	length(D,9),
	cell(Cell,D),
	numlist(1,81,Maybe),
	maybe(Cell,Maybe,81).

make_vals([],_,_).

make_vals([[]|L],Count,D) :-
	make_vals(L,Count,D).
make_vals([[E|Es]|L],Count,D) :-
	(   var(E) ->
	  NewD = D
	;
	  assignLocation(E,Count),
	  select(Count,D,NewD)

	),
	NewCount is Count+1,
	make_vals([Es|L],NewCount,NewD).

assignLocation(Cell,Value) :-
	cell(Cell,Vals),
	X is (Value+8)//9,
	nth1(X,Vals,Value).

sees(X,Y) :-
	toXY(X,X1,Y1),
	toXY(Y,X2,Y2),
	sees(X1,Y1,X2,Y2).

sees(X,_,X,_) :- true.          % same row
sees(_,Y,_,Y) :- true.          % same column
sees(X,Y,A,B) :-        % same box
	(X-1)//3 =:= (A-1)//3,
        (Y-1)//3 =:= (B-1)//3.


toXY(Val,X,Y) :-
	X is (Val+8)//9,
	Y is Val-((Val-1)//9)*9.

getGrounded([],[]).
getGrounded([H|T],Tmp) :-
	(   nonvar(H) ->
	    [H1|Out] = Tmp,
	    H1=H,
	    NO = Out
	;
	NO = Tmp
	),
	getGrounded(T,NO).

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

/**************/
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
	consider(Rest,T,Count,Value).
consider([[Es|E]|Rest],[H|T],H,Value) :-
	Es=Value,
	NewC is H+1,
	consider([E|Rest],T,NewC,Value).

consider([[_|Tail]|Rest],[H|T],Count,Value) :-
	Count \= H,!,
	NewC is Count+1,
	consider([Tail|Rest],[H|T],NewC,Value).








