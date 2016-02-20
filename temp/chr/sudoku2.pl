%http://dtai.cs.kuleuven.be/CHR/summerschool/contest/sudoku.chr

:- use_module(library(chr)).
:- chr_constraint cell/3, maybe/4, ff/1, solved/0.

maybe(_,_,_,0) <=> fail.
cell(X,Y,V) \ maybe(X,Y,L,1) <=> L = [V].
cell(X,Y,V) \ maybe(A,B,L,LL) <=> 
        sees(X,Y,A,B), ground(V), select(V,L,R) | 
        RL is LL-1, maybe(A,B,R,RL).


choice :- ff(1).
cell(X,Y,V) \ ff(LL), maybe(X,Y,L,LL) <=> member(V,L).
ff(LL) <=> LL1 is LL+1, ff(LL1).

maybe(_,_,_,_) \ solved <=> fail.
solved \ cell(_,_,_) <=> true.  % cleanup
solved <=> true.

solve :- choice, (solved -> true ; solve).

sudoku(Input) :-
    length(Input,N),
	numlist(1,N,D),
    make_vals(Input,1,1,D,N).

make_vals([],_,_,_,_).
make_vals([[]|L],_,Y,D,N) :- %was make_vals([[]|L],X,Y,D,N) :-
        Y1 is Y+1,
        make_vals(L,1,Y1,D,N).
make_vals([[E|Es]|L],X,Y,D,N) :-
        cell(X,Y,E),
	( var(E) ->
		maybe(X,Y,D,N)
	;
		true
	),
        X1 is X+1,
        make_vals([Es|L],X1,Y,D,N).

sees(X,_,X,_).          % same row
sees(_,Y,_,Y).          % same column
sees(X,Y,A,B) :-        % same box
        X//3 =:= A//3, 
        Y//3 =:= B//3.
        
        
%%%%%%%%%%%%%%%
% solves all the puzzles in sudoku_puzzles and print the runtime in milliseconds
:- [sudoku_puzzles].

solveAll :-
	findall(P,puzzles(P),Puzzles),
	solveAll(Puzzles).
solveAll([]).
solveAll([First|Rest]) :-
	statistics(runtime, [T0|_]),
	sudoku(First), solve,
	statistics(runtime, [T1|_]),
	Secs is T1-T0,
	writeln(Secs),
	solveAll(Rest).
        
