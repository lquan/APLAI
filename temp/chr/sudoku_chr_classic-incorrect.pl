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

solve :- (solved -> true ; choice, solve),  !.

sudoku(Input) :-
        length(Input,N),
		numlist(1,N,D),
        make_vals(Input,1,1,D,N).

make_vals([],_,_,_,_).
make_vals([[]|L],_,Y,D,N) :-
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
        (X-1)//3 =:= (A-1)//3, 
        (Y-1)//3 =:= (B-1)//3.

%helper predicate for printing
pretty_print([]).
pretty_print([P|Ps]) :-
	writeln(P),
	pretty_print(Ps). 

test :- medium(P), solveSudoku(P).  
solveSudoku(P) :- sudoku(P), solve, pretty_print(P).      
%%%%%%%%%%%%%%%
% solves all the puzzles in sudoku_puzzles and print the runtime in milliseconds
:- [sudoku_puzzles].

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
        
