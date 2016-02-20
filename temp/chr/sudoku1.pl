%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A CHR program for solving Sudoku problems
% 
% Derived on Nov 18, 2005 by Thom Fruehwirth from the procedural solver
% of Jon Murua Gonzalez and Henning Christiansen (c)
% by making it declarative, use more CHR. It became much shorter and faster.
% Bug fixed by Jon Sneyers.
%
% This program solves Sudoku problems in very short time
% algorithm based on the heuristics try-most-constrained-field-first (first failure).
%
% It has been tested under SICStus Prolog and SWI-Prolog
%
% The board is considered as a 4-dimensional cube with dimensions
% {a,b,c} x {a,b,c} x {1,2,3} x {1,2,3}
% where the letters identify a "big square", each of which has 3x3 atomic squares
% identified by a pait of numbers.
% For example, upper left atomic square is identified as (a,a,1,1)
%
% To use the program, enter the initially given numbers for your problem
% by modifying the 'addinitial' predicate at the bottom of the file and call
% ?- solve.

% A state of the board is represented by 81 f(ield) constraints of the form
% f( 4 x Coordinates, ListLength, ListOfPossibleValues)
% ListOfPossibleValues indicates which numbers that can be placed in this atomic field
% without violating the rules of the game.
% If ListLength=1, then ListOfPossibleValues contains the unqiue value of the field
% and we replace it by f/5 (for efficency)
% [ bugfix by Jon Sneyers: only replace f/6 by f/5 at the next fillone(1),
%   so the N1>0 tests are meaningful.    - Dec. 2005]

:- use_module(library(chr)).
:- use_module(library(lists)).


%:- chr_option(inplace_updates,on).
%:- chr_option(suspension_reuse,on).
%:- chr_option(reduced_garbage,off).
%:- chr_option(suspension_reuse_profiling,on).


:- chr_constraint f(+,+,+,+,+int,+list(int)),
               fillone(+int),             
	       f(+,+,+,+,+int),            
	       print4(+,+,+,+).
%:- chr_constraint f/6, fillone/1, f/5, print4/4.
:- chr_type list(X) ---> [] ; [X | list(X)].


fillone(N), f(A,B,C,D,N2,L)#Id <=> N2=N | 
    member(V,L), f(A,B,C,D,V), fillone(1) pragma passive(Id).
fillone(N) <=> N < 9 | N1 is N+1, fillone(N1).
fillone(_) <=> true.

f(A,B,C,D,_) \ f(A,B,C,D,_,_)#Id <=> true pragma passive(Id).

% same column
f(_,B,_,D,V) \ f(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, f(A,B,C,D,N1,LL) pragma passive(Id).
% same row
f(A,_,C,_,V) \ f(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, f(A,B,C,D,N1,LL) pragma passive(Id).
% same box
f(A,B,_,_,V) \ f(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, f(A,B,C,D,N1,LL) pragma passive(Id).






bench :- 
	measure((init_board,init_data,tryall), true, Ufib,GBfib,_) ->
        statistics,
	format('time: ~d (~d)',[Ufib,GBfib]),
	nl,
	fail.
bench.

tryall :- fillone(1), 
	  fail.
tryall.

tryall(C) :- fillone(1),updatecounter, fail.
tryall(C) :- nb_getval(counter,C).
updatecounter :- nb_getval(counter,C), C1 is C+1, 
                (0 is C1 mod 1000       ->
                    D is C1 // 1000,
                    write(D),
                    write(' thousand solutions generated...'),nl
                ;
                    true
                ),
                nb_setval(counter,C1).



init_board :-  fill1(a), fill1(b), fill1(c).
fill1(X) :- fill2(X,a),fill2(X,b),fill2(X,c).
fill2(X,Y) :- fill3(X,Y,1), fill3(X,Y,2), fill3(X,Y,3).
fill3(A,B,C) :- fill4(A,B,C,1), fill4(A,B,C,2), fill4(A,B,C,3).
fill4(A,B,C,D) :- f(A,B,C,D,9,[1,2,3,4,5,6,7,8,9]).

% NB in different enumeration order - why?
printsolution:- print1(a), print1(b), print1(c).
print1(X) :- print2(X,1),print2(X,2),print2(X,3), nl.
print2(X,Y) :- print3(X,Y,a), print3(X,Y,b), print3(X,Y,c), nl.
print3(A,B,C) :- print4(A,B,C,1), print4(A,B,C,2), print4(A,B,C,3), write(' ').

print4(A,B,C,D), f(A,C,B,D,Val) <=> write(Val).
print4(A,B,C,D) <=> write('.').

:- nb_setval(counter,0).

init_data :- f(a,a,1,1,1),  f(a,a,1,2,2),  f(a,a,1,3,3),
             f(a,a,2,1,4),  f(a,a,2,2,5),  f(a,a,2,3,6),
             f(a,a,3,1,7),  f(a,a,3,2,8),  f(a,a,3,3,9),
             f(b,b,1,1,1),  f(b,b,1,2,2),  f(b,b,1,3,3),
             f(b,b,2,1,4),  f(b,b,2,2,5),  f(b,b,2,3,6),
             f(b,b,3,1,7),  f(b,b,3,2,8),  f(b,b,3,3,9),
             f(c,c,1,1,1),  f(c,c,1,2,2),  f(c,c,1,3,3),
             f(c,c,2,1,4),  f(c,c,2,2,5),  f(c,c,2,3,6),
             f(c,c,3,1,7),  f(c,c,3,2,8),  f(c,c,3,3,9).

measure(G,_,Time,GTime,swi) :-
	cputime(X),	gctime(Y),
	call(G),
	cputime(Now),	gctime(NowG),
	GTime is NowG-Y,
	Time is Now-X-GTime.
cputime(Time) :- statistics(runtime, [Time,_]).
gctime(Time) :- statistics(garbage_collection, [_,_,Time]).

