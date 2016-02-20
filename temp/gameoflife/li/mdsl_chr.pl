% use the chr library of SWI-Prolog
% and setting some compiler options
% ?- mdsl(N).  solves the MDSL problem for a board of size N.

:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).

% declarations to enable compiler optimizations
:- chr_constraint cell(+cellID,+val),       % cellID gets asigned 1 (live) or 0 (dead)
                  cell(+cellID,+pos,+pos),  % location of cellID
                  nbs(+gID,+list(cellID)),  % list of neighbors of a cell
                  sumgeq(+list(cellID),+int),        % sum of the cell values is at least X
                  sumleq(+list(cellID),+int),        % sum of the cell values is at most X
                  sumneq(+list(cellID),+int).        % sum of the cell values is anything but X
:- chr_type val ---> 0 ; 1.
:- chr_type cellID == int.
:- chr_type gID == any.
:- chr_type pos == int.
:- chr_type list(X) ---> [] ; [X|list(X)].

% the constraints of a still life game 
cell(X,1), nbs(X,XNB) ==> sumgeq(XNB,2), sumleq(XNB,3).
cell(X,0), nbs(X,XNB) ==> sumneq(XNB,3).

% some more chr rules to deal a.o. with sumgeq/2, ...
%nbs(allcells,[X]), nbs(allcells,L) <=> nbs(allcells,[X|L]).
nbs(I,[X]), nbs(I,L) <=> \+ member(X,L) | nbs(I,[X|L]).

cell(I1,X1,Y), cell(I2,X2,Y) ==> X2 - X1 =:= 1 | nbs(I1,[I2]), nbs(I2,[I1]).
cell(I1,X,Y1), cell(I2,X,Y2) ==> Y2 - Y1 =:= 1 | nbs(I1,[I2]), nbs(I2,[I1]).
cell(I1,X1,Y1), cell(I2,X2,Y2) ==> abs(X2 - X1) =:= 1, Y2 - Y1 =:= 1 | nbs(I1,[I2]), nbs(I2,[I1]).

cell(X,V1) \ cell(X,V2) <=> V1 = V2.

sumgeq(L,S1) \ sumgeq(L,S2) <=> S1 >= S2 | true.
sumleq(L,S1) \ sumleq(L,S2) <=> S1 =< S2 | true.
sumneq(L,S) \ sumneq(L,S) <=> true.

sumgeq([],S) <=> S =< 0.
cell(I,Val) \ sumgeq(L,S) <=> select(I,L,Ls) | S2 is S - Val, sumgeq(Ls,S2).

sumleq([],S) <=> S >= 0.
cell(I,Val) \ sumleq(L,S) <=> select(I,L,Ls) | S2 is S - Val, sumleq(Ls,S2).

sumneq([],S) <=> S \== 0.
cell(I,Val) \ sumneq(L,S) <=> select(I,L,Ls) | S2 is S - Val, sumneq(Ls,S2).

% init a board
m(N) :- make_board(1,1,N,0).
make_board(X,Y,N,_) :-
    X > N, Y = N.
make_board(X,Y,N,I) :-
    X > N, Y < N,
    Y1 is Y+1,
    make_board(1,Y1,N,I).
make_board(X,Y,N,I) :-
    X =< N, Y =< N,
    cell(I,X,Y),
    nbs(allcells,[I]),
    X1 is X+1,
    I1 is I+1,
    make_board(X1,Y,N,I1).

% print a board
p(N) :- print_board(1,1,N).
print_board(X,Y,N) :-
    X > N, Y = N.
print_board(X,Y,N) :-
    X > N, Y < N,
    Y1 is Y+1,
    nl,
    print_board(1,Y1,N).
print_board(X,Y,N) :-
    X =< N, Y =< N,
    print_cell(X,Y),
    X1 is X+1,
    print_board(X1,Y,N).

:- chr_constraint print_cell(+,+).
nbs(A,_), cell(A,X,Y), cell(A,Val), print_cell(X,Y) <=> write(Val).
print_cell(_X,_Y) <=> write('?').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% upper bound (U) shown in the paper
% G. Chu, P.J. Stuckey, and M. Garcia de la Banda.
% Using relaxations in maximum density still life.
% In I. Gent, editor, Proceedings of the 15th International Conference on Principles and Practice of Constraint Programming, volume 5732 of LNCS, pp. 258-273. Springer-Verlag, 2009

mdsl(N) :- N1 is N+2, m(N1), borders(N1), U is floor(N*N/2+N-floor(N/3)), try_label(U), p(N1).

:- chr_constraint borders(+int).
borders(_), cell(A,1,_) ==> cell(A,0).
borders(N), cell(A,N,_) ==> cell(A,0).
borders(_), cell(A,_,1) ==> cell(A,0).
borders(N), cell(A,_,N) ==> cell(A,0).
borders(_) <=> true.

:- chr_constraint label(+int).  % find a solution with at least N live cells
label(N) \ nbs(allcells,List) <=> sumgeq(List,N). 
label(_), cell(A,_,_) ==> cell(A,1) ; cell(A,0).

try_label(N) :- N>=0, writeln(trying_label(N)), label(N).
try_label(N) :- N>0, N1 is N-1, try_label(N1).
