:-[examples].
:- use_module(library(chr)).
:- chr_constraint solve/1.
:- chr_constraint assigned/3.
:- chr_constraint values/3.
:- chr_constraint readSudoku/1.
:- chr_constraint readRow/2.
:- chr_constraint readEl/3.
:- chr_constraint sudoku/1.
:- chr_constraint search/0.

solve(P) <=> readSudoku(P), search.

readSudoku([Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9]) <=> veryeasy(S), sudoku(S), addValues(1,1), readRow(1,Row1), readRow(2,Row2), readRow(3,Row3), readRow(4,Row4), readRow(5,Row5), readRow(6,Row6), readRow(7,Row7), readRow(8,Row8), readRow(9,Row9).

readRow(X,[El1,El2,El3,El4,El5,El6,El7,El8,El9]) <=> readEl(X,1,El1), readEl(X,2,El2), readEl(X,3,El3), readEl(X,4,El4), readEl(X,5,El5), readEl(X,6,El6), readEl(X,7,El7), readEl(X,8,El8), readEl(X,9,El9).

readEl(X,Y,V), values(X,Y,_) <=> nonvar(V)|assigned(X,Y,V).
readEl(_,_,_) <=> true.

values(_,_,[]) <=> fail.

%row constraint
assigned(X,_,V) \ values(X,Y,List) <=> member(V,List) | del(V,List,Result),values(X,Y,Result). 

%collumn constraint
assigned(_,Y,V) \ values(X,Y,List) <=> member(V,List) | del(V,List,Result),values(X,Y,Result).

%block constraint
assigned(X1,Y1,V) \ values(X2,Y2,List) <=> 
  Xs is (X1-1)//3, Xs is (X2-1)//3,
  Ys is (Y1-1)//3, Ys is (Y2-1)//3,
  member(V,List)
    |del(V,List,Result),values(X2,Y2,Result).

sudoku(S) \ assigned(X,Y,V) <=> selectElement(S,X,Row), selectElement(Row,Y,V).

values(X,Y,[V|[]]) <=> assigned(X,Y,V).

%search
search, values(X,Y,List) <=> search(X,Y,List), search.
search <=> true.

search(X,Y,List) :-
  member(V,List),
  (
    assigned(X,Y,V)
  ;
    del(V,List,Result),
    values(X,Y,Result)
  ).

del(El, [El|Rest], Rest) :-
  !.
del(El, [X|Rest], Result) :-
  del(El, Rest, DelResult),
  Result = [X|DelResult].

selectElement([El|_],1,El).
selectElement([_|Rest],X,El) :- 
  X1 is X - 1,
  selectElement(Rest,X1,El).

addValues(_,10) :- !.
addValues(X,Y) :-
  values(X,Y,[1,2,3,4,5,6,7,8,9]),
  XNew is (X mod 9) + 1,
  YNew is Y + (X // 9),
  addValues(XNew,YNew).
