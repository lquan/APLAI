:-[examples].

solve(Sudoku) :-
  search(Sudoku),
  constraints(Sudoku).

search(Sudoku) :-
  (foreach(Row, Sudoku)
  do
     permutation([1,2,3,4,5,6,7,8,9], Row)
  ).

constraints(Sudoku) :-
  collumnConstraints(Sudoku),
  blockConstraints(Sudoku).

collumnConstraints(Sudoku) :-
  (for(I,1,9),param(Sudoku)
  do
    (foreach(Row, Sudoku),param(I),
     foreach(Var,Collumn)
      do
        selectElement(Row,I,Var)
    ),
    allDifferent(Collumn)
  ).
  
blockConstraints(Sudoku) :-
  (multifor([I,J],[0,0],[2,2]),param(Sudoku)
  do
    (multifor([M,N],[1,1],[3,3]),param(Sudoku,I,J),
     foreach(Var,Block)
      do
        X is 3*I + M,
        Y is 3*J + N,
        selectElement(Sudoku,X,Row),
        selectElement(Row,Y,Var)
    ),
    allDifferent(Block)
  ).

permutation([],[]) :-
  !.
permutation(Original, [X|Rest]) :-
  member(X,Original),
  del(X,Original,Result),
  permutation(Result, Rest).

del(El, [El|Rest], Rest) :-
  !.
del(El, [X|Rest], Result) :-
  del(El, Rest, DelResult),
  Result = [X|DelResult].

selectElement([El|_],1,El).
selectElement([_|Rest],X,El) :- 
  X1 is X - 1,
  selectElement(Rest,X1,El).

allDifferent(List) :-
  (fromto(List,[El|Rest],Rest,[])
  do
    (fromto(Rest,[X|T],T,[]),
    param(El)
    do
        El \= X
    )
  ).
