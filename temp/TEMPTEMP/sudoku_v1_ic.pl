:- [examples].
:- lib(ic).

solve(Sudoku) :- 
  initialise(Sudoku),
  constraints(Sudoku),
  search(Sudoku),
  printer(Sudoku,'Solution').

initialise(Sudoku) :-
  (foreach(Row, Sudoku)
  do 
    Row::1..9
  ).

search(Sudoku) :-
  (foreach(Row, Sudoku)
  do 
    (foreach(Var, Row)
    do 
      indomain(Var)
    )
  ).

constraints(Sudoku) :-
  rowConstraints(Sudoku),
  collumnConstraints(Sudoku),
  blockConstraints(Sudoku).
    
rowConstraints(Sudoku) :-
  (foreach(Row, Sudoku)
  do 
    alldifferent(Row)
  ).

collumnConstraints(Sudoku) :-
  (for(I,1,9),param(Sudoku)
  do
    (foreach(Row, Sudoku),param(I),
     foreach(Var,Collumn)
      do
        selectElement(Row,I,Var)
    ),
    alldifferent(Collumn)
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
    alldifferent(Block)
  ).

selectElement([El|_],1,El).
selectElement([_|Rest],X,El) :- 
  X1 is X - 1,
  selectElement(Rest,X1,El).

printer(Sudoku,Name):-
   writeln(Name),
   (foreach(Row,Sudoku)
   do 
        (foreach(X,Row)
        do
                write(X),
                write(', ')
        ),
        writeln('.')
   ).
