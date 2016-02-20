:- [examples].
:- lib(ic).
:- lib(lists).

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
  sudokuToList(Sudoku,Result),
  middle_out(Result,MOList),
  ( fromto(MOList, Vars, Rest, [])
  do
     delete(Var,Vars,Rest,0,first_fail),
     indomain(Var,middle)
  ).

sudokuToList(Sudoku,Result) :-
   (multifor([X,Y],[1,1],[9,9]), param(Sudoku,Result)
   do
      selectElement(Sudoku,X,Row),
      selectElement(Row,Y,Var),
      Index is Y+(X-1)*9,
      selectElement(Result,Index,Var)
   ).


middle_out(List, MOutList) :-
  halve(List, FirstHalf, LastHalf),
  reverse(FirstHalf, RevFirstHalf),
  splice(LastHalf, RevFirstHalf, MOutList).

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
