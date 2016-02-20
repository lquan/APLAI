:- lib(eplex).

gameOfLife(In,N,Out,CellsAlive) :-
	
	/* Initialize Eplex Solver */
     eplex_solver_setup(max(CellsAlive),CellsAlive,[sync_bounds(yes)],[bounds]),
    
	/* Initialize Dimension*/
	Np is N+2,
	dim(Out,[Np,Np]),
	%Out[1..Np,1..Np] :: 0..1,
	convertAliveList(In,InPlus),
	
	/* Initialize Known Cells */
	initBorder(Out),
	initAlive(InPlus,Out),
printOut(Out),
    !,
	
	
	/* Constraints */
	constraints(Out),
	( foreachelem(El,Out) do ( El is 1 ; El is 0) ),
	%labeling(Out),
	countCellsAlive(Out,CellsAlive),
	%( multifor([I,J], 2, (Np-1)) do T is Out[I,J], sum(flatten(Out[(I-1)..(I+J),(J-1)..(J+1)]),Sum), ( T == 0 -> Sum ~=3 ; (Sum == 3 ; Sum == 4))),
	printOut(Out)
	
.

initBorder(Out) :-
	dim(Out,[M,N]),
	T is Out[1..M,1],	( foreach(El,T) do El is 0 ),
	T is Out[1..M,N],	( foreach(El,T) do El is 0 ),
	D is Out[1,1..N],	( foreach(El,D) do El is 0 ),
	D is Out[M,1..N],	( foreach(El,D) do El is 0 )
.

convertAliveList(In,InPlus) :-
	dim(In,[N]),
	dim(InPlus,[N]),
	( foreachelem((R,C),In) , foreachelem((R1,C1),InPlus) do R1 is R+1, C1 is C+1 )
.

initAlive(In,Out) :-
	( foreachelem((R,C),In), param(Out) do
		1 is Out[R,C]
	)
.


countCellsAlive(Out, Cost) :-
	flatten_array(Out,FOut),
	sum(FOut,Cost)
.

constraints(Out) :-
	dim(Out,[L,L]),
	( multifor([I,J],1,L) , param(Out) do 
		( isAlive((I,J),Out)
			->
			%write("alive "),nl,
			constraint_stayAlive((I,J),Out)
			;
			%write("dead "),nl,
			0 is Out[I,J],
			%( ground(Out[I,J]) -> true; 0 is Out[I,J] ),
			constraint_stayDead((I,J),Out)
		)
	)
.

isAlive((I,J),Out) :- 
	1 is Out[I,J]
.

constraint_stayDead((I,J),Out) :-
nl,printOut(Out),nl,
	dim(Out,[M,N]),
	LB :: 0..1,	B :: 0..1,	RB :: 0..1,
	L :: 0..1,				R :: 0..1,
	LO :: 0..1,	O :: 0..1,	RO :: 0..1,
	
	( I > 1 , J > 1 ->  LB is Out[(I-1),(J-1)] ; LB is 0),
	( I > 1  		->   B is Out[(I-1),J] ; B is 0),
	( I > 1 , J < N ->  RB is Out[(I-1),(J+1)] ; RB is 0),
	( 		  J > 1 ->  L  is Out[I,(J-1)] ; L is 0),
	%C is Out[I,J],
	( 		  J < N ->  R  is Out[(I),(J+1)] ; R is 0),
	( I < M , J > 1 ->  LO is Out[(I+1),(J-1)] ; LO is 0),
	( I < M 		->   O is Out[(I+1),(J)] ; O is 0),
	( I < M , J < N ->  RO is Out[(I+1),(J+1)] ; RO is 0),
	
	%( eplex: (LB + B + RB + L + R + LO + O + RO $=< 2 );
	%eplex: (LB + B + RB + L + R + LO + O + RO $>= 4 ) )
	LB + B + RB + L + R + LO + O + RO ~= 3
	
.

constraint_stayAlive((I,J),Out) :-
nl,printOut(Out),nl,
	dim(Out,[M,N]),
	LB :: 0..1,	B :: 0..1,	RB :: 0..1,
	L :: 0..1,				R :: 0..1,
	LO :: 0..1,	O :: 0..1,	RO :: 0..1,
	
	( I > 1 , J > 1 ->  LB is Out[(I-1),(J-1)] ; LB is 0),
	( I > 1  		->   B is Out[(I-1),J] ; B is 0),
	( I > 1 , J < N ->  RB is Out[(I-1),(J+1)] ; RB is 0),
	( 		  J > 1 ->  L  is Out[I,(J-1)] ; L is 0),
	%C is Out[I,J],
	( 		  J < N ->  R  is Out[(I),(J+1)] ; R is 0), 
	( I < M , J > 1 ->  LO is Out[(I+1),(J-1)] ; LO is 0),
	( I < M 		->   O is Out[(I+1),(J)] ; O is 0),
	( I < M , J < N ->  RO is Out[(I+1),(J+1)] ; RO is 0),
	
%	eplex: (LB + B + RB + L  $=< 3),
%	eplex: (LB + B + RB + L + R  $=< 3),
%	eplex: (LB + B + RB + L + R + LO  $=< 3),
%	eplex: (LB + B + RB + L + R + LO + O $=< 3),
	
	eplex: (LB + B + RB + L + R + LO + O + RO $>= 2),
	eplex: (LB + B + RB + L + R + LO + O + RO $=< 3)
.

printOut(Out) :-
	nl,
	dim(Out, [M,N]),
	( for(I,1,M), param(Out,N) do		% Geen multifor voor "nl" te kunen doen
	    ( for(J,1,N), param(Out,I) do
	    	X is Out[I,J],
		( var(X) -> write("  ?") ; printf(" %2d", [X]) )
	    ), nl
	),
	nl
.

%%%%%
% Examples
%%%%%

ex1(_) :- write("Example with dimension = 1... None Habes").
ex2(Out) :- gameOfLife([]((1,1)),2,Out).
ex3a(Out) :- gameOfLife([]((1,1)),3,Out).
ex3b(Out) :- gameOfLife([]((2,2)),3,Out).
ex4a(Out) :- gameOfLife([]((1,1)),4,Out).
ex4b(Out) :- gameOfLife([]((2,2)),4,Out).
ex4c(Out) :- gameOfLife([]((1,1),(4,1),(1,4),(4,4)),4,Out).
ex5a(Out) :- gameOfLife([]((1,1),(4,1),(1,4),(4,4)),5,Out).
ex5b(Out) :- gameOfLife([]((1,1),(4,1),(1,4),(4,4),(5,5)),5,Out).
ex5c(Out) :- gameOfLife([]((3,3)),5,Out).
