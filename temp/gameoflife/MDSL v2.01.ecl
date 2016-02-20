%%%%
% Game of life
%%%%

:- lib(ic).


mdsl(N,Out) :-
	write("Fuck dees fieu!")
.





/**
 * gameOfLife(+In,+N,-Out)
 * -----------------------
 * In: 	List of Cells that has to be alive. Form: []((x1,y1),(x2,y3),...,(xN,yN)).
 * N:	The dimension of the game of life. Dimension: NxN.
 * Out:	The output list of all the cells (dead or alive), which form a still life.
 * -----------------------
 * TODO: Description.
 */
gameOfLife(In, N, Out) :-
	/* Verwerking Input */
	dim(Out,[N,N]),
	Out[1..N,1..N] :: 0..1,
	initAlive(In,Out),
	%write(Out),nl,
	!,
	/* Oplossing zoeken */
	% TODO
	/* Labelen */
	labeling(Out),
	/* Constraints */
	constraints_StillLife(Out),
	/*print*/
	printOut(Out)
.




/**
* initAlive(+In, ?Out)
* --------------------
* In:	The List of coordinates for which cells have to be alive.
* Out:	List where the 'In'-coordinates are one and the other a variable.
* --------------------
* Sets the cells of <Out> to 1, if the coordinate of the cell occures in <In>.
*/
initAlive(In,Out) :-
	( foreachelem((R,C),In), param(Out) do
		1 is Out[R,C]
	)
.

/**
* constraints_StillLife(+Out)
* ---------------------------
* Out: List that has to be checked if it is a still life
* ---------------------------
* The method returns true if the given <Out> is a still life, false otherwise
*/
constraints_StillLife(Out) :-
	dim(Out,[L,L]),
	( multifor([I,J],1,L) , param(Out) do 
		( isAlive((I,J),Out)
			->
			%write("alive "),nl,
			constraint_stayAlive((I,J),Out)
			;
			%write("dead "),nl,
			constraint_stayDead((I,J),Out)
		)
	)
.

/**
* isAlive(+(I,J),+Out)
* --------------------
* (I,J):	Coordinate to check is it is alive.
* Out:		The list where to check in.
* --------------------
* The method returns true is the coordinate <(I,J)> is alive (1) in <Out>
*/
isAlive((I,J),Out) :- 
	1 is Out[I,J]
.

/**
* constraint_stayDead(Cell,Out)
* -----------------------------
* Cell: the cell to check if it will stay dead in the next generation.
* Out:	The board with all the cells.
* ----------------------------------
* The method returns true is the cell will stay dead the next generation, false otherwise.
* Precondition: The <Cell> is dead (0).
*/
constraint_stayDead(Cell,Out) :-
	countNeighbours(Cell,Out,Neighbours),
	Neighbours ~= 3
.

/**
* constraint_stayAlive(Cell,Out)
* ------------------------------
* Cell: the cell to check if it will stay alive in the next generation.
* Out:	The board with all the cells.
* ----------------------------------
* The method returns true is the cell will stay alive the next generation, false otherwise.
* Precondition: The <Cell> is alive (1).
*/
constraint_stayAlive(Cell,Out) :-
	countNeighbours(Cell,Out,Neighbours),
	( Neighbours == 2 ; Neighbours == 3 )
.

/**
* countNeighbours(+(I,J),+Out,-Neighbours)
* ----------------------------------------
* (I,J):	The cell fromwhich the (living) neighbours need to be count
* Out:		The board containing all the cells.
* Neighbours:	The amount of living neighbours (Integer)
* ----------------------------------------
* The method returns the amount of living cells surrounding the given cell (I,J).
* Being neighbour is means that the cell touches a cell in it's direct enviroment.
* This can be horizontal, vertical and diagonal
*/
countNeighbours((I,J),Out,Neighbours) :-
	dim(Out,[M,N]),
	( I > 1 -> XX is I-1 ; XX is 1 ),	% [XX . YX]
	( I < M -> XY is I+1 ; XY is M ),	% [ . . . ]
	( J > 1 -> YX is J-1 ; YX is 1),	% [XY . YY]
	( J < N -> YY is J+1 ; YY is N),
	SubMatrix is Out[XX..XY, YX..YY],
	
	( foreach(X,SubMatrix) , fromto(0,Head,Tail,Sum) do
		( foreach(Y,X), fromto(0,First,Last,SubSum) do 
			Last is First+Y 
		), 
		Tail is Head + SubSum
	),
	T is Out[I,J],
	Neighbours is Sum - T		% Zichzelf meegeteld
.

/**
* printOut(+Out)
* --------------
* Out:	List to print out
* --------------
* Displays in a easily human readable way the given list (Out)
*/
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ************************************************ %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Voorbeeld ForLoops die werken!  ... verschil tussen [](i,j) en [i,j]
% ( foreachelem(X,[](1,2)) do write(X),nl).
% ( foreach( X , [1,2])  do write(X),nl).
%
% In = [](1,2), dim(In,L), ( for(I,1,L),param(In) do X is In[I],write(X),nl ).  %<< is zowat het 1e int lang :)
%%%%%

%%%%% misschien bruikbaar!
%( for(I,1,N), param(Out,N) do
%   Row is Out[I,1..N],
%   %RowConstraints
%	%write(Row),nl
%   Col is Out[1..N,I]
%	%ColConstraints
%	%write(Col),nl
%),
%%%%%

%%%%%
% miss bruikbaar
%dim(Array,[2,2]),
%( foreach(El,[e11,e12,e21,e22]), foreachelem(El, Array) do
%	true
%),
%->>Array = []([](..),[](..))
%%%%%

%%%%%
% miss bruikbaar
%generatePositions(N,Coor) :-
%	(multifor([I,J],1,N), fromto(Coor,Head,Tail,[]) do Head=[(I,J)|Tail])
%.
%%%%%