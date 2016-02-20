:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- import sumlist/2 from ic_global.


/**
 * mdsl( +N , ?Board )
 * -------------------
 * N:		The dimension of the board is NxN.
 * Board:	The board to fill in the mdsl. Form: []([](...),[](...),..,[](...))
 * -------------------
 * A maximum density still life solver of size N.
 */
mdsl(N,Board) :-
	sl(N,Board),
	print_board(Board)
.

/**
 * sl( +N , ?Board )
 * -----------------
 * N:		The dimension of the board is NxN.
 * Board:	The board where the still life is solved on. Form: []([](...),[](...),..,[](...))
 * -----------------
 * The Still life solver. Returns the stilllife of which cells alive are maximized.
 *
 * 'input_order' is used as search option, another possibility is first_fail.
 * See http://www.eclipseclp.org/doc/bips/lib/ic/search-6.html for more information.
 */
sl(N,Board) :- sl(N,[],Board).
sl(N,AliveList,Board) :-
	sl(N,AliveList,Board,input_order)
.

/**
 * sl( +N , ?AliveList, ?Board , +Option )
 * ---------------------------------------
 * N:			The dimension of the board is NxN.
 * AliveList:	The list containing all the living cells.	Form: [(X,Y),(X1,XY),...,(Xn,Yn)]
 * Board:		The Board where the simple life solver runs on. 	Form: []([](...),[](...),..,[](...))
 * Option:		The option used in the search method.
 *				See http://www.eclipseclp.org/doc/bips/lib/ic/search-6.html for more information.
 * ---------------------------------------
 * The still life solver. Returns the stilllife of which cells alive are maximized.
 */
sl(N,AliveList,Board,VarOrder) :-
	statistics(times, [T0|_]),
	
	Np is N+2, 
	dim(Board,[Np,Np]),
	init(Board,AliveList),
	model(Board),
	
	U is floor(N*N/2+N-floor(N/3)), %to bound the search to the upper limit
	Cost :: (-1)*U .. 0,
	collection_to_list(Board, List),
	sumlist(List,NbAlive),
	Cost #= (-1)*NbAlive,
	
	term_variables(Board,Vars),
	minimize(search(Vars, 0, VarOrder, indomain_max, complete, [backtrack(BT)]), Cost), %%must use indomain_max
	
	statistics(times, [T1|_]),
    Secs is T1-T0,
	setval(backtracks, BT),
	Density is (-1)*Cost / (N*N),
	printf("%.2f; %d; %.4f\n", [Secs, BT, Density])
.
	
/**
 * init( +Board , +AliveList )
 * ---------------------------
 * Board:		The board where the simple life solver is running on.	Form: []([](...),[](...),..,[](...))
 * AliveList:	The list containing the cells that are alive.	Form: [(X,Y),(X1,XY),...,(Xn,Yn)]
 * ---------------------------
 * Initializes the board and adds 
 */	
init(Board,AliveList) :-
	% the alive list
	( foreach((X,Y), AliveList), param(Board) do
		Board[X+1,Y+1] #= 1  %offset because of border
	)
.	
		
/**
 * model( +Board )
 * ---------------
 * Board:	The board to where the still life is ran on: Form: []([](...),[](...),..,[](...)).
 * ---------------
 * The method models the Board with the constraints necessary to compute a still live.
 */
model(Board) :-
	dim(Board, [N,N]),

	Board[1,1..N] #:: 0,	Board[N,1..N] #:: 0,
	Board[1..N,1] #:: 0,	Board[1..N,N] #:: 0,
	Board[2..N-1,2..N-1] #:: 0..1,
	
	( multifor([X,Y],2,N-1), param(Board) do  %loop over inner cells
		% current cell and its neighbours	
		Nbs #= ( Board[X-1,Y-1]  + Board[X,Y-1] + Board[X+1,Y-1] + 
	 	 	     Board[X-1,Y]    +  		  	  Board[X+1,Y]   + 
			 	 Board[X-1,Y+1]  + Board[X,Y+1] + Board[X+1,Y+1] ),

		% live cell must be kept alive		 
		Board[X,Y] => ( Nbs #>= 2 and Nbs #=<3 ),	 
		% dead cell must stay dead
		neg(Board[X,Y]) => Nbs #\= 3
	),
	
	%loop over borders, cut the corners, suppress birth
	( for(I,2,N-1), param(Board,N) do
		%the left
	 	Nbs1 #= Board[I-1,2] + Board[I,2] + Board[I+1,2],
 		Nbs1 #=< 2,
		%the top
	 	Nbs2 #= Board[2,I-1] + Board[2,I] + Board[2,I+1],
 		Nbs2 #=< 2,
 		%the right
 		Nbs3 #= Board[I-1,N-1] + Board[I,N-1] + Board[I+1,N-1],
 		Nbs3 #=< 2,
 		%the bottom
 		Nbs4 #= Board[N-1,I-1] + Board[N-1,I] + Board[N-1,I+1],
 		Nbs4 #=< 2
	),
	break_symmetry2(Board,N)
.



/**
 * break_symmetry( ?Board , +N )
 * ------------------------------
 * Board:	The board where the simple life solver is running on.	Form: []([](...),[](...),..,[](...))
 * N:		The dimension of the board is NxN.
 * ------------------------------
 * The method is a symmetry breaking.
 * ad-hoc symmetry breaking by constraining density of left side versus right side.
 * The amount of cells alive on the left side should be greater or equal than the right side.
 */
break_symmetry(Board,N) :- 
	HalveRowIdx is integer(floor(N/2))+1,
	
	( multifor([X,Y],[2,1],[N-1,HalveRowIdx]), foreach(LC, Left), param(Board) do  %loop over left half inner cells
		Board[X,Y] #= LC
	),
	( multifor([X,Y],[2,HalveRowIdx+1],[N-1,N-1]), foreach(RC, Right), param(Board) do  %loop over right half inner cells
		Board[X,Y] #= RC 
	),
	sumlist(Left,LeftSum), sumlist(Right,RightSum),
	LeftSum #>= RightSum
.
	

/**
 * break_symmetry2( ?Board , +N )
 * ------------------------------
 * Board:	The board where the simple life solver is running on.	Form: []([](...),[](...),..,[](...))
 * N:		The dimension of the board is NxN.
 * ------------------------------
 * The method is an alternative symmetry breaking.
 *
 *  http://www.hakank.org/minizinc/maximum_density_still_life.mzn
 * SBSO: Symmetry-breaking by selective ordering
 * The assignment is forced to respect an ordering on the values that occur in corner entries
 * of the board. In particular:  
 * - if the NW-corner cell is dead, the SE-corner cell
 * must be dead too 
 * - if the NE-corner cell is dead, the SW-corner cell must be dead too
 */ 
break_symmetry2(Board,N) :-	 
	Board[2,2] #>= Board[N-1,N-1],
	Board[2,N-1] #>= Board[N-1,2]
.

		
/**
 * print_Board( +Board )
 * ---------------------
 * Board:	The board containting the simple life.
 * ---------------------
 * The method displays the inbord in a more human friendly way.
 */
print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
	    	X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl
.
	
