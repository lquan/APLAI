:- lib(ic).
:- lib(ic_global).
:- lib(eplex).
:- lib(branch_and_bound).
:- import sumlist/2 from ic_global.

% maximum density still life solver of size N
mdsl(N,Board) :-
	sl(N,Board),
	print_board(Board).

% still life solver
sl(N,Board) :- sl(N,[],Board).
sl(N,AliveList,Board) :-
	sl(N,AliveList,Board,input_order). % or input_order see http://www.eclipseclp.org/doc/bips/lib/ic/search-6.html

sl(N,AliveList,Board,VarOrder) :-
	statistics(times, [T0|_]),
	Np is N+2,
	dim(Board,[Np,Np]),
	init(Board,AliveList),
	model(Board),
	collection_to_list(Board, List),
	sumlist(List,NbAlive),
	
	U is floor(N*N/2+N-floor(N/3)),
	ic:(Cost :: -U..0),
	ic:(Cost #= (-1)*NbAlive),
	
	eplex:eplex_solver_setup(min(Cost),Cost,[sync_bounds(yes)],[bounds]), %%http://eclipseclp.org/doc/tutorial/tutorial124.html
	term_variables(Board,Vars),
	minimize((search(Vars, 0, VarOrder, indomain_max, complete, []),eplex_get(cost,Cost)), Cost),
	
	statistics(times, [T1|_]),
	Secs is T1-T0,
	printf("%.2f\n", [Secs]).
	
init(Board,AliveList) :-
	%now the alive list
	( foreach((X,Y), AliveList), param(Board) do
		ic:(Board[X+1,Y+1] #= 1)
	).	


model(Board) :-
	dim(Board, [N,N]),
	ic: (Board[1,1..N] :: 0),	ic:(Board[N,1..N] :: 0),
	ic: (Board[1..N,1] :: 0),	ic:(Board[1..N,N] :: 0),
	ic: (Board[2..N-1,2..N-1] :: 0..1),
	
	( multifor([X,Y],2,N-1), param(Board) do %loop over the inner cells
		[ic]:(Alive $= Board[X,Y]),
		%eplex::integers([Alive]),
		[ic]:(Nbs $= ( Board[X-1,Y-1] + Board[X,Y-1] +  Board[X+1,Y-1] + 
		 	 		 Board[X-1,Y]   +  		          Board[X+1,Y]   + 
			 		 Board[X-1,Y+1] + Board[X,Y+1] +  Board[X+1,Y+1])),
		
		%linear constraints, try both or independently
		eplex: (3*Alive + Nbs $=< 6),  %death by overcrowding
		%eplex: (2*Alive - Nbs $=< 0),  %death by isolation 
		
		% cp constraints
		% cell alive must be kept alive		 
		ic: (Alive => (Nbs #>= 2 and Nbs #=<3)),
			 
		% dead cell must stay dead
		ic: (neg(Alive) => Nbs #\= 3)
	),
	
	%loop over borders, cut the corners, surpress birth
	( for(I,2,N-1), param(Board,N) do
		%the left
	 	ic:( Nbs1 $= Board[I-1,2] + Board[I,2] + Board[I+1,2]),
 		ic:(Nbs1 $=< 2),
		%the top
	 	ic:(Nbs2 $= Board[2,I-1] + Board[2,I] + Board[2,I+1]),
 		ic:(Nbs2 $=< 2),
 		%the right
 		ic:(Nbs3 $= Board[I-1,N-1] + Board[I,N-1] + Board[I+1,N-1]),
 		ic:(Nbs3 $=< 2),
 		%the bottom
 		ic:(Nbs4 $= Board[N-1,I-1] + Board[N-1,I] + Board[N-1,I+1]),
 		ic:(Nbs4 $=< 2)
	),
	break_symmetry(Board,N).

% ad-hoc symmetry breaking by constraining density of upper half versus lower half
break_symmetry(Board,N) :- 
	HalveRowIdx is integer(floor(N/2))+1,
	( multifor([X,Y],[2,1],[N-1,HalveRowIdx]), foreach(UC, Upper), param(Board) do  %loop over upper half inner cells
		ic: (Board[X,Y] #= UC)
	),
	( multifor([X,Y],[2,HalveRowIdx+1],[N-1,N-1]), foreach(LC, Lower), param(Board) do  %loop over lower half inner cells
		ic: (Board[X,Y] #= LC) 
	),
	sumlist(Upper, UpperSum), sumlist(Lower, LowerSum), 
	ic: (UpperSum #>= LowerSum).

% alternative symmetry breaking
break_symmetry2(Board,N) :-
  % http://www.hakank.org/minizinc/maximum_density_still_life.mzn
  % SBSO: Symmetry-breaking by selective ordering
  % The assignment is forced to respect an ordering on the values that occur in corner entries
  % of the board. In particular:  
  % - if the NW-corner cell is dead, the SE-corner cell
  % must be dead too 
  % - if the NE-corner cell is dead, the SW-corner cell must be dead too
  % 
  ic: ( Board[2,2] #>= Board[N-1,N-1]),
  ic: ( Board[2,N-1] #>= Board[N-1,2]).
  
%taken from http://www.eclipseclp.org/examples/sudoku.ecl.txt
print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
	    	X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.
