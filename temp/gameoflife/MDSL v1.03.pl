
:- lib(ic).
:- import alldifferent/1 from ic_global.
:- import list_to_array/2 from arrays.
%:- kernel(arithmetic).
%:- import sqrt/2 from arithmetic.
%:- import mod/3 from arithmetic.


mdsl(N,Out) :-

	%Het volgende tussen de '%%%' is fout en moet aangepast worden
	%Met dienen "search" ofzoiets..
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	generateAllPositions(N,AllPositions),
	!,
	MDSL_UpperBound = N * N,
	MDLS_Range :: 4..MDSL_UpperBound,
	%TODO
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	gameOfLife2(Board, N , AliveList ),
	countAlive(Board, Out),
	write("Maximum Density Still Life:  "), write(Out),nl
.

generateAllPositions(N,Out) :-
	generatePosition(N,1,1,Out)
.

generatePosition(N,N,N,[(N,N)]).
generatePosition(N,Row,Col,[(X2,Y2)|Out]) :-
	( 
		Col == N 
	 -> 
		Col1 is 1,
		Row1 is Row + 1
	 ; 
		Col1 is Col + 1,
		Row1 is Row
	),
	generatePosition(N,Row1,Col1,Out),
	X2 is Row,
	Y2 is Col
.

countAlive([],0).
countAlive([(_,_,A)|Board],Out) :-
	countAlive(Board,In),
	Out is In + A
.

/**
* gameOfLife( ?Board , +N, +AliveList)
* Board		The Board. format: [(x,y,alive)|... ]
* N			The Dimension. (NxN)
* AliveList	The list of the cells that have to be alive. format: [(x,y)|...]
* ---
* Generates board, construct the coordinates and fills in the cells, printout
*/
gameOfLife( Board , N ,AliveList) :- 
	N1 is N + 2,
	N2 is N1 * N1,
	generateBoard(Board, N2),		%problemen met dim
	convertAliveList(AliveList,AL),
	!,
	construct_Coordinates(Board , N2, AL),	%foreach deed ook kut!
	print_Board(Board,N1),
	nl,
	still_live_constraints(Board),
	nl
.	

gameOfLife2( Board , N ,AliveList) :- 
	N1 is N * N,
	generateBoard(Board, N1),		%problemen met dim
	!,
	construct_Coordinates(Board , N1, AliveList),	%foreach deed ook kut!
	print_Board(Board,N),
%	nl,
	still_live_constraints(Board)
%	,nl
.	
/**
* generateBoard(?Board,+N)
* Board		The Board
* N			Accumulator to get to dimensions right
* ---
* Generates a board with variables according to dimension N
*/
generateBoard([],0).
generateBoard([(_,_,_)|Board],N) :-
	N1 is N - 1,
	generateBoard(Board,N1)
.

/**
* convertAliveList(+AliveList,-AL) :-
* AliveList 	The list of the cells that have to be alive format: [(x,y)|...]
* AL			The converted alivelist
* ---
* Needs to be called because we at a border of dead cells, so the coordinates should be highered
*/
convertAliveList([],[]).
convertAliveList([(X,Y)|AliveList],[(X1,Y1)|AL]) :-
	X1 is X + 1,
	Y1 is Y + 1,
	convertAliveList(AliveList,AL)
.

/**
* construct_Coordinates(?Board,+N,+AliveList)
* @see construct_Coordinates(Board ,Row, Col, Divider, N, Acc, AliveList)
*/
construct_Coordinates(Board,N,AliveList) :-
	construct_Coordinates(Board,_,_,_,N,1,AliveList).

/**
* construct_Coordinates(?Board ,?Row, ?Col, ?Divider, +N, +Acc, +AliveList) :-
* Board			The Board
* Row			The row of the next cell (recursion first)
* Col			The colum of the next cell (recursion first)
* Divider		The dimension of the row, used to know when to switch rows
* N				The dimension of the board
* Acc 			The accumulator used to end the recursion
* AliveList		The list of cells that are alive
* ---
* Gives every variable in the Board a coordinate ( iteration: cols over rows )
* sets the cell to alive when occuring in the AliveList, dead otherwise
*/	
construct_Coordinates([(Row,Row,State)],Row,Row,Row,N,N,AliveList) :- 	% Row en col zouden ook in board kunnen meegegeven worden 
	RowD is sqrt(N),										% maar dan problemen met doorgeven en 'append'en?
	integer(RowD,Row),
	(
		member((Row,Row),AliveList) -> State = 1 ; State = 0
	)
.
construct_Coordinates([(Row,Col,State)|Board] ,Row, Col, Divider, N, Acc, AliveList) :-
	Acc1 is Acc + 1,															
	construct_Coordinates(Board,RowI, ColI, Divider, N, Acc1,AliveList),	% Hier dus probleem met Board doorgeven
	mod(Acc,Divider,RowCheck),
	( RowCheck == 0 
	 ->
		Row is RowI - 1,
		Col is Divider
	 ;
		Row is RowI,
		Col is ColI - 1
	),
	(
		member((Row,Col),AliveList) -> State = 1 ; State = 0
	)
.

/*
%%%%%%%%%%%%%%%%%%%%%
% Zonder diagonalen %
%%%%%%%%%%%%%%%%%%%%%
still_live_constraints(Board) :-
	(
		foreach( (X,Y,D) , Board ), param(Board)
		do
			%write("Neighbours of "), write(X), write(' '),write(Y), write(": "),
			(
				D == 1  	%alive
			  ->
			    still_live_constraints_alive(X,Y,Board)
			  ;
			    still_live_constraints_death(X,Y,Board)
			)
	)
.	


still_live_constraints_alive(X,Y,Board) :-
	count_neighbours_alive(X,Y,Board,Neighbours),
	write(Neighbours),
	( Neighbours == 2 ; Neighbours == 3)
.

still_live_constraints_death(X,Y,Board) :-
	count_neighbours_alive(X,Y,Board,Neighbours),
	write(Neighbours),
	Neighbours ~= 3
.

 %Geen diagonalen
count_neighbours_alive(_,_,[],0).
count_neighbours_alive(X,Y,[(R,C,D)|Board],Neighbours):-   % Geen diagonalen
	count_neighbours_alive(X,Y,Board,Acc),
	(
		D == 1
	 ->
		( 
			R == X
		 ->
			(
				K is Y-1, L is Y+1,
				(C == K ; C == L)
			 ->
				Neighbours is Acc + 1
			 ;
				Neighbours is Acc
			)
		 ;
			(
				C == Y
			  ->
				(
					K is X-1, L is X+1,
					(R == K ; R == L)
				  ->
					Neighbours is Acc+1
				  ;
					Neighbours is Acc
				)
			  ;
				Neighbours is Acc
			)
		)
	 ;
		Neighbours is Acc
	)
.
*/

%%%%%%%%%%%%%%%%%%
% Met Diagonalen %
%%%%%%%%%%%%%%%%%%
still_live_constraints(Board) :-
	(
		foreach( (X,Y,D) , Board ), param(Board)
		do
			(
				D == 1  	%alive
			  ->
			    still_live_constraints_alive((X,Y,D),Board)
			  ;
			    still_live_constraints_death((X,Y,D),Board)
			)
	)
.	
still_live_constraints_alive(Cell,Board) :-
	count_neighbours_alive(Cell,Board,Neighbours),
%	write(Cell),write(" :  "), write(Neighbours), nl,
	( Neighbours == 2 ; Neighbours == 3)
.

still_live_constraints_death(Cell,Board) :-
	count_neighbours_alive(Cell,Board,Neighbours),
%	write(Cell),write(" :  "), write(Neighbours), nl,
	Neighbours ~= 3
.

% MET diagonalen
count_neighbours_alive((_,_,0),[],0).
count_neighbours_alive((_,_,?),[],0).
count_neighbours_alive((_,_,1),[],-1).

count_neighbours_alive((X,Y,A),[(R,C,D)|Board],Neighbours):-   %met diagonalen
	count_neighbours_alive((X,Y,A),Board,Acc),
	( 
		D == 1
	  ->
		X1 is X - 1,
		X2 is X + 1,
		Y1 is Y - 1,
		Y2 is Y + 1,
		(
			( R >= X1 , R =< X2 , C >= Y1 , C =< Y2 )
		->
			Neighbours is Acc + 1
		;
			Neighbours is Acc
		)
	  ;
		Neighbours is Acc
	)
.




/**
* print_Board(?Board,+Jump)
* Board		The Board
* Jump		The dimension of the rows, used to find newline
* ---
* Prints the Board, is a cell is a variable it will display '?'
*/
print_Board([],_) .
print_Board([(_,Y,D) | Board],Jump) :-
	write(' '),
	( 
		var(D) -> write('?') ; write(D)
	),
	write(' '),
	(
		Y == Jump -> nl ; true
	),
	print_Board(Board,Jump)
.

