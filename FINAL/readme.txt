APLAI assignment task 1 and 2 
README FILE
Karl Dox, Li Quan & Nathan Vandecauter
---------------------------------------

------
Sudoku
------
To test the programs it is useful to use the example puzzles in 'sudoku_puzzles.pl' (2 extra puzzles added),
It's located in ./sudoku/chr/ or ./sudoku/eclipse/.

ECLiPSe:
	
	- Normal viewpoint: ./sudoku/eclipse/sudoku.ecl  (global_ic) of ./sudoku/eclipse/sudoku_ic.ecl (ic)
		Command to run a single puzzle: "<puzzle>(P), sudoku(P)."
		
	- alternative viewpoint: ./sudoku/eclipse/sudoku_alt.pl
		Command to run a single puzzle: "<puzzle>(P), sudoku(P)."
	
	a single batch script for all puzzles for both viewpoints
	is provided for UNIX-like systems, type ./run_sudoku.sh
	
Constraint Handling Rules:
	You will need a version of prolog to run these files, we recommend SWI-Prolog.
	- Normal viewpoint: ./sudoku/chr/sudoku_chr_classic.pl
		Command to run a single puzzle: "<puzzel>(P), sudoku(P), solve.". 
			If you want a nicer display you can add 'printOut(P)'.
			e.g.: medium(P), sudoku(P), solve, printOut(P).
		Command to run all the puzzles in 'sudoku_puzzles.pl' and print out the runtime in millisecond: solveAll. .

	- Alternative viewpoint:
		Two programs were made for this viewpoint, unfortunately neither give a correct output.
		First: ./sudoku/chr/sudoku_chr_alt-1.5-unfinished.pl
			Command to run a single puzzle: "<puzzel>(P), sudoku(P,O), solve.". 
				If you want a nicer display you can add 'writeOutput(O)' for the sudoku in the alternative viewpoint.
				or 'printOutput(O)' for the normal viewpount.
				e.g.: medium(P), sudoku(P,O), solve, writeOut(O), nl, printOutout(O).
			Command to run all the puzzles in 'sudoku_puzzles.pl' and print out the runtime in millisecond: solveAll. .
				But as said before, the solved puzzles are not correct.
				
		Second: ./sudoku/chr/sudoku_chr_alt.pl
			Command to run a single puzzle: "<puzzel>(P), sudoku(P,O), solve.". If you want a nicer display you can add 'writeOutput(O)'.
				e.g.: medium(P), sudoku(P,O), solve, writeOutput(O).
			Command to run all the puzzles in 'sudoku_puzzles.pl' and print out the runtime in millisecond: solveAll. .
				But as said before, the solved puzzles are not correct.

		

--------------------------	
Maximum Density Still Life
--------------------------

ECLiPSe:
	- ./still life/eclipse/mdsl.ecl			%normal mdsl
	- ./still life/eclipse/mdsl2.ecl		%mdsl with symmetry breaking
	- ./still life/eclipse/mdsl_hybrid.ecl	%hybrid mdsl with symmetry breaking
		All three programs are run with the commando: msdl(N,Board).
		e.g.: mdsl(6,Output).	%will solve the mdsl for dimension 6x6 and return it in 'Output'.
		

Constraint Handling Rules:
	You will need a version of prolog to run this file, we recommend SWI-Prolog.
	
	The file is located at: ./still life/chr/mdsl_chr.pl
		Command to run the program: "mdsl(N).", where N is the row-dimension.
			e.g.: mdsl(5). % will solve the mdsl for dimension 5x5
			
	
	

-----	
Survo
-----

ECLiPSe:	
	The file is located at: ./survo/survo.ecl
		Command to run the program: "survo([Horizontal,Vertical], Board).", 
			where Horizontal and Vertical are lists containing the respective sums, and Board is the initial filled.
			e.g.: X = [21,10,18,29], Y = [24,15,39], P = [ [7,_,5,_],[_,1,_,8],[_,_,11,_] ] , survo([X,Y], P). 
		Command to run all the examples in the file: "test."
		
		
		
		
		