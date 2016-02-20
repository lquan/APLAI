#!/bin/bash 
# ECLiPSe-clp program running script
# example usage: ./run_sudoku.sh > output.csv
# alias clp='/usr/local/bin/eclipse-clp/bin/x86_64_linux/eclipse'

DATA="medium difficult verydifficult expert lambda hard17 symme eastermonster tarek_052 goldennugget coloin hardest extra1 extra2 extra3 extra4 diabolical platinum"
VARORDERS="input_order first_fail"

echo "### Sudoku classic ECLiPSe, $(date)"
echo "### $DATA"
echo "### VarOrder; Seconds; Backtracks"
for varorder in $VARORDERS; do
	echo "# varorder = $varorder"
	for data in $DATA; do
		 /usr/local/bin/eclipse-clp/bin/x86_64_linux/eclipse -b sudoku.ecl -e "$data(P), sudoku(P, $varorder)."	 	 
    done
    echo 
done

echo
echo "### Sudoku alternative ECLiPSe, $(date)"
echo "### dual viewpoint using default ic_sets setting"
echo "### $DATA"
echo "### Seconds"
for data in $DATA; do
	/usr/local/bin/eclipse-clp/bin/x86_64_linux/eclipse -b sudoku_alt.ecl -e "$data(P), sudoku(P)."	 	 
done

