#!/bin/bash 
# CHR program running script
# example usage: ./run_sudoku_chr.sh > output.csv

DATA="medium difficult verydifficult expert lambda hard17 symme eastermonster tarek_052 goldennugget coloin hardest extra1 extra2 extra3 extra4"

echo "### Sudoku classic CHR, $(date)"
echo "### $DATA"
echo "### Runtimes in ms"

# maybe turn of optimalisation flag -O
#flags:
# O --- optimisation
# q --- quiet mode
# t --- top goal
# s --- script file
swipl -O -q -s sudoku2.pl -t "solveAll"


