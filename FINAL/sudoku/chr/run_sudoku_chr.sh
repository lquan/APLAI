#!/bin/bash 
# swipl-CHR program running script
# example usage: ./run_sudoku_chr.sh > output.csv

DATA="medium difficult verydifficult expert lambda hard17 symme eastermonster tarek_052 goldennugget coloin hardest extra1 extra2 extra3 extra4"

echo "### Sudoku classic CHR, $(date)"
echo "### $DATA"
echo "### Runtimes in ms"

# maybe turn off optimalisation flag -O
# flags:
# O --- optimisation
# q --- quiet mode
# s --- script file
# t --- top goal

swipl -O -q -s sudoku_chr_classic.pl -t "solveAll"


