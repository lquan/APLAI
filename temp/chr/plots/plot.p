set term post enh
set  autoscale                        # scale axes automatically
set output "plot.pdf"
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
set title "CHR"
set xlabel "puzzle"
set ylabel "run time (ms)"

set xtics ("medium" 1, "difficult" 2, "verydifficult" 3, "expert" 4, "lambda" 5, "hard17" 6, "symme" 7, "eastermonster" 8, "tarek_052" 9, "goldennugget" 10, "coloin" 11, "hardest" 11, "extra1" 12, "extra2" 13, "extra3" 14, "extra4" 15 )

#set xr [0.0:0.022]
#set yr [0:325]
plot    "results.txt"

