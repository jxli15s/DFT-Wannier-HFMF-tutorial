
#set terminal X11 enhanced font 'Helvetica,20' persist

set terminal qt enhanced font 'Helvetica,20' persist

###set terminal postscript eps enhanced color 'Times-Roman,20'
#set terminal postscript eps enhanced color 'Helvetica,20'
#set output 'band.eps'
#print 'The output is band.eps'

#set terminal epslatex standalone color
#set output 'band.tex'
#print 'The output is band.tex'

set style line 1   lc rgb '#ffffff' lt 1 lw 1 # white
set style line 2   lc rgb '#000000' lt 1 lw 1 # black
set style line 47  lc rgb '#666666' lt 1 lw 1 # grey40
set style line 3   lc rgb '#dd181f' lt 1 lw 1 # red
set style line 4   lc rgb '#0060ad' lt 1 lw 1 # blue
set style line 5   lc rgb '#006400' lt 1 lw 1 # dark-green
set style line 6   lc rgb '#b8860b' lt 1 lw 1 # dark-goldenrod
set style line 7   lc rgb '#c000ff' lt 1 lw 1 # dark-magenta
set style line 8   lc rgb '#00eeee' lt 1 lw 1 # dark-cyan
set style line 9   lc rgb '#c04000' lt 1 lw 1 # dark-orange
set style line 10  lc rgb '#c8c800' lt 1 lw 1 # dark-yellow
set style line 11  lc rgb '#4169e1' lt 1 lw 1 # royalblue
set style line 12  lc rgb '#ffc020' lt 1 lw 1 # goldenrod
set style line 13  lc rgb '#008040' lt 1 lw 1 # dark-spring-green
set style line 14  lc rgb '#c080ff' lt 1 lw 1 # purple
set style line 15  lc rgb '#306080' lt 1 lw 1 # steelblue
set style line 16  lc rgb '#8b0000' lt 1 lw 1 # dark-red
set style line 17  lc rgb '#408000' lt 1 lw 1 # dark-chartreuse
set style line 18  lc rgb '#ff80ff' lt 1 lw 1 # orchid
set style line 19  lc rgb '#7fffd4' lt 1 lw 1 # aquamarine

$vl << EOF
    1.474634   66.000000
    1.474634    0.000000

    1.474634    0.000000
    1.474634  -24.000000

    2.326014   66.000000
    2.326014    0.000000

    2.326014    0.000000
    2.326014  -24.000000
EOF

$fl << EOF
    0.000000    0.000000
    4.028774    0.000000
EOF

set xrange [    0.000000 :    4.028774 ]
set yrange [      -19.57 :       60.53 ]
#set yrange [       -5.00 :        5.00 ]
set yrange [       -10.00 :        15.00 ]

set ylabel 'E - E_F [eV]'
set xtics (  \
 "{/Symbol G}"     0.000000   ,\
 "M"     1.474634   ,\
 "K"     2.326014   ,\
 "{/Symbol G}"     4.028774   )
#set ytics 2

fermi =  -2.7245
file = 'bands.dat'
plot \
$vl w l ls 47 dt (10,7) notitle ,\
$fl w l ls 47 notitle ,\
file u 1:($2-fermi) w l ls 3 notitle ,\
"wannier90_band.dat" u 1:($2-fermi) w l ls 4 dt (10,7) notitle


