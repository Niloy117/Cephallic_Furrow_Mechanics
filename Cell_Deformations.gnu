Lx=640
xmin=-0.7  #-2.7
xmax=+2.7  #+4.7
ymin=-2.9
ymax=+0.7 #+0.7

Ly=floor(100*(ymax-ymin)/(xmax-xmin))
print Lx,Ly
set term postscript eps enhanced color dashed lw 1 "Times-New-Roman" 9

set size ratio -1

set xrange [xmin:xmax]
set yrange [ymin:ymax]

set xtics xmin+0.2,0.5,xmax-0.2
set xtics offset 0,-1 font "Times-New-Roman,20"
set ytics ymin+0.4,0.5,ymax-0.2
set ytics offset -1,0 font "Times-New-Roman,20"
set key font 'Times-New-Roman,20'
set key at 2.7,0.60

set output 'Cell_Deformations_case1.eps'
set multiplot

plot 'xy_dfrmtn_mod2.dat'         u ($1):($2) w lp lw 3 pt 6 ps 1.0 lc rgb 'black' title 'Untilted Cell ({/Symbol a} = 0^{/Symbol \260})',\
     'xyR_dfrmtn_mod1_exp1.dat'   u ($1):($2) w lp lw 3 pt 7 ps 1.0 lc rgb 'black' title 'Tilted Cell ({/Symbol a} = 3^{/Symbol \260})',\
     'xyR_dfrmtn_mod1_exp2.dat'   u ($1):($2) w lp lw 3 pt 8 ps 1.0 lc rgb 'black' title 'Tilted Cell ({/Symbol a} = 10^^{/Symbol \260})'

unset multiplot

# set output 'Cell_Deformations_case2.eps'
# set multiplot

# plot 'xy_dfrmtn_mod2.dat'         u ($1):($2) w lp lw 3 pt 6 ps 1.0 lc rgb 'black' title 'Undeformed Cell ({/Symbol a} = 0^{/Symbol \260})',\
#      'xyR2_dfrmtn_mod2_exp1.dat'  u ($1):($2) w lp lw 3 pt 7 ps 1.0 lc rgb 'black' title 'Deformed Cell ({/Symbol a} = 1^{/Symbol \260})',\
#      'xyR2_dfrmtn_mod2_exp2.dat'  u ($1):($2) w lp lw 3 pt 8 ps 1.0 lc rgb 'black' title 'Deformed Cell ({/Symbol a} = 5^{/Symbol \260})'

# unset multiplot

pause -1
