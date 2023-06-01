set term x11 enhanced font "arial,15"
set term png
set nokey
set xlabel "Time (T)" offset 0,0.5 font "Times new roman,14"
set ylabel "Tension" offset 2,0 font "Times new roman,14"

N_spr = 48 #50

do for [i=1:N_spr] {
   
   set title sprintf('Tension Variation of Spring %02d',i) font "Times new roman,14"
   show title
   
   xmin = 0.00
   xmax = 1.00
   
   ymin = -1.10
   ymax = 1.10
   
   set xrange [xmin:xmax]
   set yrange [ymin:ymax]
   
   set xlabel "Time (T)" offset 0,0.5 font "Times new roman,14"	
   set ylabel "Tension" offset 2,0 font "Times new roman,14"
   
   set output sprintf('SprTnsnTWPC2_%03d.png',i)
   plot sprintf('TnsnEvolPC2_%03d.dat',i) u 2:4 w l lt 8 lw 2
   
   set title sprintf('Tension Variation of Spring %02d',i) font "Times new roman,14"
   show title
      
   xmin = 1
   xmax = 22
   
   ymin = -1.02
   ymax = 1.02
       
   set xrange [xmin:xmax]
   set yrange [ymin:ymax]
   set xlabel "Frame No" offset 0,0.5 font "Times new roman,14"
   set ylabel "Tension" offset 2,0 font "Times new roman,14"
   
   set output sprintf('SprTnsnFWPC2_%03d.png',i)
   plot sprintf('TnsnEvolPC2_%03d.dat',i) u 1:4 w l lt 8 lw 2
   
   set title sprintf('Sping Stiffness Variation of Spring %02d',i) font "Times new roman,14"
   show title
   
   ymin = -0.02
   ymax = 1.02
   set yrange [ymin:ymax]
   
   set xlabel "Frame No" offset 0,0.5 font "Times new roman,14"
   set ylabel "Spring Co-efficient (ks)" offset 2,0 font "Times new roman,14"
   
   set output sprintf('SprConstPC2_%03d.png',i)
   plot sprintf('SprPropEvolPC2_%03d.dat',i) u 1:4 w l lt 8 lw 2
   
   set title sprintf('Sping length Variation of Spring %02d',i) font "Times new roman,14"
   show title
   
   set xlabel "Frame No" offset 0,0.5 font "Times new roman,14"
   set ylabel "Spring length (l)" offset 2,0 font "Times new roman,14"
   
   
   set output sprintf('SprLenPC2_%03d.png',i)
   plot sprintf('SprPropEvolPC2_%03d.dat',i) u 1:6 w l lt 8 lw 2
   
   set title sprintf('Spring Rest length Variation of Spring %02d',i) font "Times new roman,14"
   show title
   
   set xlabel "Frame No" offset 0,0.5 font "Times new roman,14"
   set ylabel "Spring rest length (l_0)" offset 2,0 font "Times new roman,14"
   
   set output sprintf('SprRestLenPC2_%03d.png',i)
   plot sprintf('SprPropEvolPC2_%03d.dat',i) u 1:8 w l lt 8 lw 2
   
}

pause -1
