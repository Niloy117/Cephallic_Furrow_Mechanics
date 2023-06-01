ApBsORLt = 3 # Change this line for Apical/Basal/Lateral

N_cell=37 #37 #47 #37 #33 #31 #29 #23
Hlf_Ncell=19 #19 #23 #19 #17 #15 #14 #11
Hlf_of_Hlf_Ncell=10 #10 #12 #10 #9 #8 #7
N_cycl=7 ; N_fpc=22
N_frme=(N_cycl)*(N_fpc)
strtFrmCellincr=2

NAEC_Apcl=0
NAEC_Bsal=2
NAEC_Ltrl=2

nsprsInACellNI=(NAEC_Apcl+1)+(NAEC_Bsal+1)+(NAEC_Ltrl+1)
nsprsInACellTN=1+1+1

WT=3
extnsn='.eps'

print "CREATING OUTPUT NAMES"
print " "

output1=sprintf('CellPressACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output2=sprintf('SprTnsnAndLengthACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output3=sprintf('SprLengthACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output4=sprintf('SprL0ACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output5=sprintf('SprKsACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output6=sprintf('CellAreaACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output7=sprintf('CellA0ACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)
output8=sprintf('CellkaACtotalCycl%02d_IFWT%01d%s',N_cycl,WT,extnsn)

print output1
print output2
print output3
print output4
print output5
print output5
print output6
print output7
print output8

print "CREATING PARAMETER VALUES FOR OBJECTS"

set palette model RGB defined (0.00 "white",0.25 "violet",0.5 "black", 0.75 "red", 0.87 "brown", 1 "blue")

cbr=1.0
set cbrange [0.0:cbr]
unset colorbox

array xPos[N_frme]
array yPos[N_frme]
array xPosPres[N_frme]
array yPosPres[N_frme]

cmax = 15
cf   = 0.9
selected_nc = 0
p=1
rad0=0.15
array radius[cmax]

array posCor[cmax] ; array color[cmax]
array radiusVal[cmax]

posCor[1]=01   ; posCor[2]=11   ; posCor[3]=22   ; posCor[4]=33   ; posCor[5]=44
posCor[6]=55   ; posCor[7]=66   ; posCor[8]=77   ; posCor[9]=88   ; posCor[10]=99
posCor[11]=110 ; posCor[12]=121 ; posCor[13]=132 ; posCor[14]=143 ; posCor[15]=154


do for [nn=1:cmax]{
   color[nn]=0.5
   radiusVal[nn]=0.00
   }
 
color[6]  = 0.75
color[8]  = 0.87
color[10] = 1.00
color[12] = 0.25

radiusVal[6]  = rad0
radiusVal[8]  = rad0
radiusVal[10] = rad0
radiusVal[12] = rad0

set term x11

set xrange [-100.0:100.0]
set yrange [-100.0:100.0]

do for [j=1:N_frme] {
   if(ApBsORLt==1){plot 'TensionPointsDataApcl.dat' every 1:1:j-1:0:j-1:0 u (xPos[j]=$1,yPos[j]=$2)}
   if(ApBsORLt==2){plot 'TensionPointsDataBsal.dat' every 1:1:j-1:0:j-1:0 u (xPos[j]=$1,yPos[j]=$2)}
   if(ApBsORLt==3){plot 'TensionPointsDataLtrl.dat' every 1:1:j-1:0:j-1:0 u (xPos[j]=$1,yPos[j]=$2)}
   
   plot 'PressurePointsData.dat' every 1:1:j-1:0:j-1:0 u (xPosPres[j]=$1,yPosPres[j]=$2)
   print xPos[j],yPos[j]
   print xPosPres[j],yPosPres[j]}
   
unset xrange
unset yrange

set term postscript eps enhanced color dashed lw 1 "Times-New-Roman" 9
set nokey
#set xlabel "t(T)" offset -1,0 font "Times new roman,24"
#set ylabel "P" offset 0,0 font "Times new roman,24"

unset grid 


#set title 'Pressure Variation of All Cells' font "Times new roman,14"
set output output1


xshift = 10.50
firstPlottedPressure=0.16002494682293261

addToRnge=(1.00-0.15)

xmin = 0.000-xshift+0.50
xmax = 19.00-xshift-0.50

yticsMin=0.50 ; yticsMax=1.50
yRngeExtnsn=0.02

ymin=yticsMin-yRngeExtnsn
ymax=yticsMax+yRngeExtnsn

noOfYticsBarStrt = 4.0
YticsDiff = (yticsMax-yticsMin)/(noOfYticsBarStrt)

print "ymax-ymin"
print ymin,ymax

set xtics xmin,2.00,xmax font "Times new roman,24" offset 1,-1
set ytics yticsMin,YticsDiff,yticsMax font "Times new roman,24" offset -1

set xrange [xmin:xmax]
set yrange [ymin:ymax]

set lmargin 12
set rmargin 10
set bmargin 10
set tmargin 10

#set arrow from 12.0, graph 0 to 12.0, graph 1 nohead lw 1.0 #lc rgb 'black' ls 5 lt 8 lw 1
#set arrow from 6.00, graph 0 to 6.00, graph 1 nohead lw 3.0 #lc rgb 'black' ls 5 lt 8 lw 1

generate_circles2='print "generating circles";\
print "cmax=",cmax,", selected_nc=",selected_nc,", cf=",cf,", p=",p;\
do for [nn=1:cmax]{\
xPosV = posCor[nn] ; yPosV = posCor[nn] ; \
nof = 10+nn ; noc = 40+nn ; xx = xPosPres[xPosV]-xshift ; yy = yPosPres[yPosV] ; rad=rad0*cf**0; print xx,yy;\
set object nof circle at xx,yy size radiusVal[nn] fc palette frac color[nn] lw 2 fillstyle empty front;\
set object noc circle at xx,yy size radiusVal[nn] fc palette frac color[nn] lw 2 fillstyle solid back;\
fcLabel=sprintf("%2.2f",cf**0);\
};\
print "LAST"'

#@generate_circles2

set multiplot

imax=(Hlf_Ncell/N_cycl) + 1

do for [i=1:imax] {
   
   if (i==1) {cellNm=1 + (strtFrmCellincr) }
   if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr)}
   
   #add=(i-1)*(N_cycl-1)*22 #- (i-1)
   add=(i-1)*6.00
   print add
   ltVal = i
   plot sprintf('PressEvolAC%03d.dat',cellNm) u ($2+add-xshift):($3/firstPlottedPressure) w l lt 8 lw 4	#divide not add
}

unset multiplot
unset arrow
unset title

xshift = 10.50
xmin   = 0.000-xshift+0.50
xmax   = 19.00-xshift-0.50

if (ApBsORLt == 1) {firstPlottedTnsn=0.67038329842130495 ; firstPlottedLength=2.0524301198486867}
if (ApBsORLt == 2) {firstPlottedTnsn=0.73894720312743511 ; firstPlottedLength=2.1240359158776458}
if (ApBsORLt == 3) {firstPlottedTnsn=0.31722440079966124 ; firstPlottedLength=8.7353421688103676}

y1min  = 0.00  #-1.00
y1max  = 2.50
y2min  = 0.00 
y2max  = 2.50

noOfXticsBarStrt  = (19.0-1.00)*(0.50)
noOfY1ticsBarStrt = 5.0
noOfY2ticsBarStrt = 5.0

xticsDiff  = (xmax-xmin)/(noOfXticsBarStrt)
y1ticsDiff = (y1max-y1min)/(noOfY1ticsBarStrt)
y2ticsDiff = (y2max-y2min)/(noOfY2ticsBarStrt)

set lmargin 12
set rmargin 10
set bmargin 10
set tmargin 10

#set title 'Evolution of Tension and Length of Lateral Membranes' font "Times new roman,24"
set output output2

generate_circles1='print "generating circles";\
print "cmax=",cmax,", selected_nc=",selected_nc,", cf=",cf,", p=",p;\
do for [nn=1:cmax]{\
xPosV = posCor[nn] ; yPosV = posCor[nn] ; \
nof = 10+nn ; noc = 40+nn ; xx = xPos[xPosV]-xshift ; yy = -yPos[yPosV] ; rad=rad0*cf**0; print xx,yy;\
set object nof circle at xx,yy size radiusVal[nn] fc palette frac color[nn] lw 2 fillstyle empty front;\
set object noc circle at xx,yy size radiusVal[nn] fc palette frac color[nn] lw 2 fillstyle solid back;\
fcLabel=sprintf("%2.2f",cf**0);\
};\
print "LAST"'

#@generate_circles1


keyxDiff  = (xmax-xmin)/20.0
keyy1Diff = (y1max-y1min)/20.0
keyy2Diff = (y2max-y2min)/20.0

set key font "Times new roman,24"

#if (ApBsORLt==1) {set ylabel  "{/Symbol s}_a" offset -1,0 font "Times new roman,24"}
#if (ApBsORLt==2) {set ylabel  "{/Symbol s}_b" offset -1,0 font "Times new roman,24"}
#if (ApBsORLt==3) {set ylabel  "{/Symbol s}_l" offset -1,0 font "Times new roman,24"}

#set y2label "{/:Italic l}"  offset 0,0 font "Times new roman,24" 

set xtics xmin,xticsDiff,xmax font "Times new roman,24"
set ytics y1min,y1ticsDiff,y1max font "Times new roman,24"
#set y2tics y2min,y2ticsDiff,y2max font "Times new roman,24"
set nogrid

set xrange [xmin:xmax]
set yrange [y1min:y1max]
set y2range [y2min:y2max]

set multiplot

#set arrow from 12.0, graph 0 to 12.0, graph 1 nohead lw 1.0 #lc rgb 'black' ls 5 lt 8 lw 1
#set arrow from 6.00, graph 0 to 6.00, graph 1 nohead lw 1.5 #lc rgb 'black' ls 5 lt 8 lw 1


do for [i=1:imax] {
                       
   if (i==1) {cellNm=1 + (strtFrmCellincr)}
   if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr)}
        
   sprA1=(cellNm-1)*nsprsInACellNI + 1 ; sprA2=(cellNm-1)*nsprsInACellTN + 1
   sprB1=(cellNm-1)*nsprsInACellNI + 3 ; sprB2=(cellNm-1)*nsprsInACellTN + 2
   sprL1=(cellNm-1)*nsprsInACellNI + 6 ; sprL2=(cellNm-1)*nsprsInACellTN + 3
   
   #add=(i-1)*(N_cycl-1)*22
   add=(i-1)*6.00
    
   ltVal = i
      
   #set key at (xmax-0.0*keyxDiff),(ymax-1.0*keyyDiff)
   #plot sprintf('TnsnEvolAC%03d.dat',sprA1) u ($2+add):($3) w l lt 6 lw 2 title 'Apical Membrane'
   #set key at (xmax-0.0*keyxDiff),(ymax-2.0*keyyDiff)
   #plot sprintf('TnsnEvolAC%03d.dat',sprB1) u ($2+add):($3) w l lt 7 lw 2 title 'Basal Membrane'
   
   
   if (ApBsORLt==1) {plot sprintf('TnsnEvolAC_OLM%03d.dat',sprA1) u ($2+add-xshift):(-$3/firstPlottedTnsn) w l lt 8 lw 4 title '{/Symbol s}_a' axes x1y1,\
   	sprintf('SprPropEvolSgmntdToCmbndAC_OLM%03d.dat',sprA2) u ($2+add-xshift):($5/firstPlottedLength) w l lt 8 lw 4 dt 2 title '{/:Italic l}' axes x1y2}
   if (ApBsORLt==2) {plot sprintf('TnsnEvolAC_OLM%03d.dat',sprB1) u ($2+add-xshift):(-$3/firstPlottedTnsn) w l lt 8 lw 4 title '{/Symbol s}_b' axes x1y1,\
   	sprintf('SprPropEvolSgmntdToCmbndAC_OLM%03d.dat',sprB2) u ($2+add-xshift):($5/firstPlottedLength) w l lt 8 lw 4 dt 2 title '{/:Italic l}' axes x1y2}
   if (ApBsORLt==3) {plot sprintf('TnsnEvolAC_OLM%03d.dat',sprL1) u ($2+add-xshift):(-$3/firstPlottedTnsn) w l lt 8 lw 4 title '{/Symbol s}_l' axes x1y1,\
   	sprintf('SprPropEvolSgmntdToCmbndAC_OLM%03d.dat',sprL2) u ($2+add-xshift):($5/firstPlottedLength) w l lt 8 lw 4 dt 2 title '{/:Italic l}' axes x1y2}
   	
   }
   
unset multiplot

pause 1

# xmin = 0.00
# xmax = 18.00
# ymin = 0.00
# ymax = 12.00

# keyxDiff=(xmax-xmin)/20.0
# keyyDiff=(ymax-ymin)/20.0    

# set ylabel "Length" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,1,ymax
# set grid 
# show grid

# set title 'length Variation of Membranes' font "Times new roman,14"
# set output output3

# set xrange [xmin:xmax]
# set yrange [ymin:ymax]

# set multiplot

# do for [i=1:imax] {
                  
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr)}
   
#    sprA=(cellNm-1)*nsprsInACellTN + 1
#    sprB=(cellNm-1)*nsprsInACellTN + 2
#    sprL=(cellNm-1)*nsprsInACellTN + 3
   
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    set key at (xmax-keyxDiff),(ymax-keyyDiff)
#    plot sprintf('SprPropEvolSgmntdToCmbndAC%03d.dat',sprA) u (($1+add)/22):($5) w l lt 6 lw 2 title 'Apical Membrane'
#    set key at (xmax-keyxDiff),(ymax-2.0*keyyDiff) 
#    plot sprintf('SprPropEvolSgmntdToCmbndAC%03d.dat',sprB) u (($1+add)/22):($5) w l lt 7 lw 2 title 'Basal Membrane'
#    set key at (xmax-keyxDiff),(ymax-3.0*keyyDiff) 
#    plot sprintf('SprPropEvolSgmntdToCmbndAC%03d.dat',sprL) u (($1+add)/22):($5) w l lt 8 lw 2 title 'Lateral Membrane'
      
# }

# unset multiplot


# xmin = 0.00
# xmax = 18.00
# ymin = 0.00
# ymax = 5.00

# keyxDiff=(xmax-xmin)/20.0
# keyyDiff=(ymax-ymin)/20.0

# set ylabel "Length" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,1,ymax
# set grid 
# show grid

# set title 'l_0 Variation of Membranes' font "Times new roman,14"
# set output output4

# set xrange [xmin:xmax]
# set yrange [ymin:ymax]


# set multiplot

# do for [i=1:imax] {
   
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr) }
   	
#    sprA=(cellNm-1)*nsprsInACellNI + 1
#    sprB=(cellNm-1)*nsprsInACellNI + 3
#    sprL=(cellNm-1)*nsprsInACellNI + 6
   
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    set key at (xmax-keyxDiff),(ymax-keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprA) u (($1+add)/22):($7) w l lt 6 lw 2 title 'Apical Membrane'
#    set key at (xmax-keyxDiff),(ymax-2.0*keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprB) u (($1+add)/22):($7) w l lt 7 lw 2 title 'Basal Membrane'
#    set key at (xmax-keyxDiff),(ymax-3.0*keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprL) u (($1+add)/22):($7) w l lt 8 lw 2 title 'Lateral Membrane'
   
# }

# unset multiplot

# xmin = 0.00
# xmax = 18.00
# ymin = 0.00
# ymax = 10.00

# keyxDiff=(xmax-xmin)/20.0
# keyyDiff=(ymax-ymin)/20.0

# set ylabel "Length" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,1,ymax
# set grid 
# show grid

# set title 'k_s Variation of Membranes' font "Times new roman,14"
# set output output5

# set xrange [xmin:xmax]
# set yrange [ymin:ymax]

# set multiplot

# do for [i=1:imax] {
   
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr)}
   
#    sprA=(cellNm-1)*nsprsInACellNI + 1
#    sprB=(cellNm-1)*nsprsInACellNI + 3
#    sprL=(cellNm-1)*nsprsInACellNI + 6
   
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    set key at (xmax-keyxDiff),(ymax-keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprA) u (($1+add)/22):($3) w l lt 6 lw 2 title 'Apical Membrane'
#    set key at (xmax-keyxDiff),(ymax-2.0*keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprB) u (($1+add)/22):($3) w l lt 7 lw 2 title 'Basal Membrane'
#    set key at (xmax-keyxDiff),(ymax-3.0*keyyDiff)
#    plot sprintf('SprPropEvolAC%03d.dat',sprL) u (($1+add)/22):($3) w l lt 8 lw 2 title 'Lateral Membrane'
      
# }

# unset multiplot
      
      
# xmin = 0.00
# xmax = 18.00
# ymin = 8.00
# ymax = 22.00

# set ylabel "Area" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,2,ymax
# set grid 
# show grid

# set title 'Area Variation of Cells' font "Times new roman,14"
# set output output6

# set xrange [xmin:xmax]
# set yrange [ymin:ymax]

# set multiplot

# do for [i=1:imax] {
   
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr) }
   
   
#    #add=i-1
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    plot sprintf('AreaPropEvolAC%03d.dat',cellNm) u (($1+add)/22):($5) w l lt 6 lw 2
   
# }

# unset multiplot

# xmin = 0.00
# xmax = 18.00
# ymin = 8.00
# ymax = 22.00

# set ylabel "A0 variation of Cells" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,2,ymax
# set grid 
# show grid

# set title 'A_0 Variation of All Laterals' font "Times new roman,14"
# set output output7
    
# set xrange [xmin:xmax]
# set yrange [ymin:ymax]

# set multiplot

# do for [i=1:imax] {
        
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr) }
   
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    plot sprintf('AreaPropEvolAC%03d.dat',cellNm) u (($1+add)/22):($7) w l lt 6 lw 2
   
# }

# unset multiplot

# xmin = 0.00
# xmax = 18.00
# ymin = 0.05
# ymax = 0.1

# set ylabel "k_a variation of Cells" offset 2,0 font "Times new roman,14"

# set xtics xmin,1,xmax
# set ytics ymin,0.01,ymax
# set grid 
# show grid

# set title 'k_a Variation of All Laterals' font "Times new roman,14"
# set output output8
    
# set xrange [xmin:xmax]
# set yrange [ymin:ymax]

# set multiplot

# do for [i=1:imax] {
      
#    if (i==1) {cellNm=1 + (strtFrmCellincr) }
#    if (i >1) {cellNm=(i-1)*(N_cycl)-(i-2) + (strtFrmCellincr) }
   
#    add=(i-1)*(N_cycl-1)*22
   
#    ltVal = i
   
#    plot sprintf('AreaPropEvolAC%03d.dat',cellNm) u (($1+add)/22):($3) w l lt 6 lw 2
   
# }

# unset multiplot

pause -1

