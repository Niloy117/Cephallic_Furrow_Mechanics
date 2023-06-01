ApBsORLt = 2 # Change this line for Apical/Basal/Lateral

N_cell=23
Hlf_Ncell=11

N_cycl=7 ; N_fpc=22
N_frme=(N_cycl)*(N_fpc)
strtFrmCellincr=2

NAEC_Apcl=0
NAEC_Bsal=2
NAEC_Ltrl=2

nsprsInACellNI=(NAEC_Apcl+1)+(NAEC_Bsal+1)+(NAEC_Ltrl+1)
nsprsInACellTN=1+1+1

set term postscript eps enhanced color dashed lw 1 "Times-New-Roman" 9
extnsn='.eps'

case=1

if (case==1){
   SprFile1=sprintf('CnvrtdSprPrpImprfctCase.dat')
   CellFile1=sprintf('CellPrpImprfctCase.dat')}
   
if (case==2){
   SprFile1=sprintf('CnvrtdSprPrpPrfctCase038.dat')
   CellFile1=sprintf('CellPrpPrfctCase038.dat')}

if (case==3){
   SprFile1=sprintf('CnvrtdSprPrpPrfctCase388.dat')
   CellFile1=sprintf('CellPrpPrfctCase388.dat')}


print "CREATING OUTPUT NAMES"
print " "

if (case==1){
   output1=sprintf('ApclTnsnAndLengthImp%s',extnsn)
   output2=sprintf('BsalTnsnAndLengthImp%s',extnsn)
   output3=sprintf('LtrlTnsnAndLengthImp%s',extnsn)
   output7=sprintf('PresImp%s',extnsn)}

if (case==2){
   output1=sprintf('ApclTnsnAndLengthPrf038%s',extnsn)
   output2=sprintf('BsalTnsnAndLengthPrf038%s',extnsn)
   output3=sprintf('LtrlTnsnAndLengthPrf038%s',extnsn)
   output7=sprintf('PresPrf038%s',extnsn)}

if (case==3){
   output1=sprintf('ApclTnsnAndLengthPrf388%s',extnsn)
   output2=sprintf('BsalTnsnAndLengthPrf388%s',extnsn)
   output3=sprintf('LtrlTnsnAndLengthPrf388%s',extnsn)
   output7=sprintf('PresPrf388%s',extnsn)}

TouchingCell=6
TouchingCellReal=6.00

xmin=0.99-TouchingCellReal
xmax=11.01-TouchingCellReal
ymin=-0.01
ymax=3.05

set xrange [xmin:xmax]
set xtics xmin+1.01,2,xmax-0.01
set xtics offset 0,-1.5 font "Times-New-Roman,30"

set yrange [ymin:ymax]
set ytics ymin+0.01,0.5,ymax-0.05
set ytics offset -1,0 font "Times-New-Roman,30"
set nokey

if (case==1){
   ApclTnsnSpr1=0.69402877045635558 ; ApclLenSpr1=2.0878983279012626 
   BsalTnsnSpr1=0.75457936636537637 ; BsalLenSpr1=2.1457525684529251
   LtrlTnsnSpr1=0.25928990167497118 ; LtrlLenSpr1=8.2921591419971747
   PresFrstCell=0.17505099401493188}
   
if (case==2){
   ApclTnsnSpr1=0.68406830674595498  ; ApclLenSpr1=2.0729576323356618
   BsalTnsnSpr1=0.73598212556880727  ; BsalLenSpr1=2.1189224962108510
   LtrlTnsnSpr1=0.24959157697143761  ; LtrlLenSpr1=8.2294819876883736
   PresFrstCell=0.17252536058560372}

if (case==3){
   ApclTnsnSpr1=0.68273960664986166 ; ApclLenSpr1=2.0709645821915217 
   BsalTnsnSpr1=0.73662417598808472 ; BsalLenSpr1=2.1204180484832378 
   LtrlTnsnSpr1=0.25239605683433075 ; LtrlLenSpr1=8.2489326179721196
   PresFrstCell=0.17219487001836400}


#'{/Symbol s}_a{{/Symbol s}_a}^{/Symbol (\245)}'

#set xlabel "Cell Number" offset 0,0 font "Times new roman,14"
#set ylabel "Tension" offset 3,0 font "Times new roman,14"
#set y2label "Length" offset 1,0 font "Times new roman,14"


print output1

set output output1
set multiplot

#title '{/Symbol s}_a'
#title '{/:Italic l}_a'

plot SprFile1 using ($1-TouchingCell):(-$2/ApclTnsnSpr1) w lp lw 3 pt 7 ps 2.5 lc rgb 'black',\
     SprFile1 using ($1-TouchingCell):($8/ApclLenSpr1)   w lp lw 3 pt 9 ps 2.5 lc rgb 'black'

     
unset multiplot

print output2
set output output2
set multiplot

plot SprFile1 using ($1-TouchingCell):(-$3/BsalTnsnSpr1) w lp lw 3 pt 7 ps 2.5 lc rgb 'black',\
     SprFile1 using ($1-TouchingCell):($9/BsalLenSpr1)   w lp lw 3 pt 9 ps 2.5 lc rgb 'black'

unset multiplot

print output3
set output output3
set multiplot

plot SprFile1 using ($1-TouchingCell):(-$4/LtrlTnsnSpr1) w lp lw 3 pt 7 ps 2.5 lc rgb 'black',\
     SprFile1 using ($1-TouchingCell):($10/LtrlLenSpr1)  w lp lw 3 pt 9 ps 2.5 lc rgb 'black'

unset multiplot

ymin=-0.01
ymax=2.51
set yrange [ymin:ymax]
set ytics ymin+0.01,0.50,ymax-0.01

print output7
set output output7
#set xlabel "Cell Number" offset 0,0 font "Times new roman,14"
#set ylabel "Pressure" offset 3,0 font "Times new roman,14"
unset y2label

set multiplot

plot CellFile1 using ($1-TouchingCell):($2/PresFrstCell) w lp lw 3 pt 7 ps 2.5 lc rgb 'black'

unset multiplot

pause -1