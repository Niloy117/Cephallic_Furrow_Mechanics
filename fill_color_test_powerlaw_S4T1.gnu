
max(x,y)=(x<y)?y:x
ftr(x,alpha)=abs(x)**(1.+alpha)/x
alpha=0.5

set term x11 enhanced font "arial,15" 
set size ratio -1
set angles degrees

unset key
unset border
set notics

set xrange [-17.00:17.00]
set yrange [-20.50:2.00]

set palette model RGB defined (-1 "red",0 "white", 1 "blue")
cbr=1.
set cbrange [-cbr:cbr]

N_iter=1

ncells = 17
nsprs  = 53
ncurve = 31

array fcolorArea[ncells]
array fcolorSpr[nsprs]

stressScaleA=0.19
stressScaleS=0.53

stressMaxA=0.
stressMaxS=0.

n1=0.
n2=0.
n3=0.

do for [n=1:N_iter]{
   key_val=0+(n-1)*2
   
   set output
   set term x11
   
   datadir='./'
   datafileArea = datadir.sprintf('S4T1_strct1_AW%03d.dat',n)
   datafileSpr  = datadir.sprintf('S4T1_strct1_SW%03d.dat',n)
   #datafileArea = datadir.sprintf('S4T1_strct2_AW%03d.dat',n)
   #datafileSpr  = datadir.sprintf('S4T1_strct2_SW%03d.dat',n)
   
   #datafileArea = datadir.sprintf('NI_modelAW%03d.dat',n)
   #datafileSpr  = datadir.sprintf('NI_modelSW%03d.dat',n)
   #datafileArea = datadir.sprintf('BeginStg_AW%03d.dat',n)
   #datafileSpr  = datadir.sprintf('BeginStg_SW%03d.dat',n)
   #datafileArea = datadir.sprintf('IdontKnwAW%03d.dat',n)
   #datafileSpr  = datadir.sprintf('IdontKnwSW%03d.dat',n)
   
   do for [i=1:ncells]{
      plot datafileArea every 1:1:0:i-1:0:i-1 u (stressA=$1)
      plot datafileArea every 1:1:0:i-1:0:i-1 u (fcolorArea[i]=(1.+ftr($1/stressScaleA,alpha))/2.)
      #print i,fcolorArea[i]
      stressMaxA=max(stressMaxA,abs(stressA))
      #print i,stressA,stressMaxA
      }
    
   do for [i=1:nsprs]{
      plot datafileSpr every 1:1:0:i-1:0:i-1 u (stressS=$1)
      plot datafileSpr every 1:1:0:i-1:0:i-1 u (fcolorSpr[i]=(1.+ftr($1/stressScaleS,alpha))/2.)
      #print i,fcolorSpr[i]
      stressMaxS=max(stressMaxS,abs(stressS))
      }
	 	 
   #print n,stressMaxA,stressMaxS
   
   	 
   set term gif
   outputfile=sprintf('Exp0_%03d.gif',n)
   set output outputfile
   
   set multiplot
   #set title sprintf("frame%02d",(n+0))
   		
   plot for [i=1:ncells] datafileArea every 1:1:1:i-1::i-1 w filledcurves lc palette frac fcolorArea[i]
   
   plot \
   for [i=1:nsprs] datafileSpr every 1:1:1:i-1::i-1 w l lt 1 lc rgb "grey" lw 4,\
   for [i=1:nsprs] datafileSpr every 1:1:1:i-1::i-1 w filledcurves lc palette frac fcolorSpr[i] lw 2
  
   unset multiplot
   
}

pause -1

#Exp0_aftOptmzToIncrseCg
#Exp2_l0ShorteningSecndMidSprSW%03d.dat
#Exp3_l0ShorteningPulleySprSW%03d.dat
#Exp4_l0ShorteningApicalSprNxtToPulleySW%03d.dat
#Exp5_l0ShorteningDiagonalSprSW%03d.dat
#Exp6_l0ShorteningBasalSprNxtToDiagSw%03d.dat
#Exp7_l0ShorteningThrdMidSprSW%03d.dat
#Exp8_l0ShorteningVertSprBfrPulleySW%03d.dat
#Exp0_atCg0_A0_incr_thrdPairfrmBottm
#Exp0_atCg0_A0_incr_optmz_A0incrthrdPairfrmBottm_optmz
#Exp0_Cg0_bot6_A0incr_SW
