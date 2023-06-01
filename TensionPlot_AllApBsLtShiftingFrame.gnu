
set term x11 enhanced font "arial,15"
set term png
set style data line

set key on
show key

N_spr=66
N_ApBsLtSpr=4 #2
hlf_Ncell=7 #5
spr_nm=0

array apclSpr[N_ApBsLtSpr]
array bsalSpr[N_ApBsLtSpr]
array ltrlSpr[N_ApBsLtSpr]

do for [i=4:hlf_Ncell] {
   apclSpr[i-3]=(3*i)-2
   bsalSpr[i-3]=(3*i)-1
   ltrlSpr[i-3]=(3*i)-0
}


#set output sprintf('TnsnApclShftFW.png')
#set output sprintf('TnsnBsalShftFW.png')
#set output sprintf('TnsnLtrlShftFW.png')

#set output sprintf('ksApclShftFW.png')
#set output sprintf('ksBsalShftFW.png')
#set output sprintf('ksLtrlShftFW.png')

#set output sprintf('LngthApclShftFW.png')
#set output sprintf('LngthBsalShftFW.png')
#set output sprintf('LngthLtrlShftFW.png')

#set output sprintf('RstLngthApclShftFW.png')
#set output sprintf('RstLngthBsalShftFW.png')
#set output sprintf('RstLngthLtrlShftFW.png')

#set output sprintf('ll0_diffApclShftFW.png')
#set output sprintf('ll0_diffBsalShftFW.png')
set output sprintf('ll0_diffLtrlShftFW.png')

#set output sprintf('TnsnWithKvalApclShftFW.png')
#set output sprintf('TnsnWithKvalBsalShftFW.png')
#set output sprintf('TnsnWithKvalLtrlShftFW.png')

#set title sprintf('Tension Variation of Apical Membranes')  font "Times new roman,18"
#set title sprintf('Tension Variation of Basal Membranes')   font "Times new roman,18"
#set title sprintf('Tension Variation of Lateral Membranes') font "Times new roman,18"

#set title sprintf('Spring elastic coefficient (k_s) Variation of Apical Membranes') font "Times new roman,18"
#set title sprintf('Spring elastic coefficient(k_s) Variation of Basal Membranes')   font "Times new roman,18"
#set title sprintf('Spring elastic coefficient(k_s) Variation of Lateral Membranes') font "Times new roman,18"

#set title sprintf('Length Variation (l) of Apical Membranes')  font "Times new roman,18"
#set title sprintf('Length Variation (l) of Basal Membranes')   font "Times new roman,18"
#set title sprintf('Length Variation (l) of Lateral Membranes') font "Times new roman,18"

#set title sprintf('Rest Length (l_0) Variation of Apical Membranes')  font "Times new roman,18"
#set title sprintf('Rest Length (l_0) Variation of Basal Membranes')   font "Times new roman,18"
#set title sprintf('Rest Length (l_0) Variation of Lateral Membranes') font "Times new roman,18"

#set title sprintf('(l-l_0) Variation of Apical Membranes')  font "Times new roman,18"
#set title sprintf('(l-l_0) Variation of Basal Membranes')   font "Times new roman,18"
set title sprintf('(l-l_0) Variation od Lateral Membranes') font "Times new roman,18"

#set title sprintf('Edited Tension Variation of Apical Membranes')   font "Times new roman,18"
#set title sprintf('Edited Tension Variation of Basal Membranes')    font "Times new roman,18"
#set title sprintf('Edited Tension Variation of Lateral Membranes')  font "Times new roman,18"

show title
set multiplot

#ref_spring=19
#ref_spring=20
ref_spring=21

period=22.
spr_nm=0
  
xmin=-95
xmax=+95

ymin      = -0.40 #-0.2(T),0.00(ks),0.00(L),[0.00(A+B);-0.40(L)](l-l0),[-0.2(A+B);-0.10(L)](ET)
ymax      = 2.00  #0.70(T),[1.00(Apcl_ks);4.00(Bsal_ks);0.60(Ltrl_ks)],10.0(L),2.0(l-l0),[1.20(A+B),0.40(L)](ET)
yticsIncr = 0.40  #0.20(T),[0.20(Apcl_ks);0.80(Bsal_ks);0.12(Ltrl_ks)],2.00(L),0.4(l-l0),[0.30(A+B);0.10(L)](ET)

set xrange [xmin:xmax]
set yrange [ymin:ymax]

set xtics -88,22,88
set grid xtics mytics
set ytics ymin,yticsIncr,ymax

keyy0    = 0.10    #-0.00(T), [0.20(Apcl_ks);0.80(Bsal_ks);0.12(Ltrl_ks)] , 9.00(L),    [0.70(A);1.60(B),0.10(L)](l-l0),[0.10(A);1.00(B),0.35(L)](ET)
keyy_incr= -0.1   #-0.032(T),[-0.04(Apcl_ks);-0.16(Bsal_ks);-0.024(Ltrl_ks)],-0.70(L), [-0.10(l-l0)],[-0.05(A+B);-0.02(L)](ET)

set xlabel "Frame No" offset 0,0.5 font "Times new roman,18"

#set ylabel "Tension" offset 2,0 font "Times new roman,18"
#set ylabel "Membrane elastic coefficient (k_s)" offset 2,0 font "Times new roman,18"
#set ylabel "Length (l)" offset 2,0 font "Times new roman,18"
#set ylabel "Rest length (l_0)" offset 2,0 font "Times new roman,18"
set ylabel "(l-l_0)" offset 2,0 font "Times new roman,18
#set ylabel "Edited Tension" offset 2,0 font "Times new roman,18"

kval= (1.00/7.50) #1.50(A+B),7.50(L)

do for [i=1:N_ApBsLtSpr] {
   
   spr_nm=apclSpr[i]
   #spr_nm=bsalSpr[i]
   #spr_nm=ltrlSpr[i]
   print i,spr_nm
    
   shift=(ref_spring-spr_nm)*period/3.
   
   print spr_nm,shift
     
   keyx  = 80.00 #75.00
   keyy = keyy0 + (i-1)*keyy_incr
   
   set key at keyx,keyy
   
   plot sprintf('TnsnEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):(-$3) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   
   #plot sprintf('SprPropEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):($3) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   #plot sprintf('SprPropEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):($5) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   #plot sprintf('SprPropEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):($7) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   #plot sprintf('SprPropEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):($5-$7) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   #plot sprintf('SprPropEvol_RduceOscill%03d.dat',spr_nm) u ($1-shift):(kval*($5-$7)) w l lt 8 lc (i-0) lw 4 title sprintf('membrane %02d',i)
   
}

#plot 'Insert1.png' binary filetype=png center=(-22,-0.1) dx=5 dy=0.1 w rgbimage

unset multiplot

pause -1
