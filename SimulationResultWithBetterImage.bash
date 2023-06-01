#! /bin/bash

rm Exp0_???.pdf
rm flNm_*.pdf

gnuplot fill_color_test_powerlaw_stage1E.gnu
#gnuplot fill_color_test_powerlaw_NIadded.gnu   #%%%%%IMPORTANT%%%%%%
convert -delay 100 Exp0_???.gif OneNodeNI.gif
#exit
countMax=2 #46 #14 #399 #44 #388 #47 #224 #8 #38 #27 #6 #8 #16 #28
count=1
loopCount=1

echo variables 
echo parameters values are $cyclNo $NfrmPerCycl $countMax $count $loopCount

while [ $count -le $countMax ] ; do
    
    #echo The counter is $count
    
    prevFlnm=$(printf "Exp0_%03d.eps"  "$count")
    currFlnm=$(printf "Exp0_%03d.pdf"  "$count")
    cropFlnm=$(printf "Exp0_c%03d.pdf" "$count")
    
    echo $prevFlnm $currFlnm
    ps2pdf $prevFlnm $currFlnm
    pdfcrop --margins '40 40 40 40' --clip  $currFlnm $cropFlnm
    
    #ps2pdf -dEPSCrop $prevFlnm
    #ps2pdf -dPDFSETTINGS=/prepress -dEPSCrop $prevFlnm
    
    let count=count+1
    
    flNm=$(printf "flNm_%03d.pdf" "$loopCount")
    echo $flNm
    
    cp $cropFlnm $flNm
    echo $cropFlnm $flNm
    
    let loopCount=loopCount+1
done

pdftk flNm_*.pdf cat output OneNodeNI00.pdf

pdftk OneNodeNI00.pdf cat output OneNodeNI.pdf

#finalPdf=$(printf "stage1E.pdf")
#finalPdf=$(printf "ImageCreatn.pdf")
#finalPdf=$(printf "InitatorCellIncr3.pdf")
#finalPdf=$(printf "ForceIm4.pdf")
#finalPdf=$(printf "StiffArea.pdf")

#finalPdf=$(printf "ApplyingForceVrticallyDown3.pdf") #3 is only aft node added
#finalPdf=$(printf "ApplyingForceVrticallyDown2.pdf") #2 is for only NI,no begin in btwn
#finalPdf=$(printf "ApplyingForceVrticallyDown7NoNode.pdf")

#finalPdf=$(printf "SimlnForRebuttal.pdf")
#finalPdf=$(printf "First_HalfCycle.pdf")
#finalPdf=$(printf "CurvatureReductionAtCellTwoBfrInvgntngCell1.pdf")
#finalPdf=$(printf "lengthMatching.pdf")
#finalPdf=$(printf "Dignl_Tension_Test_Tr2.pdf")
#finalPdf=$(printf "ApclIncr_and_IncrForceToHold1.pdf") #Frm16-27 [pdf 1]
#finalPdf=$(printf "ReduceTilt_Tr2_Aftpdf2.pdf") #Frm28-32

#finalPdf=$(printf "Reduce_Force_On_Basal_Boundary7.pdf")
#finalPdf=$(printf "Reduce_Force_On_Apical_Boundary5.pdf")
#finalPdf=$(printf "Force_On_Both_Boundary.pdf")
#finalPdf=$(printf "RestrctAftCM1_withForceOnBothBndry.pdf")

#finalPdf=$(printf "RestrctBfrCM1_withForceDownwards.pdf")
finalPdf=$(printf "cell_dfrmtn.pdf")

echo $finalPdf

cp OneNodeNI.pdf $finalPdf

rm Exp0_*.pdf
rm flNm_*.pdf

evince $finalPdf &
