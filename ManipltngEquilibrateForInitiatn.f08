module MAE_initaitn
  use Wfunc_and_its_derivative
  use changing_parameters
  use storing_changing_restoring_routines
  use switch_and_unswitch_models
  use nodeInsrtd_cntrlStates
  use Adding_cells
  
  implicit none
  integer              :: N_manpltdAngls
  real*8               :: ka_tobeCopied,A0_tobeCopied
  real*8               :: lowrF=0.20d0,medmF=0.25d0,higrF=0.50d0,zeroF=0.00d0
  
  real*8,  allocatable :: ks_tobeCopied(:),l0_tobeCopied(:)
  integer, allocatable :: cellNmbrs(:),crnrVal(:)
  real*8 , allocatable :: criticalAngls(:),cmprDirctn(:)
  real*8 , allocatable :: crnrAngls(:)
  integer, allocatable :: switchingXcoor(:),bfrSwitchXcoor(:)
  
contains
  
  subroutine change_shape_of_InitiatnPhase(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: hlfCyclNo
    integer, intent(in)    :: whichEnd
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    !write(*,*) Exprmnt,Frame,"DO I KNOW U?"
    
    if (hlfCyclNo==1) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo) 
    if (hlfCyclNo==2) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo) 
    if (hlfCyclNo==3) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo) 
    if (hlfCyclNo==4) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo) 
    if (hlfCyclNo==5) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    if (hlfCyclNo==6) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    if (hlfCyclNo==7) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    if (hlfCyclNo==8) call combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    
  end subroutine change_shape_of_InitiatnPhase
  
  
  subroutine combine_diffrnt_manpltn_and_variabls(hlfCyclNo,whichEnd,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: hlfCyclNo
    integer, intent(in)    :: whichEnd
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    logical :: lgcl_AnglManplt=.False.
    logical :: lgcl_strght=.False. 
    integer :: cnt_Manplt
    integer :: CC
    integer :: writORread,manpltNum
    integer :: cellNm,cellNmToCmpr,ApBsLt,TnsnDirc,maxCnt,l0ORks
    real*8  :: hmuch,hmuchA0
    integer :: upToWhchCell,idealCell
    integer :: cell_A,ApBsLt_A,cell_B,ApBsLt_B
    integer :: diffRteOrNot
    real*8  :: limTnsn
    integer :: cntInRtn,deallctnNeeds
    integer :: dirctn
    
    
    if (hlfCyclNo==1) then
       
       if (whichEnd==-1) then
          call adjust_the_firstFrm_of_EpithelialLayer(ExpNo,FrmNo)
          writORread=1 ; manpltNum=38 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       elseif (whichEnd==+1) then
          
          call get_variabls_for_diff_hlfCycl(hlfCyclNo)
          
          !  *** Manplt=1 (start)**** 
          !cnt_Manplt = 0
          !do
          !  call manplt_angls_apcl_btwn_ltrl(ExpNo,FrmNo,lgcl_AnglManplt)
          ! cnt_Manplt = cnt_Manplt+1
          ! if (lgcl_AnglManplt .eqv. .True.) exit
          ! if (cnt_Manplt==10) exit
          !enddo
          
          !write(*,*) cnt_Manplt,"--- > HOW MANY TIMES THE manplt_angls_apcl_btwn_ltrl LOOP RUNS"
          !stop
          
          !  *** Manplt=1 (End) ***
          
          !  *** Manplt=2 (start)****
          !CC=1;call apcl_bsal_l0_manplt_of_cells_make_systm_strght(ExpNo,FrmNo,CC,lgcl_strght)
          !  *** Manplt=2 (End)****
          
          !  *** Manplt=3 (start)****
          CC=2;call apcl_bsal_l0_manplt_of_cells_make_systm_strght(ExpNo,FrmNo,CC,lgcl_strght)
          !  *** Manplt=3 (End)****
          !stop
       endif
       
    elseif (hlfCyclNo==2) then
       
       if (whichEnd==-1) then
          
          !call incrPressAndMaintainShapeOFInitiatorCell(ExpNo,FrmNo)
          !writORread=2 ; manpltNum=1 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1 ; ApBsLt=2 ; TnsnDirc=+1 ; maxCnt=40
          !call change_l0ksSpr_gvnCellNmAndEquill(cellNm,ApBsLt,TnsnDirc,maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=2 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1 ; ApBsLt=1 ; TnsnDirc=-1 ; maxCnt=5
          !call change_l0ksSpr_gvnCellNmAndEquill(cellNm,ApBsLt,TnsnDirc,maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=3 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !** Not using now
          
          !TnsnDirc=+1 ; maxCnt=20
          !call initiatr_cell_bsal_l0ksIncr(TnsnDirc,maxCnt,ExpNo,FrmNo)
          !writORread=1 ; manpltNum=4 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !** Not using now
          
          !cellNm=Hlf_Ncell; ApBsLt=1; l0ORks=2; TnsnDirc=-1; hmuch=0.75d0; maxCnt=1
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo) ! reducing the apcal mem ks for cellNm=Hlf_Ncell
          !writORread=2 ; manpltNum=5 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !TnsnDirc=-1 ; maxCnt=10
          !call initiatr_cell_TwoLtrl_l0ksIncr(TnsnDirc,maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=6 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-2; ApBsLt=2 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=10
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=7 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-2; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=+1 ; hmuch=0.10d0 ; maxCnt=10
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=8 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-2; ApBsLt=2 ; l0ORks=1 ; TnsnDirc=+1 ; hmuch=0.10d0 ; maxCnt=3
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=9 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1; ApBsLt=2 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=3
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=10 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          
          !cellNm=Hlf_Ncell-1; ApBsLt=2 ; l0ORks=2 ; TnsnDirc=-1 ; hmuch=0.50d0 ; maxCnt=5
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=11 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !upToWhchCell=Hlf_Ncell-2 ; idealCell=1
          !call making_InitiatnPhase_IncmingCellsIdntcl(upToWhchCell,idealCell,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=12 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=2
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=13 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cell_A=Hlf_Ncell-1 ; ApBsLt_A=1 ; cell_B=Hlf_Ncell-1 ; ApBsLt_B=2
          !call copy_prp_frm_sprATosprB(cell_A,ApBsLt_A,cell_B,ApBsLt_B,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=14 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1; ApBsLt=3 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=1
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=15 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          
          !write(*,*) "ENTERED AS IT IS"
          
          !A0(N_cell) = 1.10d0*A0(N_cell)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !A0(N_cell) = 1.10d0*A0(N_cell)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !l0(33) = 0.90d0*l0(33) ; l0(66) = 0.90d0*l0(66) ; l0(67) = 0.90d0*l0(67)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !l0(32) = 0.90d0*l0(32)  ; l0(65) = 0.90d0*l0(65)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !stop
          
          !cellNm=Hlf_Ncell ; ApBsLt=3 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.075d0 ; maxCnt=2
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          writORread=2 ; manpltNum=16 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !CgXNode(23) = 0.10d0 ; CgXNode(47) = 0.10d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !CgXNode(23) = 0.15d0 ; CgXNode(47) = 0.15d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !CgXNode(23) = 0.20d0 ; CgXNode(47) = 0.20d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !CgXNode(23) = 0.25d0 ; CgXNode(47) = 0.25d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !CgXNode(23) = 0.30d0 ; CgXNode(47) = 0.30d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !write(*,*) "stopping in ManipltngEquilibrateForInitiatn.f08"
          !stop
          
       elseif (whichEnd==+1) then
          
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !call incrPressAndMaintainShapeOFInitiatorCell(ExpNo,FrmNo)
          !writORread=2 ; manpltNum=17 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell; ApBsLt=2 ; l0ORks=3 ; TnsnDirc=+1 ; hmuch=0.10d0 ; maxCnt=15
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=18 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1; ApBsLt=2 ; l0ORks=3 ; TnsnDirc=+1 ; hmuch=0.10d0 ; maxCnt=15
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=19 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell; ApBsLt=1 ; l0ORks=3 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=5
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          !writORread=1 ; manpltNum=20 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !CgXNode(2)  = 0.15d0 ; CgXNode(26) = -0.15d0
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=2 ; manpltNum=23 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !hmuchA0=1.25d0; diffRteOrNot=1
          !call incrRigidNessOfInitiatorCell(hmuchA0,diffRteOrNot,ExpNo,FrmNo)
          !writORread=1 ; manpltNum=24 ; call savePropFrmPrvManplt(writORread,manpltNum)
          
          !cellNm=Hlf_Ncell ; dirctn=-1
          !call apply_ThreeDiff_force_at_TopNode_of_LS_ofAcell_andEquill(cellNm,dirctn,ExpNo,FrmNo) 
          !write(*,*) "stopping at ManipltngEquilibrateForInitiatn.f08 f"
          !stop
          
       endif
       
    elseif (hlfCyclNo==3) then
       
       if (whichEnd==-1) then
          continue
          
       elseif (whichEnd==+1) then
          
          !cellNm=N_cell ; cellNmToCmpr=Hlf_Ncell
          !call incrPressAndCmprToACell(cellNm,cellNmToCmpr,ExpNo,FrmNo)
          !writORread=1 ; manpltNum=25 ; call savePropFrmPrvManplt(writORread,manpltNum)
          
          !A0(N_cell) = 1.85d0*A0(N_cell)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          !writORread=1 ; manpltNum=26 ; call savePropFrmPrvManplt(writORread,manpltNum)
          
          
          !cellNm=Hlf_Ncell-1;ApBsLt=3;l0ORks=1 ;TnsnDirc=+1;hmuch=0.05d0;limTnsn=0.00d0
          !call change_l0KsorBothOfAspr_TochangeTnsn(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     limTnsn,ExpNo,FrmNo)
          !writORread=2 ; manpltNum=27 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-1; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.05d0 ; maxCnt=6
          !call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     maxCnt,ExpNo,FrmNo)
          writORread=2 ; manpltNum=28 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          cellNm=N_cell ; cntInRtn=1 ; deallctnNeeds=0
          call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          
       endif
       
    elseif (hlfCyclNo==4) then

       if (whichEnd==-1) then
          writORread=2 ; manpltNum=28 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       elseif (whichEnd==+1) then
          
          !cellNm=N_cell ; cntInRtn=2 ; deallctnNeeds=0
          !call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=2 ; manpltNum=29 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       endif
       
    elseif (hlfCyclNo==5) then
       
       
       if (whichEnd==-1) then
          call very_stiff_areas(ExpNo,FrmNo)
          write(*,*) "Stopped at ManpltingEquilibrateForInitiatn, hlCycl=5,whichEnd=-1"
          stop
          continue
       elseif (whichEnd==+1) then
          
          !cellNm=N_cell ; cntInRtn=2 ; deallctnNeeds=0
          !call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=2 ; manpltNum=30 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !cellNm=Hlf_Ncell-2;ApBsLt=3;l0ORks=1 ;TnsnDirc=+1;hmuch=0.05d0;limTnsn=-0.05d0
          !call change_l0KsorBothOfAspr_TochangeTnsn(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
          !     limTnsn,ExpNo,FrmNo)
          writORread=2 ; manpltNum=31 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          
          cellNm=Hlf_Ncell; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=2
          call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
               maxCnt,ExpNo,FrmNo)
          writORread=1 ; manpltNum=32 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          cellNm=Hlf_Ncell-2; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=-1 ; hmuch=0.10d0 ; maxCnt=7
          call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
               maxCnt,ExpNo,FrmNo)
          writORread=1 ; manpltNum=33 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          !stop
          
       endif
       
    elseif (hlfCyclNo==6) then
       
       if (whichEnd==-1) then
          writORread=2 ; manpltNum=33 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       elseif (whichEnd==+1) then
          
          cellNm=N_cell ; cntInRtn=2 ; deallctnNeeds=0
          call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=1 ; manpltNum=34 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       endif
       
    elseif (hlfCyclNo==7) then
       
       if (whichEnd==-1) then
          continue
       elseif (whichEnd==+1) then
          
          cellNm=N_cell ; cntInRtn=2 ; deallctnNeeds=0
          call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=1 ; manpltNum=35 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       endif
          
    elseif (hlfCyclNo==8) then
       
       if (whichEnd==-1) then
          writORread=2 ; manpltNum=35 ; call savePropFrmPrvManplt(writORread,manpltNum)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       elseif (whichEnd==+1) then
          
          cellNm=N_cell ; cntInRtn=2 ; deallctnNeeds=0
          call propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
          call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          writORread=1 ; manpltNum=36 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
          cellNm=Hlf_Ncell-1; ApBsLt=1 ; l0ORks=1 ; TnsnDirc=+1 ; hmuch=0.10d0 ; maxCnt=2
          call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
               maxCnt,ExpNo,FrmNo)
          writORread=1 ; manpltNum=37 ; call savePropFrmPrvManplt(writORread,manpltNum)
          !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
          
       endif
          
    endif
    
  end subroutine combine_diffrnt_manpltn_and_variabls
  
  
  
  
  subroutine get_variabls_for_diff_hlfCycl(hlfCyclNo)
    implicit none
    integer, intent(in) :: hlfCyclNo
    integer :: i,j
    integer :: N_itm
    integer :: whichSwitch
    
    if (hlfCyclNo == 1) then
       
       N_manpltdAngls = 9
       
       allocate(cellNmbrs(1:N_manpltdAngls))
       allocate(crnrVal(1:N_manpltdAngls))
       allocate(criticalAngls(1:N_manpltdAngls))
       allocate(cmprDirctn(1:N_manpltdAngls))
       allocate(crnrAngls(1:N_manpltdAngls))
       allocate(switchingXcoor(1:N_manpltdAngls),bfrSwitchXcoor(1:N_manpltdAngls))

       
       do i = 1,N_manpltdAngls
          cellNmbrs(i) = i
       enddo
       
       crnrVal(1:N_manpltdAngls)       = 3   !set as 3 as we need one cornr angle to compre
       criticalAngls(1:N_manpltdAngls) = 90.00d0
       cmprDirctn(1:N_manpltdAngls)    = +1       ! Either can be +1 or -1
       crnrAngls(1:N_manpltdAngls)     = +1000.0d0

       whichSwitch=1 ; call get_SwitchXcoorVals(whichSwitch)
       
       open(unit=214,file='get_variables_for_diff_hlfCycl.dat',position='append')
       
       N_itm = 6
       
       do i = 1,N_itm
          
          do j = 1,N_manpltdAngls
             
             if (i==1) write(214,*) cellNmbrs(j),j,"cellNmbrs"
             if (i==2) write(214,*) crnrVal(j),j,"crnrVals"
             if (i==3) write(214,*) criticalAngls(j),j,"criticalAngls"
             if (i==4) write(214,*) cmprDirctn(j),j,"cmprDirctns"
             if (i==5) write(214,*) crnrAngls(j),j,"crnrAngls"
             if (i==6) write(214,*) bfrSwitchXcoor(j),j,"bfrSwitch"
             
          enddo
          
          write(214,*) " "
       enddo
       
       close(214)
       
    elseif (hlfCyclNo == 2) then
       
       continue
    elseif (hlfCyclNo == 3) then
       
       continue
    endif
    
  end subroutine get_variabls_for_diff_hlfCycl
  
  
  
  
  subroutine incrRigidNessOfInitiatorCell(hmuchA0,diffRteOrNot,ExpNo,FrmNo)
    implicit none
    real*8 , intent(in)    :: hmuchA0
    integer, intent(in)    :: diffRteOrNot
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: ICB !ICB=InitiatorCellBottom
    real*8  :: areaBfrChng,areaAftChng
    real*8  :: prcntChngArea,tolPrcnt
    real*8  :: hmuchl0,hmuchks,hmuchl0_incr,hmuchks_incr
    
    integer :: sprsInICB,sprNm
    integer :: i,j
    integer :: cnt
    
    ICB         = N_cell
    areaBfrChng = A(ICB) !area here is ACTUALLY NI area
    
    A0(ICB) = (hmuchA0)*(A0(ICB))
    
    call Equilibrate_system
    call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
    FrmNo = FrmNo+1
    call switchto_NI_model_run_and_switchbackto_TN
    
    hmuchl0 = 1.0d0-(abs(hmuchA0-1.0d0)/4.0d0)
    hmuchks = 1.0d0+(abs(hmuchA0-1.0d0)/4.0d0)

    hmuchl0_incr = 1.0d0-(3.0d0*abs(hmuchA0-1.0d0)/4.0d0)
    hmuchks_incr = 1.0d0+(3.0d0*abs(hmuchA0-1.0d0)/4.0d0)

    write(*,*) hmuchl0,hmuchks,"hmuchl0-hmuchks"
    
    if (hmuchl0.gt.1.0d0) then
       write(*,*) 'hmuchl0 cant be greater than 1.0d0'
       stop
    endif
    
    tolPrcnt = 1.0d0
    cnt = 1
    
    do
       sprsInICB = area_spr(ICB,0)
       
       do i = 1,sprsInICB
          sprNm = area_spr(ICB,i)
          
          if (diffRteOrNot==1) then
             if (i.ne.(sprsInICB-1)) then
                l0(sprNm)    = (hmuchl0)*l0(sprNm)
                k_spr(sprNm) = (hmuchks)*k_spr(sprNm)
             elseif (i==(sprsInICB-1)) then
                l0(sprNm)    = (hmuchl0_incr)*l0(sprNm)
                k_spr(sprNm) = (hmuchks_incr)*k_spr(sprNm)
             endif
          elseif (diffRteOrNot==0) then
             l0(sprNm)    = (hmuchl0)*l0(sprNm)
             k_spr(sprNm) = (hmuchks)*k_spr(sprNm)
          endif
          
          
       enddo
       
       call Equilibrate_system
       call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
       FrmNo = FrmNo+1
       call switchto_NI_model_run_and_switchbackto_TN
       areaAftChng = A(ICB)
       
       prcntChngArea=(abs(areaAftChng-areaBfrChng)/(areaBfrChng))*(100.0d0)
       if (prcntChngArea.le.tolPrcnt) then
          write(*,*) "areaGOTreduced and count =",cnt
          exit
       endif
       
       cnt=cnt+1
    enddo
    
  end subroutine incrRigidNessOfInitiatorCell

  
  subroutine incrPressAndCmprToACell(cellNm,cellNmToCmpr,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cellNm,cellNmToCmpr,ExpNo
    integer, intent(inout) :: FrmNo
    
    real*8  :: hmuchA0
    integer :: diffRteOrNot,countLp
    integer :: extOrNot
    real*8  :: PressVal(1:N_cell)
    real*8  :: areaBfrLp
    integer :: sgmntdOrNot,whrTo
    interface
       function Pressure(dum_Nodes,dumA0)
         use system_parameters
         use transfrm_info
         implicit none
         real*8  :: Pressure(1:N_cell)
         real*8  :: dum_Nodes(1:N_node,1:N_dmnsn)
         real*8  :: dumA0(1:N_cell)
       end function Pressure
    end interface
    
    if (cellNm==N_cell) then
       
       countLp = 1
       areaBfrLp = A(cellNm)
       write(*,*) areaBfrLp,"areaBfrLp"

       
       
       do
          
          hmuchA0 = 1.06d0 ; diffRteOrNot=0
          call incrRigidNessOfInitiatorCell(hmuchA0,diffRteOrNot,ExpNo,FrmNo)
          
          sgmntdOrNot=1 ; whrTo=1
          call switchto_NI_model_run_switchbackto_TN_seprtlyOrtogthrNOFlnm(sgmntdOrNot,whrTo)
          PressVal(1:N_cell) = Pressure(node_xy,A0)
          if (PressVal(cellNm).gt.PressVal(cellNmToCmpr)) then
             write(*,*) "countLp is =",countLp
             exit
          endif
          
          write(*,*) PressVal(N_cell),PressVal(Hlf_Ncell),"Press of N_cell,Hlf_Ncell"
          write(*,*) A(N_cell),areaBfrLp,"area of N_cell after-before"
          
          sgmntdOrNot=1 ; whrTo=2
          call switchto_NI_model_run_switchbackto_TN_seprtlyOrtogthrNOFlnm(sgmntdOrNot,whrTo)
          countLp=countLp+1
       enddo
       
       write(*,*) "FrmNo and countLp =",(FrmNo-1),countLp
       
    endif
    
  end subroutine incrPressAndCmprToACell
  
  
  subroutine change_l0KsorBothOfAspr_TochangeTnsn(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
       limTnsn,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cellNm,ApBsLt,l0ORks
    integer, intent(in)    :: TnsnDirc
    real*8 , intent(in)    :: hmuch,limTnsn
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: maxCnt,LpCntExt,sprNm ! will not be needed, just for calling purpose
    real*8  :: TnsnVal(1:N_spr)
    
    interface
       
       function TnsnComprsn(dum_Nodes,duml0)
         use system_parameters
         use transfrm_info
         implicit none
         real*8 :: TnsnComprsn(1:N_spr)
         real*8 :: dum_Nodes(1:N_node,1:N_dmnsn)
         real*8 :: duml0(1:N_spr)
       end function TnsnComprsn
       
    end interface
    
    maxCnt   = 1
    LpCntExt = 0
    
    do
       
       call change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
            maxCnt,ExpNo,FrmNo)
       TnsnVal(1:N_spr) = TnsnComprsn(node_xy,l0)

       if (ApBsLt==1) sprNm=(cellNm-1)*3+1
       if (ApBsLt==2) sprNm=(cellNm-1)*3+2
       if (ApBsLt==3) sprNm=(cellNm-1)*3+3
       
       write(*,*) TnsnVal(sprNm),limTnsn,TnsnDirc,"TnsnDirc"
       
       if (TnsnDirc==+1) then
          
          if (TnsnVal(sprNm) .le. limTnsn) then
             if (LpCntExt==1) then
                exit
             else
                LpCntExt = LpCntExt+1
             endif
          endif
          
       elseif (TnsnDirc==-1) then
          
          if (TnsnVal(sprNm) .gt. limTnsn) then
             if (LpCntExt==1) then ! just to Equilibrate 1 more time after matching condition
                exit
             else
                LpCntExt = LpCntExt+1
             endif
          endif
       endif
       
    enddo

    write(*,*) TnsnVal(sprNm),limTnsn,"TnsnS"
    call sleep(1)
    
  end subroutine change_l0KsorBothOfAspr_TochangeTnsn
  
  subroutine manplt_angls_apcl_btwn_ltrl(ExpNo,FrmNo,lgcl_AnglManplt)
    implicit none
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    logical, intent(out)   :: lgcl_AnglManplt
    
    integer :: area_nmL,crnr_nmL,node_nmL,cmpr_val
    integer :: cnt_AnglChng
    real*8  :: Phi=-1000.0d0
    integer :: i,j
    integer :: switchV,choiceSpr
    
    lgcl_AnglManplt = .False.
    cnt_AnglChng    = 0
    
    open(unit=215,file='manplt_angls_apcl_btwn_ltrl.dat',position='append')
    
    do i = 1,N_manpltdAngls
       
       area_nmL = cellNmbrs(i)
       crnr_nmL = crnrVal(i)
       node_nmL = (area_nmL-1)*2 + crnr_nmL 
       cmpr_val = cmprDirctn(i)
       
       call get_Angle_AtANode(node_nmL,area_nmL,Phi) ; CrnrAngls(i) = Phi
       call get_SingleSwitchXcoorVals(i,switchV) ; switchingXcoor(i) = switchV
       
       write(215,*) area_nmL,crnr_nmL,node_nmL,switchingXcoor(i),bfrSwitchXcoor(i),i,"var values"
       
       
       if (switchingXcoor(i) .ne. bfrSwitchXcoor(i)) then
          cnt_AnglChng = cnt_AnglChng+1   
          
       elseif (switchingXcoor(i) == bfrSwitchXcoor(i)) then
          choiceSpr = 0 ! choice=0 as we wanna alter both apical and basal
          call manplt_apclbsalprop_forAngls(area_nmL,cmpr_val,choiceSpr)
       endif
       
    enddo
    
    write(215,*) cnt_AnglChng,"cnt_AnglChng"
    
    if (cnt_AnglChng == (N_manpltdAngls-7)) then
       write(*,*) "Angle manipulation is done"
       lgcl_AnglManplt = .True.
    elseif (cnt_AnglChng .ne. N_manpltdAngls) then
       continue
    endif
    
    call Equilibrate_system
    call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
    FrmNo = FrmNo+1
    call switchto_NI_model_run_and_switchbackto_TN
    
    close(215)
    !stop
    
  end subroutine manplt_angls_apcl_btwn_ltrl
  
  
  subroutine manplt_apclbsalprop_forAngls(areaNmL,cmprVal,choice)
    implicit none
    integer, intent(in)  :: areaNmL,cmprVal
    integer, intent(in)  :: choice
    
    integer :: i,j,jmax
    integer :: nsprsInACell
    integer :: nApclSpr,nBsalSpr,nLtrlSpr
    integer :: sprNmL,sprNmR
    real*8  :: hmuchA,hmuchB
    
    real*8  :: l0BfrL,l0BfrR,l0AftL,l0AftR
    
    integer, allocatable :: apclSprL(:),bsalSprL(:),ltrlSprL(:)
    integer, allocatable :: apclSprR(:),bsalSprR(:),ltrlSprR(:)
    
    open(unit=216,file='manplt_apclbsalprop_forAngls.dat',position='append')
    
    if (modelID==1) then
       nApclSpr = 1 ; nBsalSpr=1 ; nLtrlSpr=1
       nsprsInACell = (napclSpr+nbsalSpr+nltrlSpr)
       
    elseif (modelID==2) then
       nApclSpr=NAEC_Apcl+1 ; nBsalSpr=NAEC_Bsal+1 ; nLtrlSpr=NAEC_Ltrl+1
       nsprsInACell = ((NAEC_Apcl+1) + (NAEC_Bsal+1) + (NAEC_Ltrl+1)) 
    endif
    
    if (cmprVal == +1) then
       hmuchA = -0.05d0
       hmuchB = +0.05d0
    elseif (cmprVal == -1) then
       hmuchA = +0.05d0
       hmuchB = -0.05d0
    endif
    
    allocate(apclSprL(1:napclSpr),bsalSprL(1:nbsalSpr),ltrlSprL(1:nltrlSpr))
    allocate(apclSprR(1:napclSpr),bsalSprR(1:nbsalSpr),ltrlSprR(1:nltrlSpr))
    
    
    do i = 1,2
       
       if (i==1) jmax = nApclSpr
       if (i==2) jmax = nBsalSpr
       
       do j = 1,jmax
          
          if (i==1) then
             
             apclSprL(j) = (areaNmL-1)*(nsprsInACell) + j
             apclSprR(j) = apclSprL(j) + (Hlf_Ncell*nsprsInACell)
             
             sprNmL = apclSprL(j) ; sprNmR = apclSprR(j)
             
             l0BfrL = l0(sprNmL) ; l0BfrR = l0(sprNmR)
             
             if (choice==0 .or. choice==1) then ! choice=0 for both, choice=1 for Apical only
                l0(sprNmL) = (1.0d0-hmuchA) * l0(sprNmL)
                l0(sprNmR) = l0(sprNmL)
             endif
             
             l0AftL = l0(sprNmL) ; l0AftR = l0(sprNmR)
             
             write(216,*) l0BfrL,l0AftL,(l0AftL/l0BfrL),sprNmL,"l0chng ApclL"
             write(216,*) l0BfrR,l0AftR,(l0AftR/l0BfrR),sprNmR,"l0chng ApclR"
             
          elseif (i==2) then
             
             bsalSprL(j) = (areaNmL-1)*(nsprsInACell) + nApclSpr + j
             bsalSprR(j) = bsalSprL(j) + (Hlf_Ncell*nsprsInACell)
             
             sprNmL = bsalSprL(j) ; sprNmR = bsalSprR(j)
             
             l0BfrL = l0(sprNmL) ; l0BfrR = l0(sprNmR)
             
             if (choice==0 .or. choice==2) then ! choice=0 for both, choice=2 for Basal only
                l0(sprNmL) = (1.0d0-hmuchB) * l0(sprNmL)
                l0(sprNmR) = l0(sprNmL)
             endif
             
             l0AftL = l0(sprNmL) ; l0AftR = l0(sprNmR)
             
             write(216,*) l0BfrL,l0AftL,(l0AftL/l0BfrL),sprNmL,"l0chng BsalL"
             write(216,*) l0BfrR,l0AftR,(l0AftR/l0BfrR),sprNmR,"l0chng BsalR"
             
          endif
          
       enddo
       
       write(216,*) " "
       
    enddo
    
    write(216,*) "End OF the ROUTINE"
    
    close(216)
    
  end subroutine manplt_apclbsalprop_forAngls
  
  
  subroutine get_SwitchXcoorVals(whichOne)
    implicit none
    integer, intent(in) :: whichOne
    
    integer :: i,j
    integer :: nodeAtTop,nodeAtBot
    real*8  :: xValAtTop,xvalAtBot

    open(unit=217,file='get_SwitchXcoor.dat',position='append')
    
    do i = 1,N_manpltdAngls
       
       nodeAtTop = crnrVal(i) + (i-1)*2
       nodeAtBot = nodeAtTop+1
       xvalAtTop = abs(node_xy(nodeAtTop,1))
       xvalAtBot = abs(node_xy(nodeAtBot,1))
       
       write(217,*) nodeAtTop,nodeAtBot,xvalAtTop,xvalAtBot,i,"node-x"
       
       if (whichOne==1) then
          
          if (xvalAtTop .gt. xvalAtBot) bfrSwitchXcoor(i) = +1
          if (xvalAtTop .le. xvalAtBot) bfrSwitchXcoor(i) = -1
          
          write(217,*) bfrSwitchXcoor(i),i,"bfrSwitch"
          
       elseif (whichOne==2) then
          
          if (xvalAtTop .gt. xvalAtBot) switchingXcoor(i) = +1 
          if (xvalAtTop .le. xvalAtBot) switchingXcoor(i) = -1
          
          write(217,*) switchingXcoor(i),i,"SwitchXcoor"
          
       endif
       
    enddo
    
    close(217)
    
  end subroutine get_SwitchXcoorVals
  
  subroutine get_SingleSwitchXcoorVals(seq,switchV)
    implicit none
    integer, intent(in)  :: seq
    integer, intent(out) :: switchV
    
    integer :: nodeAtTop,nodeAtBot
    real*8  :: xValAtTop,xvalAtBot

    open(unit=218,file='SingleSwitch1.dat',position='append')
    open(unit=219,file='SingleSwitch2.dat',position='append')
    
    nodeAtTop = crnrVal(seq) + (seq-1)*2
    nodeAtBot = nodeAtTop+1
    xvalAtTop = abs(node_xy(nodeAtTop,1))
    xvalAtBot = abs(node_xy(nodeAtBot,1))
    
    write(218,*) nodeAtTop,nodeAtBot,xvalAtTop,xvalAtBot,i,"node-x"
    
    if (xvalAtTop .gt. xvalAtBot) switchV = +1
    if (xvalAtTop .le. xvalAtBot) switchV = -1
    
    write(219,*) seq,switchV,"seq-switchV"
    
    close(218)
    close(219)
    
  end subroutine get_SingleSwitchXcoorVals
  
  
  subroutine apcl_bsal_l0_manplt_of_cells_make_systm_strght(ExpNo,FrmNo,CC,lgcl_strght)
    implicit none
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    integer, intent(in)    :: CC    ! CC=Choice of Cells
    logical, intent(out)   :: lgcl_strght
    
    integer, allocatable :: cellNm(:)
    integer, allocatable :: l0_manpltDirctn(:)
    integer, allocatable :: choicSpr(:)
    
    integer :: choicSprV
    integer :: NcellMan
    integer :: cellNmV,l0_manpltDirctnV,choiceSprV
    
    if (CC==1) then
       
       NcellMan=2
       allocate(cellNm(1:NcellMan),l0_manpltDirctn(1:NcellMan),choicSpr(1:NcellMan))
       
       cellNm(1)                   = hlf_Ncell-1
       cellNm(2)                   = hlf_Ncell
       l0_manpltDirctn(1:NcellMan) = -1 ! (-1) --- Apcl contrct + Bsal expnsn
       choicSpr(1:NcellMan)        = 0  ! both apical and basal manplt
       
    elseif (CC==2) then
       
       NcellMan  = 1
       allocate(cellNm(1:NcellMan),l0_manpltDirctn(1:NcellMan),choicSpr(1:NcellMan))
       
       cellNm(1)                   = hlf_Ncell-2
       l0_manpltDirctn(1:NcellMan) = +1 ! (+1) --- Apcl expnsn + Bsal contrct
       choicSpr(1:NcellMan)        =  1 ! only apical   
       
    endif
    
    do
       
       do i = 1,NcellMan
          cellNmV          = cellNm(i)
          l0_manpltDirctnV = l0_manpltDirctn(i)
          choicSprV        = choicSpr(i) 
          call manplt_apclbsalprop_forAngls(cellNmV,l0_manpltDirctnV,choiceSprV)
       enddo
       
       call Equilibrate_system
       call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
       FrmNo = FrmNo+1
       call switchto_NI_model_run_and_switchbackto_TN
       
       call get_the_strght_lgcl(lgcl_strght)
       if (lgcl_strght .eqv. .True.) exit
    enddo
    
  end subroutine apcl_bsal_l0_manplt_of_cells_make_systm_strght
  
  
  subroutine get_the_strght_lgcl(lgcl_strght)
    implicit none
    logical, intent(out) :: lgcl_strght
    integer :: cellNmV
    integer :: first_TN,secnd_TN,third_TN,furth_TN
    real*8  :: tol_len,curr_dfrmtn
    
    lgcl_strght = .False. !strgtness is measured based on cell1
    
    cellNmV   = 1
    first_TN  = 2*(cellNmV-1)+1
    secnd_TN  = 2*(cellNmV-1)+2
    third_TN  = 2*(cellNmV-1)+3
    furth_TN  = 2*(cellNmV-1)+4
    
    tol_len     = 0.05d0*(node_xy(third_TN,1)-node_xy(first_TN,1))
    curr_dfrmtn = abs(node_xy(secnd_TN,1)-node_xy(first_TN,1))
    
    if (curr_dfrmtn .le. tol_len) lgcl_strght=.True.
    
  end subroutine get_the_strght_lgcl
  
  
  subroutine change_l0ksSpr_gvnCellNmAndEquill(cellNm,ApBsLt,TnsnDirc,maxCnt,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cellNm,ApBsLt,TnsnDirc,ExpNo
    integer, intent(inout) :: FrmNo 
    integer, intent(in)    :: maxCnt
    
    integer :: i,imax,countLp
    integer :: nsprInAp,nsprInBs,nsprInLt
    integer :: nsprsInACell,sprNmL,sprNmR
    real*8  :: hmuchL0orKS
    
    integer, allocatable :: apclSpr(:),bsalSpr(:),ltrlSpr(:)
    integer, allocatable :: sprsInTheSide(:)
    
    if (modelID==1) then
       nsprInAp=1 ; nsprInBs=1 ; nsprInLt=1 
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
       
    elseif (modelID==2) then
       nsprInAp=(NAEC_Apcl+1) ; nsprInBs=(NAEC_Bsal+1) ; nsprInLt=(NAEC_Ltrl+1)
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
    endif
    
    allocate(apclSpr(1:nsprInAp),bsalSpr(1:nsprInBs),ltrlSpr(1:nsprInLt))
    apclSpr=0 ; bsalSpr=0 ; ltrlSpr=0
    
    if (ApBsLt==1) imax=nsprInAp
    if (ApBsLt==2) imax=nsprInBs
    if (ApBsLt==3) imax=nsprInLt
    
    allocate(sprsInTheSide(1:imax))
    
    do i = 1,imax
       if (ApBsLt==1) apclSpr(i) = (cellNm-1)*(nsprsInACell)+i
       if (ApBsLt==2) bsalSpr(i) = (cellNm-1)*(nsprsInACell)+(nsprInAp)+i
       if (ApBsLt==3) ltrlSpr(i) = (cellNm-1)*(nsprsInACell)+(nsprInAp)+(nsprInBs)+i
    enddo
    
    if (ApBsLt==1) sprsInTheSide = apclSpr
    if (ApBsLt==2) sprsInTheSide = bsalSpr
    if (ApBsLt==3) sprsInTheSide = ltrlSpr
    
    if (TnsnDirc==+1) hmuchL0orKS = +0.05d0
    if (TnsnDirc==-1) hmuchL0orKS = -0.05d0
    
    countLp = 0
    
    do
       
       do i = 1,imax
          sprNmL        = sprsInTheSide(i)       ; sprNmR = sprNmL+(Hlf_Ncell)*(nsprsInACell)
          l0(sprNmL)    = (1.0d0-hmuchL0orKS)*l0(sprNmL)    ; l0(sprNmR)    = l0(sprNmL)
          k_spr(sprNmL) = (1.0d0+hmuchL0orKS)*k_spr(sprNmL) ; k_spr(sprNmR) = k_spr(sprNmL)
          write(*,*) sprNmL,sprNmR,i,"sprNmL-R"
       enddo
       
       call Equilibrate_system
       call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
       FrmNo = FrmNo+1
       call switchto_NI_model_run_and_switchbackto_TN
       countLp = countLp+1

       if (countLp==maxCnt) then
          write(*,*) "FrmNo bfr exiting in change_l0ksSpr routine",(FrmNo-1)
          exit
       endif
       
    enddo
    
  end subroutine change_l0ksSpr_gvnCellNmAndEquill
  
  !subroutine   
  
  
  subroutine change_l0_or_ks_gvnCellNmAndEquill(cellNm,ApBsLt,l0ORks,TnsnDirc,hmuch,&
       maxCnt,ExpNo,FrmNo)
    
    implicit none
    integer, intent(in)    :: cellNm,ApBsLt,l0ORks,TnsnDirc
    real*8 , intent(in)    :: hmuch
    integer, intent(in)    :: maxCnt,ExpNo
    integer, intent(inout) :: FrmNo 
    
    integer :: i,imax,countLp
    integer :: nsprInAp,nsprInBs,nsprInLt
    integer :: nsprsInACell,sprNmL,sprNmR
    real*8  :: hmuchL0orKS
    
    integer, allocatable :: apclSpr(:),bsalSpr(:),ltrlSpr(:)
    integer, allocatable :: sprsInTheSide(:)
    
    if (modelID==1) then
       nsprInAp=1 ; nsprInBs=1 ; nsprInLt=1 
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
       
    elseif (modelID==2) then
       nsprInAp=(NAEC_Apcl+1) ; nsprInBs=(NAEC_Bsal+1) ; nsprInLt=(NAEC_Ltrl+1)
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
    endif
    
    allocate(apclSpr(1:nsprInAp),bsalSpr(1:nsprInBs),ltrlSpr(1:nsprInLt))
    apclSpr=0 ; bsalSpr=0 ; ltrlSpr=0
    
    if (ApBsLt==1) imax=nsprInAp
    if (ApBsLt==2) imax=nsprInBs
    if (ApBsLt==3) imax=nsprInLt
    
    allocate(sprsInTheSide(1:imax))
    
    do i = 1,imax
       if (ApBsLt==1) apclSpr(i) = (cellNm-1)*(nsprsInACell)+i
       if (ApBsLt==2) bsalSpr(i) = (cellNm-1)*(nsprsInACell)+(nsprInAp)+i
       if (ApBsLt==3) ltrlSpr(i) = (cellNm-1)*(nsprsInACell)+(nsprInAp)+(nsprInBs)+i
    enddo
    
    if (ApBsLt==1) sprsInTheSide = apclSpr
    if (ApBsLt==2) sprsInTheSide = bsalSpr
    if (ApBsLt==3) sprsInTheSide = ltrlSpr
    
    if (TnsnDirc==+1) hmuchL0orKS = +abs(hmuch)
    if (TnsnDirc==-1) hmuchL0orKS = -abs(hmuch)
    
    countLp = 0
    
    do
       
       do i = 1,imax
          
          sprNmL        = sprsInTheSide(i)
          sprNmR = sprNmL+(Hlf_Ncell)*(nsprsInACell)
          
          if (l0ORks==1) then
             l0(sprNmL)    = (1.0d0-hmuchL0orKS)*l0(sprNmL)
             l0(sprNmR)    = l0(sprNmL)
             
          elseif (l0ORks==2) then
             k_spr(sprNmL) = (1.0d0+hmuchL0orKS)*k_spr(sprNmL)
             k_spr(sprNmR) = k_spr(sprNmL)
             
          elseif (l0ORks==3) then
             l0(sprNmL)    = (1.0d0-hmuchL0orKS)*l0(sprNmL)
             l0(sprNmR)    = l0(sprNmL)
             k_spr(sprNmL) = (1.0d0+hmuchL0orKS)*k_spr(sprNmL)
             k_spr(sprNmR) = k_spr(sprNmL)
          endif
          
          write(*,*) sprNmL,sprNmR,i,"sprNmL-R"
          
       enddo
       
       call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
       countLp = countLp+1
       
       if (countLp==maxCnt) then
          write(*,*) "FrmNo bfr exiting in change_l0ksSpr routine",(FrmNo-1)
          exit
       endif
       
    enddo
    
  end subroutine change_l0_or_ks_gvnCellNmAndEquill
  
  subroutine initiatr_cell_bsal_l0ksIncr(TnsnDirc,maxCnt,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: TnsnDirc,maxCnt,ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: i,imax,countLp
    integer :: nsprInAp,nsprInBs,nsprInLt
    integer :: nsprsInACell,sprNm
    real*8  :: hmuchL0orKS
    
    
    if (modelID==1) then
       nsprInAp=1 ; nsprInBs=1 ; nsprInLt=1 
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
       
    elseif (modelID==2) then
       nsprInAp=(NAEC_Apcl+1) ; nsprInBs=(NAEC_Bsal+1) ; nsprInLt=(NAEC_Ltrl+1)
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
    endif
    
    if (TnsnDirc==+1) hmuchL0orKS = +0.05d0
    if (TnsnDirc==-1) hmuchL0orKS = -0.05d0
    
    countLp = 0
    imax    = nsprInBs
    
    do
       
       do i = 1,imax
          sprNm        = (N_cell-1)*(nsprsInACell) + i
          l0(sprNm)    = (1.0d0-hmuchL0orKS)*l0(sprNm)
          k_spr(sprNm) = (1.0d0+(5.0*hmuchL0orKS))*k_spr(sprNm)
          write(*,*) sprNm,i,"sprNm"
       enddo
       
       call Equilibrate_system
       call save_config_and_generate_ColorData(coordntes_xy,l0,A0,ExpNo,FrmNo)
       FrmNo = FrmNo+1
       call switchto_NI_model_run_and_switchbackto_TN
       countLp = countLp+1
       
       if (countLp==maxCnt) then
          write(*,*) "FrmNo bfr exiting in change_l0ksSpr routine",(FrmNo-1)
          exit
       endif
       
    enddo
    
  end subroutine initiatr_cell_bsal_l0ksIncr
  
  
  
  subroutine initiatr_cell_TwoLtrl_l0ksIncr(TnsnDirc,maxCnt,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: TnsnDirc,maxCnt,ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: i,imax,countLp
    integer :: nsprInAp,nsprInBs,nsprInLt
    integer :: nsprsInACell,sprNmL,sprNmR
    real*8  :: hmuchL0orKS
    
    
    if (modelID==1) then
       nsprInAp=1 ; nsprInBs=1 ; nsprInLt=1 
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
       
    elseif (modelID==2) then
       nsprInAp=(NAEC_Apcl+1) ; nsprInBs=(NAEC_Bsal+1) ; nsprInLt=(NAEC_Ltrl+1)
       nsprsInACell = nsprInAp+nsprInBs+nsprInLt
    endif
    
    if (TnsnDirc==+1) hmuchL0orKS = +0.05d0
    if (TnsnDirc==-1) hmuchL0orKS = -0.05d0
    
    countLp = 0
    imax    = nsprInBs
    
    do
       
       do i = 1,imax
          sprNmL   = (Hlf_Ncell*nsprsInACell)-(nsprInLt)+i; sprNmR = sprNmL+(Hlf_Ncell*nsprsInACell)
          l0(sprNmL)    = (1.0d0-hmuchL0orKS)*l0(sprNmL)    ; l0(sprNmR)    = l0(sprNmL)
          k_spr(sprNmL) = (1.0d0+hmuchL0orKS)*k_spr(sprNmL) ; k_spr(sprNmR) = k_spr(sprNmL)
          write(*,*) sprNmL,sprNmR,i,"sprNm"
       enddo
       
       call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
       countLp = countLp+1
       
       if (countLp==maxCnt) then
          write(*,*) "FrmNo bfr exiting in change_l0ksSpr routine",(FrmNo-1)
          exit
       endif
       
    enddo
    
  end subroutine initiatr_cell_TwoLtrl_l0ksIncr
  
  subroutine making_InitiatnPhase_IncmingCellsIdntcl(upToWhchCell,idealCell,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: upToWhchCell
    integer, intent(in)    :: idealCell
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: nsprsInACell
    integer :: apclSpr,bsalSpr,ltrlSpr
    
    real*8  :: ks_apcl,ks_bsal,ks_ltrl
    real*8  :: l0_apcl,l0_bsal,l0_ltrl
    real*8  :: ka_apcl,ka_bsal,ka_ltrl
    real*8  :: A0_apcl,A0_bsal,A0_ltrl
    real*8  :: ka_ideal,A0_ideal
    
    integer :: cellNmL,cellNmR,sprNmL,sprNmR
    integer :: i,j,jmax
    
    if (modelID==1) continue
    if (modelID==2) then
       write(*,*) "stop in making_InitiatnPhase_IncmingCells, not for modelID==2"
       stop
    endif
    
    nsprsInACell = 3
    
    ka_ideal = k_area(idealCell)
    A0_ideal = A0(idealCell)
    
    apclSpr = (idealCell-1)*(nsprsInACell)+1 ; bsalSpr = apclSpr+1 ; ltrlSpr = apclSpr+2
    
    ks_apcl = k_spr(apclSpr)  ; ks_bsal = k_spr(bsalSpr)  ; ks_ltrl = k_spr(ltrlSpr)
    l0_apcl = l0(apclSpr)     ; l0_bsal = l0(bsalSpr)     ; l0_ltrl = l0(ltrlSpr)
    ka_apcl = k_area(apclSpr) ; ka_bsal = k_area(bsalSpr) ; ka_ltrl = k_area(ltrlSpr)
    A0_apcl = A0(apclSpr)     ; A0_bsal = A0(bsalSpr)     ; A0_ltrl = A0(ltrlSpr)
    
    do i = 1,upToWhchCell
       
       cellNmL = i ; cellNmR = i+Hlf_Ncell
       
       k_area(cellNmL) = ka_ideal ; k_area(cellNmR) = k_area(cellNmL)
       A0(cellNmL)     = A0_ideal ; A0(cellNmR)     = A0(cellNmL)
       
       write(*,*) k_area(i),A0(i),i,"ka-A0-cell"
       write(*,*) " "
       
       do j = 1,nsprsInACell
          sprNmL = (cellNmL-1)*(nsprsInACell) + j
          sprNmR = sprNmL + (Hlf_Ncell*nsprsInACell)
          
          if (j==1) then
             k_spr(sprNmL) = ks_apcl ; l0(sprNmL) = l0_apcl
             k_spr(sprNmR) = ks_apcl ; l0(sprNmR) = l0_apcl
             
          elseif (j==2) then
             k_spr(sprNmL) = ks_bsal ; l0(sprNmL) = l0_bsal
             k_spr(sprNmR) = ks_bsal ; l0(sprNmR) = l0_bsal
             
          elseif (j==3) then
             k_spr(sprNmL) = ks_ltrl ; l0(sprNmL) = l0_ltrl
             k_spr(sprNmR) = ks_ltrl ; l0(sprNmR) = l0_ltrl
          endif
          
          write(*,*) k_spr(sprNmL),l0(sprNmL),sprNmL,"ks-l0-sprNmL"
          write(*,*) k_spr(sprNmR),l0(sprNmR),sprNmR,"ks-l0-sprNmR"
       enddo

       write(*,*) " "
    enddo
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
    
  end subroutine making_InitiatnPhase_IncmingCellsIdntcl


  subroutine copy_prp_frm_sprATosprB(cell_A,ApBsLt_A,cell_B,ApBsLt_B,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cell_A,ApBsLt_A
    integer, intent(in)    :: cell_B,ApBsLt_B
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: i,j
    integer :: spr_A,spr_B
    integer :: spr_AL,spr_BL,spr_AR,spr_BR
    integer :: nsprsInACell

    if (modelID==1) continue
    if (modelID==2) then
       write(*,*) "stopped as modelID==2"
       stop
    endif
    
    nsprsInACell = 3
    
    if (ApBsLt_A==1) spr_A = (cell_A-1)*(nsprsInACell) + 1
    if (ApBsLt_A==2) spr_A = (cell_A-1)*(nsprsInACell) + 2
    if (ApBsLt_A==3) spr_A = (cell_A-1)*(nsprsInACell) + 3
    
    if (ApBsLt_B==1) spr_B = (cell_B-1)*(nsprsInACell) + 1
    if (ApBsLt_B==2) spr_B = (cell_B-1)*(nsprsInACell) + 2
    if (ApBsLt_B==3) spr_B = (cell_B-1)*(nsprsInACell) + 3

    spr_BL = spr_B ; spr_BR = spr_BL+(Hlf_Ncell*nsprsInACell)
    spr_AL = spr_A ; spr_AR = spr_AL+(Hlf_Ncell*nsprsInACell)
    
    k_spr(spr_BL) = k_spr(spr_AL) ; k_spr(spr_BR) = k_spr(spr_AR)
    l0(spr_BL)    = l0(spr_AL)    ; l0(spr_BR)    = l0(spr_AR)
    
    write(*,*) spr_AL,spr_BL,"spr_AL-spr_BL"
    write(*,*) spr_AR,spr_BR,"spr_AR-spr_BR"
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
  end subroutine copy_prp_frm_sprATosprB
  
  subroutine savePropFrmPrvManplt(writORread,manpltNum)
    implicit none
    integer, intent(in) :: writORread
    integer, intent(in) :: manpltNum

    character(len=100) :: flnm,flnmbr,full_flnm
    
    integer :: i,j,jmax
    integer :: N_itm
    
    N_itm = 3

    flnm='InitiatnManplt'
    
    if ((manpltNum.gt.0) .and. (manpltNum.le.9)) then
       write(flnmbr,'(i1.1,a)') manpltNum,'.dat'
    elseif ((manpltNum.gt.9) .and. (manpltNum.le.99)) then
       write(flnmbr,'(i2.2,a)') manpltNum,'.dat'
    endif
    
    write(*,*) trim(adjustl(flnm))//trim(adjustl(flnmbr))
    full_flnm=trim(adjustl(flnm))//trim(adjustl(flnmbr))
    
    open(unit=82,file=trim(adjustl(full_flnm)))
    
    do i = 1,N_itm
       
       if (i==1) jmax = N_spr
       if (i==2) jmax = N_cell
       if (i==3) jmax = N_node
       
       do j = 1,jmax
          
          if (i==1) then
             
             if (writORread==1) write(82,*) k_spr(j),l0(j),j
             if (writORread==2) read(82,*)  k_spr(j),l0(j)
             
          elseif (i==2) then 
             
             if (writORread==1) write(82,*) k_area(j),A0(j),j
             if (writORread==2) read(82,*)  k_area(j),A0(j)
             
          elseif (i==3) then
             
             if (writORread==1) write(82,*) CgXNode(j),j
             if (writORread==2) read(82,*)  CgXNode(j)
             
          endif
          
       enddo

       if (writORread==1) write(82,*) " "
    enddo
    
    close(82)
    
  end subroutine savePropFrmPrvManplt
  
  
  subroutine propsOfAllCompnts_writeORread(cellNm,cntInRtn,deallctnNeeds)
    implicit none
    integer, intent(in) :: cellNm
    integer, intent(in) :: cntInRtn
    integer, intent(in) :: deallctnNeeds
    !ka_tobeCopied,A0_tobeCopied,ks_tobeCopied(:),l0_tobeCopied(:)
    integer :: i,sprInArea,sprNm
    
    if (deallctnNeeds==1) then
       deallocate(ks_tobeCopied)
       deallocate(l0_tobeCopied)
    endif
    
    if (cntInRtn==1) then
       
       open(unit=232,file='propsOfAllCompntsSave.dat')
       write(232,*) ka_tobeCopied,A0_tobeCopied
       
       ka_tobeCopied = k_area(cellNm)
       A0_tobeCopied = A0(cellNm)
       
       
       sprInArea = area_spr(cellNm,0)
       allocate(ks_tobeCopied(1:sprInArea))
       allocate(l0_tobeCopied(1:sprInArea))
       
       do i = 1,sprInArea
          sprNm            = area_spr(cellNm,i)
          ks_tobeCopied(i) = k_spr(sprNm)
          l0_tobeCopied(i) = l0(sprNm)
          write(232,*) ks_tobeCopied(i),l0_tobeCopied(i)
       enddo
       
        
       close(232)
       
    elseif (cntInRtn==2) then

       open(unit=233,file='propsOfAllCompntsSave.dat')
       read(233,*) ka_tobeCopied,A0_tobeCopied
       
       k_area(cellNm) = ka_toBeCopied
       A0(cellNm)     = A0_tobeCopied
       
       sprInArea = area_spr(cellNm,0)
       allocate(ks_tobeCopied(1:sprInArea))
       allocate(l0_tobeCopied(1:sprInArea))
       
       do i = 1,sprInArea
          
          read(233,*) ks_tobeCopied(i),l0_tobeCopied(i)
          
          sprNm        = area_spr(cellNm,i)
          k_spr(sprNm) = ks_tobeCopied(i)
          l0(sprNm)    = l0_tobeCopied(i)
       enddo
       
       close(233)
       
    endif
    
  end subroutine propsOfAllCompnts_writeORread
  
  
  subroutine adjust_the_firstFrm_of_EpithelialLayer(ExpNo,FrmNo)
    implicit none        !we will take the strctpropsAddedCell_TN and copy the ideal cell properties
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: N_itm
    integer :: i,j,jmax
    real*8  :: ksV(1:N_spr),kaV(1:N_cell)
    real*8  :: l0V(1:N_spr),A0V(1:N_cell)
    real*8  :: CgXV(1:N_node)
    integer :: idealCell
    integer :: sprNmIdeal,nsprsInTN
    integer :: cellNmL,cellNmR
    integer :: sprNmL,sprNmR,sprNm
    
    real*8               :: kaIdeal,A0Ideal
    real*8, allocatable  :: ksIdeal(:),l0Ideal(:)
    
    open(unit=78,file='strctPropsAddedCellTN2S1T1.dat')
    
    N_itm = 3
    
    do i = 1,N_itm
       
       if (i==1) jmax=N_spr
       if (i==2) jmax=N_cell
       if (i==3) jmax=N_node
       
       do j = 1,jmax
          if (i==1) read(78,*) ksV(j),l0V(j)
          if (i==2) read(78,*) kaV(j),A0V(j)
          if (i==3) read(78,*) CgXV(j)
       enddo
          
    enddo
    
    close(78)
    
    idealCell = 1
    
    kaIdeal = kaV(idealCell)
    A0Ideal = A0V(idealCell)
    
    nsprsInTN = 3
    allocate(ksIdeal(1:nsprsInTN))
    allocate(l0Ideal(1:nsprsInTN))
    
    do i = 1,nsprsInTN
       sprNmIdeal = (idealCell-1)*(nsprsInTN) + i 
       ksIdeal(i) = ksV(sprNmIdeal)
       l0Ideal(i) = l0V(sprNmIdeal)
    enddo
    
    
    do i = 1,Hlf_Ncell
       
       cellNmL=i                 ; cellNmR=i+Hlf_Ncell
       k_area(cellNmL) = kaIdeal ; A0(cellNmL) = A0Ideal
       k_area(cellNmR) = kaIdeal ; A0(cellNmR) = A0Ideal
       
       do j = 1,nsprsInTN
          sprNmL=(cellNmL-1)*(nsprsInTN)+j ; sprNmR=sprNmL+(Hlf_Ncell*nsprsInTN)
          k_spr(sprNmL) = ksIdeal(j) ; l0(sprNmL) = l0Ideal(j)
          k_spr(sprNmR) = ksIdeal(j) ; l0(sprNmR) = l0Ideal(j)
       enddo
          
    enddo
    
    k_area(N_cell) = kaIdeal ; A0(N_cell) = A0Ideal

    do i = 1,2
       sprNm        = (N_cell-1)*(nsprsInTN)+i
       k_spr(sprNm) = ksIdeal(i)
       l0(sprNm)    = l0Ideal(i)
    enddo
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
  end subroutine adjust_the_firstFrm_of_EpithelialLayer
  
  
  
  subroutine incrAllA0val_andReduceAll_l0Val(ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    real*8  :: Aval(1:N_cell)
    integer :: i,j
    
    Aval(1:N_cell) = A(1:N_cell)
    
    do i = 1,N_cell
       A0(i) = 1.20d0*A0(i)
    enddo
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)

    do j = 1,5
       do i = 1,N_spr
          l0(i) = 0.95d0*l0(i)
       enddo
       call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    enddo
       
  end subroutine incrAllA0val_andReduceAll_l0Val
  
  
  subroutine apply_ThreeDiff_force_at_TopNode_of_LS_ofAcell_andEquill(cellNm,dirctn,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cellNm
    integer, intent(in)    :: dirctn ! +1 for upward force, -1 for downward force
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: TopNodeOfLS1,TopNodeOfLS2 ! LS=LateralSide
    real*8  :: CgXSign
    
    TopNodeOfLS1 = (cellNm*2) + 1
    TopNodeOfLS2 = (Hlf_Ncell+1)*2 + (cellNm*2) + 1
    
    write(*,*) TopNodeOfLS1,TopNodeOfLS2,"TopNode L,R"
    
    if (dirctn==+1) CgXSign=-1.0d0
    if (dirctn==-1) CgXSign=+1.0d0
    
    write(*,*) ExpNo,FrmNo,"FrmNo in apply Force at Begin"
    
    CgXNode(TopNodeOfLS1) = (CgXSign*lowrF) ; CgXNode(TopNodeOfLS2) = (CgXSign*lowrF)
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    call check_grd_at_both_TNandNI
    
    CgXNode(TopNodeOfLS1) = (CgXSign*medmF) ; CgXNode(TopNodeOfLS2) = (CgXSign*medmF)
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    call check_grd_at_both_TNandNI
    
    CgXNode(TopNodeOfLS1) = (CgXSign*higrF) ; CgXNode(TopNodeOfLS2) = (CgXSign*higrF)
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    call check_grd_at_both_TNandNI
    
    CgXNode(TopNodeOfLS1) = zeroF ; CgXNode(TopNodeOfLS2) = zeroF
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
    write(*,*) CgXNode(1),CgXNode(TopNodeOfLS1),"CGNODE"
    write(*,*) ExpNo,(FrmNo-1),"FrmNo in apply force at End"
    
    
    
  end subroutine apply_ThreeDiff_force_at_TopNode_of_LS_ofAcell_andEquill
  
  
  subroutine apply_ThreeDiff_force_at_IntrmdNode(cellNm,ApBsLt,sgmntNo,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: cellNm,ApBsLt,sgmntNo,ExpNo
    integer, intent(inout) :: FrmNo
    
    !call switch_to_NI
    
    ! if (ApBsLt==1) then
       
    !    if (NAEC_Apcl.ne.0) then
    !       continue
    !    else
    !       write(*,*) "Not in Apical side"
    !    endif
          
    ! elseif (ApBsLt==2) then
       
    !    if (NAEC_Bsal.ne.0) then
          
    !    else
    !       continue
    !    endif
       
    ! elseif (ApBsLt==3) then
       
    !    if (NAEC_Ltrl.ne.0) then
          
    !    else
          
    !    endif
       
    ! endif
    
  end subroutine apply_ThreeDiff_force_at_IntrmdNode
  
  ! subroutine chk_forces_at_joint_node()
  !   implicit none
  !   real*8, allocatable :: forceAtTN(:,:),forceAtNI(:,:)
  !   real*8, allocatable :: CgXSave(:)
    
  !   integer :: i,j
    
  !   allocate(forceAtTN(1:N_node,1:N_dmnsn))
  !   allocate(CgXSave(1:N_node))
    
  !   open(unit=651,file='chk_forces_at_joint_node.dat')
    
  !   CgXSave(1:N_node) = CgXNode(1:N_node)

  !   write(651,*) select_xy,"slctXT" 
  !   write(651,*) "writing before changes"
  !   do i = 1,N_node
  !      write(651,*) CgXNode(i),i,"CgXVal"
  !   enddo
    
  !   close(651)
    
  !   call get_
  !   forceAtTN()
    
  ! end subroutine chk_forces_at_joint_node
  
  
  subroutine very_stiff_areas(ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    real*8, allocatable :: kaSave(:),A0Save(:)
    real*8  :: incrmntFac
    integer :: i,j
    
    allocate(kaSave(1:N_cell),A0Save(1:N_cell))
    
    kaSave(1:N_cell) = k_area(1:N_cell)
    A0Save(1:N_cell) = A0(1:N_cell)
    
    !now I will equate the A0 as A, and increase the k_area by say 10 times
    
    A0(1:N_cell) = A(1:N_cell)
    
    incrmntFac = 10.0d0
    
    do i = 1,N_cell
       k_area(i) = (incrmntFac)*(k_area(i))
    enddo
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
  end subroutine very_stiff_areas
  
  ! routines for applyling force before the lateral membranes actually meet
  
  subroutine read_get_the_conditions_force_bfr_apclSpr_demlish(demlishingSpr,initialL,tolrncL)
    implicit none
    integer, intent(out) :: demlishingSpr
    real*8 , intent(out) :: initialL,tolrncL
    
    integer  :: nsprsInACellTN,nsprsInACellNI 
    real*8   :: prcntTol
    
    nsprsInACellTN  = 1+1+1 
    nsprsInACellNI  = (NAEC_Apcl+1)+(NAEC_Bsal+1)+(NAEC_Ltrl+1) 
    
    write(*,*) nsprsInACellTN, nsprsInACellNI,"nsrpsInACell TN,NI" 
    
    demlishingSpr  = (N_cell-1)*(nsprsInACellTN) + 1
    initialL       = l(demlishingSpr)
    prcntTol       = 0.85d0
    tolrncL        = (prcntTol)*(initialL)
    write(*,*) initialL,tolrncL,"initialL-tol L"
    
  end subroutine read_get_the_conditions_force_bfr_apclSpr_demlish
  
  
  subroutine chk_condn_bfrPutting_downwardForce(demlishingSpr,tolrncL,lgcl_condn)
    implicit none
    integer, intent(in)  :: demlishingSpr
    real*8 , intent(in)  :: tolrncL
    logical, intent(out) :: lgcl_condn
    
    real*8 :: currLen
    
    lgcl_condn = .False.
    currLen = l(demlishingSpr)
    
    if (currLen.lt.tolrncL) then
       lgcl_condn = .True.
    endif
    
  end subroutine chk_condn_bfrPutting_downwardForce
  
  subroutine chkCondtnAndPutForceBfrPulleyPntCreatn(demlishingSpr,tolrncL,ExpNo,FrmNo) 
    implicit none
    integer, intent(in)    :: demlishingSpr,ExpNo
    real*8 , intent(in)    :: tolrncL
    integer, intent(inout) :: FrmNo
    
    real*8   :: currLen 
    integer  :: nsprsInACellTN,nsprsInACellNI 
    integer  :: nodeL=-1,nodeR=-2
    
    call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
    
    currLen = l(demlishingSpr)
    write(*,*) currLen,tolrncL,"curr-tol L"
    
    if (currLen.lt.tolrncL) then 
       call change_NodeTypForThetobejoiningNodes(nodeL,nodeR)
       call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
       call applyForceAtToBeJoiningNodes(nodeL,nodeR,ExpNo,FrmNo) 
    endif
    
  end subroutine chkCondtnAndPutForceBfrPulleyPntCreatn
  
  
  
  subroutine applyForceAtToBeJoiningNodes(nodeL,nodeR,ExpNo,FrmNo) 
    implicit none 
    integer, intent(in)    :: nodeL,nodeR 
    integer, intent(in)    :: ExpNo 
    integer, intent(inout) :: FrmNo 
    
    integer  :: i, N_diffForces 
    real*8   :: incrCgX 
    
    
    N_diffForces = 3 
    
    do i = 1,N_diffForces 
       
       incrCgX = real(i-1)*0.05d0 
       
       CgYNode(nodeL) = 0.15d0 + incrCgX 
       CgYNode(nodeR) = CgYNode(nodeL) 
       
       call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
       
    enddo
    write(*,*) "stopping in applyForceAtToBeJoiningNodes"
    !stop
    
  end subroutine applyForceAtToBeJoiningNodes
  
  subroutine apply_Force_aft_ManuallyShifting_Nodes(ExpNo,FrmNo) 
    implicit none 
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer  :: nodeL,nodeR 
    integer  :: i, N_diffForces 
    real*8   :: incrCgY
    integer  :: fileNo
    real*8   :: E
    integer  :: cell_val,crtclNode
    
    call get_the_shifting_nodes(nodeL,nodeR)
    N_diffForces = 21
    
    call find_the_cell_with_diagonal_spr(cell_val)
    crtclNode = (2*cell_val)+3
    write(*,*) cell_val,crtclNode,"cell_val and crtclNode"
    
    do i = 1,N_diffForces
       
       incrCgY        = real(i-1)*0.05d0   
       CgYNode(nodeL) = incrCgY 
       CgYNode(nodeR) = CgYNode(nodeL) 
       
       write(*,*) CgYNode(nodeL),CgYNode(nodeR),nodeL,nodeR,"CgY vals inside apply_Force_aft_Man"
       call Equilibrate_only_NI_model
       
       !call Equilibrate_bothTN_NImodel(ExpNo,FrmNo)
       !write(*,*) (FrmNo-1),i,"FrmNos in apply_force"
       
       !fileNo=i+1 ; call propCheckForDiffFile(fileNo)
       !write(*,*) A0(N_cell),"A0-N_cell"
       
       ! if (i==1) then
       !    call switch_to_NI_model
       !    E = Energy(node_xy,l0,A0)
       !    allocate(ksv2NI(1:N_spr),kav2NI(1:N_cell),l0v2NI(1:N_spr),A0v2NI(1:N_cell))
       !    allocate(CgXv2NI(1:N_node),CgYv2NI(1:N_node))
       !    ksv2NI=k_spr ; kav2NI=k_area ; l0v2NI=l0 ; A0v2NI=A0 ; CgXv2NI=CgXNode ; CgYv2NI=CgYNode
       !    call not_Equlibrate_backto_TN
       !    !exit
       ! endif
       
       if (abs(node_xy(crtclNode,1)) .le. 0.23d0) then
          write(*,*) i,"loop val Val at exit"
          exit
       endif
       
    enddo
    
    write(*,*) "apply_Force_aft_ManuallyShifting_Nodes"
    !stop
    
  end subroutine apply_Force_aft_ManuallyShifting_Nodes
  
  subroutine get_the_shifting_nodes(nodeL,nodeR)
    implicit none
    integer, intent(out) :: nodeL,nodeR
    
    nodeL = (Hlf_Ncell)*2 + 1
    nodeR = (Hlf_Ncell+1)*2 + nodeL
    
    write(*,*) nodeL,nodeR,"nodeL-R in the get the shifting nodes"
    
  end subroutine get_the_shifting_nodes
  
  
  subroutine change_NodeTypForThetobejoiningNodes(nodeL,nodeR)
    implicit none 
    integer, intent(out) :: nodeL,nodeR 
    
    nodeL=(Hlf_Ncell)*2+1 
    nodeR=(Hlf_Ncell+1)*2+nodeL
    
    write(*,*) nodeL,nodeR,"nodes" 
    write(*,*) node_typ(nodeL),node_typ(nodeR),"check node_typs" 
    
    if (node_typ(nodeL) .ne. 2) then
       write(*,*) "node_typ must be 2 here"
    endif
    
    node_typ(nodeL) = 1 
    node_typ(nodeR) = 1 ! make both the nodes free 
    
  end subroutine change_NodeTypForThetobejoiningNodes
  
  subroutine propCheckForDiffFile(fileNo)
    implicit none
    integer, intent(in) :: fileNo
    character(len=100)  :: fileNm

    integer :: i,j,imax,jmax
    
    write(fileNm,'(a,i2.2,a)')"propCheck",fileNo,'.dat'
    write(*,*)trim(adjustl(fileNm))
    
    open(unit=244,file=trim(adjustl(fileNm)))

    imax = 3
    
    do i = 1,imax
       
       if (i==1) jmax = N_spr
       if (i==2) jmax = N_cell
       if (i==3) jmax = N_node 
       
       do j = 1,jmax
          if (i==1) write(244,*) k_spr(j),l0(j),j
          if (i==2) write(244,*) k_area(j),A0(j),j
          if (i==3) write(244,*) CgXNode(j),CgYNode(j),j
       enddo
    
    enddo
    
    close(244)
    
  end subroutine propCheckForDiffFile
  
  
  subroutine Apcl_shorten_Adjacent_of_IC_and_incr_vrtcl_force_to_hold(ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    real*8  :: hmL0Ap,hmL0Bs,hmL0Lt
    integer :: cell_val
    real*8  :: CgY_incr
    integer :: nodeL,nodeR
    real*8  :: y_nodeL,y_nodeR
    integer :: cntLp1,cntLp2
    
    integer, allocatable :: ApclSp(:),BsalSp(:),LtrlSp(:)
    
    hmL0Ap   = 0.85d0 ; CgY_incr = 0.05d0
    cell_val = Hlf_Ncell
    
    allocate(ApclSp(1:(NAEC_Apcl+1)),BsalSp(1:(NAEC_Bsal+1)),LtrlSp(1:(NAEC_Ltrl+1)))
    call get_ApclBsalLtrl_OfNIsystm(cell_val,ApclSp,BsalSp,LtrlSp)
    
    call get_the_shifting_nodes(nodeL,nodeR)
    y_nodeL = node_xy(nodeL,2) ; y_nodeR = node_xy(nodeR,2)
    write(*,*) nodeL,nodeR,y_nodeL,y_nodeR,"nodeL,nodeR,y_node L and R"
    
    write(*,*) CgYNode(nodeL),CgYNode(nodeR),"prv_CgYnodeL"
    !stop
    
    
    do cntLp1=1,6
       
       write(*,*) "cntLp1 value and Frame_NI",cntLp1,Frame_NI-1
       
       call manplt_spr_NIsys(ApclSp,NAEC_Apcl,hmL0Ap)
       call Equilibrate_only_NI_model
       write(*,*) node_xy(nodeL,2),node_xy(nodeR,2),"ynodeL aft Apcl Incr"
       write(*,*) Frame_NI-1,"Frame_NI aft Apcl Incr"
       
       cntLp2 = 0
       
       do
          
          CgYNode(nodeL) = CgYNode(nodeL) + CgY_incr
          CgYNode(nodeR) = CgYNode(nodeL)
          call Equilibrate_only_NI_model
          write(*,*) node_xy(nodeL,2),node_xy(nodeR,2),"ynodeL aft Cg Incr"
          write(*,*) CgYNode(nodeL),CgYNode(nodeR),"CgYNode in Lp"
          
          write(*,*) Frame_NI-1,"Frame_NI aft Cg Incr"
          cntLp2 = cntLp2+1

          if (cntLp2.ge.5) then
             write(*,*) "Exiting for bounding"
             exit
          endif
          
          if (abs(node_xy(nodeL,2)) .gt. abs(y_nodeL)) then
             write(*,*) abs(node_xy(nodeL,2)),abs(y_nodeL),"abs val"
             exit
          endif
          
       enddo
       
    enddo
    
  end subroutine Apcl_shorten_Adjacent_of_IC_and_incr_vrtcl_force_to_hold
  
  subroutine tilt_reduction_of_the_boundry(forMakingCellsMeet,ExpNo,FrmNo)
    implicit none
    integer, intent(in)    :: forMakingCellsMeet
    integer, intent(in)    :: ExpNo
    integer, intent(inout) :: FrmNo
    
    integer :: cell_val,cnt
    real*8  :: hmL0Ap1,hmL0Bs1,hmL0Ap2,hmL0Bs2,hmL0Ap3,hmL0Bs3,hmL0Ap4,hmL0Bs4
    
    integer, allocatable :: ApclSp1(:),BsalSp1(:),LtrlSp1(:)
    integer, allocatable :: ApclSp2(:),BsalSp2(:),LtrlSp2(:)
    integer, allocatable :: ApclSp3(:),BsalSp3(:),LtrlSp3(:)
    integer, allocatable :: ApclSp4(:),BsalSp4(:)
    
    integer :: readTheFile
    integer :: FrmNoIncr
    integer :: cnt1,cnt2
    
    allocate(ApclSp1(1:(NAEC_Apcl+1)),BsalSp1(1:(NAEC_Bsal+1)),LtrlSp1(1:(NAEC_Ltrl+1)))
    allocate(ApclSp2(1:(NAEC_Apcl+1)),BsalSp2(1:(NAEC_Bsal+1)),LtrlSp2(1:(NAEC_Ltrl+1)))
    allocate(ApclSp3(1:(NAEC_Apcl+1)),BsalSp3(1:(NAEC_Bsal+1)),LtrlSp3(1:(NAEC_Ltrl+1)))
    allocate(ApclSp4(1:(NAEC_Apcl+1)),BsalSp4(1:(NAEC_Bsal+1)))
    
    cell_val=Hlf_Ncell-1 ; call get_ApclBsalLtrl_OfNIsystm(cell_val,ApclSp1,BsalSp1,LtrlSp1)
    cell_val=Hlf_Ncell-2 ; call get_ApclBsalLtrl_OfNIsystm(cell_val,ApclSp2,BsalSp2,LtrlSp2)
    cell_val=Hlf_Ncell-0 ; call get_ApclBsalLtrl_OfNIsystm(cell_val,ApclSp3,BsalSp3,LtrlSp3)
    
    if (CellsMeet==0)   call get_ApclBsal_singl_cell_NIsys_with_CM0(ApclSp4,BsalSp4)
    if (CellsMeet.gt.0) call get_Bsal_singl_cell_NIsys_with_CM_gt_0(BsalSp4)
    
    write(*,*) ApclSp1,"Apcl1",BsalSp1,"Bsal1"
    write(*,*) ApclSp2,"Apcl2",BsalSp2,"Bsal2"
    write(*,*) ApclSp3,"Apcl3",BsalSp3,"Bsal3"
    
    if (CellsMeet==0)   write(*,*) ApclSp4,"Apcl4",BsalSp4,"Bsal4"
    if (CellsMeet.gt.0) write(*,*) BsalSp4,"Bsal4"
    
    readTheFile = 0
    
    if (readTheFile==0) then
       
       if (forMakingCellsMeet==1) then
          cnt1=10  ; cnt2=4
       elseif (forMakingCellsMeet==2) then
          cnt1=10 ; cnt2=7
       endif
       
       do cnt = 1,cnt1
          hmL0Bs1=0.95d0 ; call manplt_spr_NIsys(BsalSp1,NAEC_Bsal,hmL0Bs1)
          hmL0Bs2=0.95d0 ; call manplt_spr_NIsys(BsalSp2,NAEC_Bsal,hmL0Bs2)
          hmL0Bs3=0.95d0 ; call manplt_spr_NIsys(BsalSp3,NAEC_Bsal,hmL0Bs3)
          hmL0Bs4=0.95d0 ; call manplt_spr_of_singl_cell_NIsys(BsalSp4,NAEC_Bsal,hmL0Bs4)
          call Equilibrate_only_NI_model
       enddo
       
       do cnt = 1,cnt2
          hmL0Ap1=1.05d0 ; call manplt_spr_NIsys(ApclSp1,NAEC_Apcl,hmL0Ap1)
          hmL0Ap2=1.05d0 ; call manplt_spr_NIsys(ApclSp2,NAEC_Apcl,hmL0Ap2)
          call Equilibrate_only_NI_model
       enddo
       
    elseif (readTheFile==1) then
       
       write(*,*) Frame_NI,"Frame_NI bfr increasing"
       
       FrmNoIncr = 17
       Frame_NI  = Frame_NI+FrmNoIncr-1
       
       write(*,*) Frame_NI,"Frame_NI aft increasing"
       
       call read_config_and_start_simlnFrm_there(Exprmnt_NI,Frame_NI)
       call Equilibrate_only_NI_model
       
    endif
    
  end subroutine tilt_reduction_of_the_boundry
  
  
  !hmL0Ap1=1.05d0 ; call manplt_spr_NIsys(ApclSp1,NAEC_Apcl,hmL0Ap1)
  !hmL0Ap2=1.05d0 ; call manplt_spr_NIsys(ApclSp2,NAEC_Apcl,hmL0Ap2)
  
  !write(*,*) l0(ApclSp1(1)),l0(BsalSp1(1:3)),l0(LtrlSp1(1:3)),"Ap1-Bs1-Lt1",cnt
  !write(*,*) l0(ApclSp2(1)),l0(BsalSp2(1:3)),l0(LtrlSp1(1:3)),"Ap2-Bs2-Lt2",cnt
  
  
  subroutine flatten_the_IC_and_maintain_Area_and_Length
    implicit none
    integer, allocatable :: ApclSp(:),BsalSp(:),LtrlSp1(:),LtrlSp2(:)
    integer :: nsprInACell
    integer :: i,j,imax,jmax
    
    if (modelID==1) then
       write(*,*) "not for modelID=1",modelID,"sb: flatten_the_IC_and_maintain_Area_and_Length"
       stop
    endif

    
    nsprInACell = (NAEC_Apcl+1) + (NAEC_Bsal+1) + (NAEC_Ltrl+1)
    allocatable(ApclSp(1:(NAEC_Apcl+1)),BsalSp(1:(NAEC_Bsal+1)),LtrlSp1(1:(NAEC_Ltrl+1)),LtrlSp2(1:(NAEC_Ltrl+1)))

    do i = 1,4

       do j = 1,jmax
          ApclSp = (N_cell-1)*(nsprInACell) + 1
          
       enddo
       
    enddo
    
  end subroutine flatten_the_IC_and_maintain_Area_and_Length
  
end module MAE_initaitn
