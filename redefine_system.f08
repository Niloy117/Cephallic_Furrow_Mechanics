
module redefining_system_module
  use system_parameters
  use transfrm_info
  use mltiple_calls_together
  use switch_and_unswitch_models
  
  implicit none
  real*8,allocatable :: nodeXY_Saved(:,:)
  
  integer :: sprDem !spring to be demolished
  integer :: sprDemForCmbning
  real*8  :: ydis_frmVitlnMem
  real*8  :: ydis_aftCmbingSpr
  
contains
  
  subroutine allocate_rdfnmod_vars
    implicit none
    
    allocate(nodeXY_Saved(1:N_node,1:N_dmnsn))
    
  end subroutine allocate_rdfnmod_vars
  
  subroutine deallocate_rdfnmod_vars
    implicit none
    
    if (CyclNo.gt.1) then
       deallocate(nodeXY_Saved)
    endif
    
  end subroutine deallocate_rdfnmod_vars
  
  subroutine redefine_system
    implicit none
    
    call deallocate_moving_coordnte_variables
    call deallocate_transfrm_variables
    call deallocate_phiInfo
    call deallocate_curve_variables
    
    call redefining_system_parameters_stage4
    call redefining_nodeTypes_Ends_stage4
    call get_nodeXY_coordntesXYbfrRdfn
    
    call redefining_spring_types_and_ends_stage4
    call redefining_area_types_stage4
    call get_SideWiseCellNo
    
    call redefining_CgXNode_stage4
    
    call get_list_of_double_nodes_method2
    call nodes_cnnctd_and_count_this_dn
    call get_all_moving_coordnte_variables
    call nodes_to_coordntes(node_xy,coordntes_xy)
    call get_all_the_transfrms
    
  end subroutine redefine_system
  
  subroutine redeine_system_for_changing_only_node_typ
    implicit none
    integer :: node_typ_chng_case
    
    call deallocate_moving_coordnte_variables_wo_StrVars
    node_typ_chng_case=1 ; call change_node_typ(node_typ_chng_case)
    
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
  end subroutine redeine_system_for_changing_only_node_typ
  
  subroutine change_node_typ(node_typ_chng_case)
    implicit none
    integer, intent(in) :: node_typ_chng_case
    integer             :: node1,node2
    
    if (node_typ_chng_case == 1) then
       
       node1 = (Hlf_Ncell+1)*2 - 1
       node2 = 2*(Hlf_Ncell+1)*2 - 1
    
       write(*,*) node1,node2,"node1-2"
       write(*,*) node_typ(node1),node_typ(node2),"curr_node_typ"
       
       node_typ(node1) = 0
       node_typ(node2) = 0
    
       write(*,*) node_typ(node1),node_typ(node2),"changed_node_typ"
       
    endif
    
  end subroutine change_node_typ
  
  subroutine redefining_system_parameters_stage4
    implicit none

    open(unit=167,file='redefine_system.dat')
    
    strctNo = 2
    write(167,fmt=*) strctNo, "strct No"
    
    !lft_ladder_params
    ncl  = ncl - 1
    nsl  = nsecl * ncl
    nvsl = ncl + 1
    
    lft_node = 2*nvsl
    
    write(167,fmt=*) ncl,nsl,nvsl,lft_node,"ncl,nsl,nvsl,lft_Node"
    
    !rght_ladder_params
    ncr  = ncr - 1
    nsr  = nsecr * ncr
    nvsr = ncr + 1
    
    rght_node     = 2*nvsr
    
    write(167,fmt=*) ncr,nsr,nvsr,rght_node,"ncl,nsl,nvsl,lft_Node"
    
    !clft_top_params & clft_rght_params
    
    call clft_top_params
    call crght_top_params

    write(167,fmt=*) nsclt,nvsclt,clft_top_node,clft_top_spr,clft_top_end_spr,&
         clft_top_cell_nmbr,"clft_top_params"
    write(167,fmt=*) nscrt,nvscrt,crght_top_node,crght_top_spr,crght_top_end_spr,&
         crght_top_cell_nmbr,"crght_top_params"
    
    
    !clft_bot_params
    ncclb   = ncclb + 1
    nsclb   = nsecclb * ncclb
    nvsclb  = ncclb
    
    clft_bot_node      = 2*ncclb
    clft_bot_end_node  = lft_node + rght_node + clft_top_node &
         + additnl_node + crght_top_node + clft_bot_node
    
    clft_bot_spr       = nsclb
    clft_bot_end_spr   = nsl + nsr + clft_top_spr + additnl_spr &
         + crght_top_spr + clft_bot_spr
    
    first_spr_of_clft_bot = nsl+ nsr+ clft_top_spr+ crght_top_spr+ 1 
    clft_bot_cell_nmbr    = ncl + ncr + ncclt + nccrt + 2*ncclb - 1 !2 for symmetry
    write(167,fmt=*) ncclb,nsclb,nvsclb,clft_bot_node,clft_bot_end_node,&
         clft_bot_spr,clft_bot_end_spr,first_spr_of_clft_bot, clft_bot_cell_nmbr,"clft bot params"

    !crght_bot_params
    nccrb = nccrb + 1
    nscrb   = nseccrb * nccrb
    nvscrb  = nccrb

    crght_bot_node     = 2*nccrb
    crght_bot_end_node = lft_node + rght_node + clft_top_node &
         + additnl_node + crght_top_node + clft_bot_node + crght_bot_node

    crght_bot_spr     = nscrb
    crght_bot_end_spr = nsl + nsr + clft_top_spr + additnl_spr &
         + crght_top_spr + clft_bot_spr + crght_bot_spr

    first_spr_of_crght_bot = nsl+ nsr+ clft_top_spr+ crght_top_spr+ nsecclb+ 1  
    crght_bot_cell_nmbr    = ncl + ncr + ncclt + nccrt + ncclb + nccrb

    write(167,fmt=*) nccrb,nscrb,nvscrb,crght_bot_node,crght_bot_end_node,&
         crght_bot_spr,crght_bot_end_spr,first_spr_of_crght_bot, crght_bot_cell_nmbr,"crght bot params"
    
    call global_params !checking as redefining wont change these params
    
    write(167,fmt=*) N_node,N_spr,N_cell,Hlf_Ncell,"N_node,N_spr,N_cell,Hlf_Ncell"
    write(167,fmt=*) N_lftCells,N_rghtCells,"N lft and rght Cells"

    close(167)
    
  end subroutine redefining_system_parameters_stage4

  
  subroutine redefining_nodeTypes_Ends_stage4
    implicit none

    count_nodes = 0
    
    do i = 1,nvsl

       if (i .eq. 1) then
          node_typ(count_nodes+1) = 2
          node_typ(count_nodes+2) = 2

       elseif (i.eq.2) then
          node_typ(count_nodes+1) = 2 
          node_typ(count_nodes+2) = 1
          
       else
          node_typ(count_nodes+1) = 2
          node_typ(count_nodes+2) = 1
          
       endif
       
       if (i .eq. nvsl) then
          lft_endNode(0) = 2
          lft_endNode(1) = count_nodes + 1 
          lft_endNode(2) = count_nodes + 2
       endif
       
       count_nodes = count_nodes + 2
    enddo
    
    do i = 1,nvsr
       
       if (i==1) then
          node_typ(count_nodes+1) = 2
          node_typ(count_nodes+2) = 2
          
       elseif (i==2) then
          node_typ(count_nodes+1) = 2
          node_typ(count_nodes+2) = 1
       else
          node_typ(count_nodes+1) = 2
          node_typ(count_nodes+2) = 1
       endif
       
       if (i .eq. nvsr) then
          rght_endNode(0) = 2
          rght_endNode(1) = count_nodes + 1
          rght_endNode(2) = count_nodes + 2
       endif
       
       count_nodes = count_nodes + 2
    enddo
    
    write(*,*) count_nodes,"count_nodes rdfn1"
    
    do i = 1,clft_top_node
       
       if(i .eq. 1) then
          node_typ(count_nodes+1)    = 0
       elseif (i .eq. 2) then  
          node_typ(count_nodes+1)    = 1
       elseif (i .eq. 3) then
          node_typ(count_nodes+1)    = 1  
       endif
       
       if (i.eq.clft_top_node) then
          clft_top_endNode(0) = 3
          clft_top_endNode(1) = count_nodes - 1 
          clft_top_endNode(2) = count_nodes
          clft_top_endNode(3) = count_nodes + 1
          !write(*,*) "Entered"
       endif
       
       count_nodes = count_nodes + 1
    enddo
    
    write(*,*) clft_top_endNode(0:3),"clft"
    !stop
    
    do i = 1,crght_top_node
       
       if (i .eq. 1) then   !Pulley
          node_typ(count_nodes+1) = 0
       elseif (i .eq. 2) then
          node_typ(count_nodes+1) = 1
       else
          node_typ(count_nodes+1) = 1
       endif
       
       if (i.eq.crght_top_node) then
          crght_top_endNode(0) = 3
          crght_top_endNode(1) = count_nodes - 1 
          crght_top_endNode(2) = count_nodes
          crght_top_endNode(3) = count_nodes + 1
       endif
       
       count_nodes = count_nodes + 1
    enddo

    node_typ((count_nodes+1):N_node) = 1
    
    !clft_bot_endNode and crght_bot_endNode wont change
    
  end subroutine redefining_nodeTypes_Ends_stage4
  
  
  subroutine redefining_spring_types_and_ends_stage4
    implicit none
    integer :: i
    integer :: count_spr
    
    count_spr = 0
    
    do i = 1,nsl
       count_spr = count_spr + 1
       
       if (i .eq. 1) typ_spr(count_spr) = 1
       if (i .eq. 2) typ_spr(count_spr) = 2
       
       if (i.gt.2) then
          if (mod(i,3) .eq. 0) then
             typ_spr(count_spr) = 5
          elseif (mod(i,3) .eq. 1) then
             typ_spr(count_spr) = 3 
          else
             typ_spr(count_spr) = 4
          endif
       endif
       
       
       if (i.eq.nsl) then
          lft_endSpring(0) = 1
          lft_endSpring(1) = count_spr
       endif
    enddo
    
    do i = 1,nsr
       count_spr = count_spr + 1
       
       if (i.eq.1) typ_spr(count_spr) = 1
       if (i.eq.2) typ_spr(count_spr) = 2
       
       if (i.gt.2) then
          if (mod(i,3) .eq. 0) then
             typ_spr(count_spr) = 5
          elseif (mod(i,3) .eq. 1) then
             typ_spr(count_spr) = 3
          else
             typ_spr(count_spr) = 4
          endif
       endif

       if (i.eq.nsr) then
          rght_endSpring(0) = 1
          rght_endSpring(1) = count_spr
       endif
          
    enddo

    
    do i = 1, clft_top_spr
       count_spr = count_spr+1

       if (i.eq.1) then
          typ_spr(count_spr) = 6
          
       elseif (i .eq. 2) then 
          typ_spr(count_spr) = 7 
          
       elseif (i .eq. 3) then
          typ_spr(count_spr) = 8
          
       endif
       
       if (i.eq.1) then
          clft_top_endSpring(1) = count_spr
       elseif (i.eq.clft_top_spr) then
          clft_top_endSpring(2) = count_spr
       endif

    enddo
    

    do i = 1, crght_top_spr
       count_spr = count_spr + 1
       
       if (i .eq. 1) then
          typ_spr(count_spr) = 6
          
       elseif (i.eq.2) then
          typ_spr(count_spr) = 7
          
       elseif (i.eq.3) then
          typ_spr(count_spr) = 8
          
       endif

       if (i .eq. 1) then
          crght_top_endSpring(1) = count_spr
       elseif (i .eq. crght_top_spr) then
          crght_top_endSpring(2) = count_spr
       endif
       
    enddo
    
    
    do i = 1, nsclb
      
       if (i.ne.1 .and. mod(i,nsecclb).eq.1) then !nsecclb=3
          count_spr = count_spr + 4
       else
          count_spr = count_spr + 1
       endif
       
       if (mod(i,3).eq.1) then
          typ_spr(count_spr) = 3
       elseif (mod(i,3) .eq. 2) then 
          typ_spr(count_spr) = 4
       elseif (mod(i,3) .eq. 0) then
          typ_spr(count_spr) = 5
       endif
       
       
       if (i==nsclb) then
          clft_bot_endSpring(1) = count_spr
       endif
       
    enddo
    
    count_spr = nsl + nsr + nsclt + nscrt + nsecclb
    
    do i = 1,nscrb 
       
       if (i.ne.1 .and. mod(i,nseccrb).eq.1) then !nseccrb=3
          count_spr = count_spr + 4
       else
          count_spr = count_spr + 1
       endif
       
       if (mod(i,3).eq.1) then
          typ_spr(count_spr) = 3   
       elseif (mod(i,3) .eq. 2) then 
          typ_spr(count_spr) = 4
       elseif (mod(i,3) .eq. 0) then
          typ_spr(count_spr) = 5
       endif
       
       if (i==nscrb) then
          crght_bot_endSpring(1) = count_spr
       endif
       
    enddo
    
  end subroutine redefining_spring_types_and_ends_stage4
  
  
  subroutine redefining_area_types_stage4
    implicit none
    integer :: j
    integer :: count_area
    
    count_area = 0

    do j = 1,ncl
       count_area = count_area + 1
       typ_area(count_area) = 1
    enddo
    
    do j = 1,ncr
       count_area = count_area + 1
       typ_area(count_area) = 1
    enddo

    do j = 1,ncclt
       count_area = count_area + 1
       typ_area(count_area) = 2 
    enddo

    do j = 1,nccrt
       count_area = count_area + 1
       typ_area(count_area) = 2 
    enddo
    

    count_area = ncl + ncr + ncclt + nccrt
    
    do j = 1,ncclb
       
       if (j.eq.1) then
          count_area = count_area + 1
          typ_area(count_area) = 3
          
       elseif (j.eq.2) then
          count_area = count_area + 2
          typ_area(count_area) = 4
          
       else
          count_area = count_area + 2
          typ_area(count_area) = 5
          
       endif
       
    enddo

    count_area = ncl + ncr + ncclt + nccrt + 1
    
    do j = 1,nccrb
       
       if (j.eq.1) then
          count_area = count_area+1
          typ_area(count_area) = 3
          
       elseif (j.eq.2) then
          count_area = count_area+2
          typ_area(count_area) = 4
          
       else
          count_area = count_area+2
          typ_area(count_area) = 5
          
       endif
       
    enddo
    
  end subroutine redefining_area_types_stage4

  
  subroutine redefining_CgXNode_stage4
    implicit none
    integer :: nodes_Rdfn(1:N_node)
    integer :: i
    integer :: node_nm
    real*8  :: CgXNode_prv(1:N_node)

    CgXNode_prv = CgXNode
    
    call get_rdfnNodes(nodes_Rdfn)

    open(unit=34,file='CgX_NodeRdfn.dat')

    
    do i = 1,N_node
       node_nm = nodes_Rdfn(i)
       CgXNode(i) = CgXNode_prv(node_nm)
       write(unit=34,fmt=*) CgXNode(i),i,node_nm,"CgXNode,i,node_nm"
    enddo

    close(34)
    
  end subroutine redefining_CgXNode_stage4
  
  
  subroutine get_nodeXY_coordntesXYbfrRdfn
    implicit none
    integer :: nodes_Rdfn(1:N_node)
    integer :: i,j
    integer :: node_nm
    
    call get_rdfnNodes(nodes_Rdfn)
    
    open(unit=211,file='check_nodeXY_aftRdfn.dat')
    open(unit=212,file='node_xyBfrRdfn.dat')

    do i = 1,N_node
       node_nm = nodes_Rdfn(i)
       
       !write(*,*) nodeXY_Saved(node_nm,1:2),i,node_typ(i),"node_xy,node,typ"
       
       do j = 1,N_dmnsn
          node_xy(i,j) = nodeXY_Saved(node_nm,j)
          nodeXY_strctTN(3,i,j) = nodeXY_Saved(node_nm,j)
       enddo

       
       write(211,fmt=*) node_xy(i,1:2),i,node_typ(i),nodes_Rdfn(i),"node_xy,node,node,typ,rd"
       write(212,fmt=*) nodeXY_Saved(i,1:2),i,"nodeXYsaved"
       
    enddo
    
    !call nodes_to_coordntes(node_xy,coordntes_xy)
    
    !write(*,*) coordntes_xy,"coordntes_xy"

    close(211)
    close(212)
    
  end subroutine get_nodeXY_coordntesXYbfrRdfn

  
  subroutine get_rdfnNodes1(nodes_Rdfn)
    implicit none
    integer,intent(inout) :: nodes_Rdfn(1:N_node)
    integer :: i
    
    open(unit=305,file='nodesRdfn.dat')
    
    do i = 1,N_node
       if (i.le.8) then
          nodes_Rdfn(i) = i
          
       elseif(i.gt.8 .and. i.le.24) then
          
          if (i==9)  nodes_Rdfn(i) = 11
          if (i==10) nodes_Rdfn(i) = 12
          if (i==11) nodes_Rdfn(i) = 13
          if (i==12) nodes_Rdfn(i) = 14
          if (i==13) nodes_Rdfn(i) = 15
          if (i==14) nodes_Rdfn(i) = 16
          if (i==15) nodes_Rdfn(i) = 17
          if (i==16) nodes_Rdfn(i) = 18
          
          if (i==17) nodes_Rdfn(i) = 21
          if (i==18) nodes_Rdfn(i) = 9
          if (i==19) nodes_Rdfn(i) = 10
          
          if (i==20) nodes_Rdfn(i) = 24
          if (i==21) nodes_Rdfn(i) = 19
          if (i==22) nodes_Rdfn(i) = 20
          
          if (i==23) nodes_Rdfn(i) = 22
          if (i==24) nodes_Rdfn(i) = 23
          
       elseif (i.gt.24) then
          nodes_Rdfn(i) = i
       endif
       
       write(unit=305,fmt=*)"Rdfnd node is =",nodes_Rdfn(i), "for node old =",i
       
    enddo

    close(305)
    
    
  end subroutine get_rdfnNodes1
  
  subroutine get_rdfnNodes(nodes_Rdfn)
    implicit none
    integer, intent(inout) :: nodes_Rdfn(1:N_node)
    
    integer :: i
    integer :: CurrNvsl,PrevNvsl
    integer :: CurrNvsr,PrevNvsr
    integer :: CurrNnl,PrevNnl
    integer :: CurrNnr,PrevNnr
    integer :: cnlr,PrevCnlr
    integer :: PrevNnCntrlTop
    
    open(unit=305,file='nodesRdfnRwrt.dat')

    CurrNvsl = nvsl ; PrevNvsl = nvsl+1
    CurrNvsr = nvsr ; PrevNvsr = nvsr+1

    CurrNnl = 2*CurrNvsl ; PrevNnl = 2*PrevNvsl 
    CurrNnr = 2*CurrNvsr ; PrevNnr = 2*PrevNvsr

    cnlr = CurrNnl+CurrNnr
    PrevCnlr = PrevNnl + PrevNnr
    
    PrevNnCntrlTop = 2*PrevNvsl + 2*PrevNvsr + clft_top_node + crght_top_node
    
    do i = 1,N_node
       
       if (i.le.CurrNnl) then
          nodes_Rdfn(i) = i 
       elseif (i.gt.CurrNnl .and. i.le.cnlr) then
          nodes_Rdfn(i) = i+2 !2 is dueto 1 vs is chopped off  
       elseif (i.gt.cnlr .and. i.le.PrevNnCntrlTop) then
          
          if (i==(cnlr+1)) nodes_Rdfn(i) = PrevCnlr+1
          if (i==(cnlr+2)) nodes_Rdfn(i) = PrevNnl-1
          if (i==(cnlr+3)) nodes_Rdfn(i) = PrevNnl
          
          if (i==(cnlr+4)) nodes_Rdfn(i) = PrevCnlr+clft_top_node+1
          if (i==(cnlr+5)) nodes_Rdfn(i) = PrevCnlr-1
          if (i==(cnlr+6)) nodes_Rdfn(i) = PrevCnlr
          
          if (i==(PrevNnCntrlTop-3)) nodes_Rdfn(i) = PrevCnlr+2
          if (i==(PrevNnCntrlTop-2)) nodes_Rdfn(i) = PrevCnlr+clft_top_node
          if (i==(PrevNnCntrlTop-1)) nodes_Rdfn(i) = i
          if (i==(PrevNnCntrlTop-0)) nodes_Rdfn(i) = i
          
       elseif (i.gt.PrevNnCntrlTop) then
          nodes_Rdfn(i) = i
       endif
       
       write(unit=305,fmt=*)"Rdfnd node is =",nodes_Rdfn(i), "for node old =",i
    enddo

    close(305)

  end subroutine get_rdfnNodes
  
  
  subroutine save_nodeXY
    implicit none
    integer :: i,j
    
    do i = 1,N_node
       do j = 1,N_dmnsn
          nodeXY_Saved(i,j) = node_xy(i,j)
       enddo
    enddo
    
  end subroutine save_nodeXY
  
  
  subroutine demolish_spring_and_redefine_system(sprNo)
    implicit none
    integer, intent(in) :: sprNo
    
    sprDem = sprNo
    
    call store_SysVars_and_Arrays_wwo_dmlsh_spr ! 0 call inside
    call get_Sysvars_dmlsh_spr ! 0 call inside
    call deallocate_and_reallocate_arrays_wwo_dmlsh_spr ! 4 calls inside
    
    call get_NodeVars_wwo_dmlsh_spr
    call get_SprVars_dmlsh_spr
    call get_Cgvars_wwo_dmlsh_spr
    call get_kphi_and_nodePhiTyp_wwo_dmlsh_spr
    
    call store_all_moving_coordnte_variables
    call deallocate_moving_coordnte_variables_wo_StrVars 
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call store_DS_trnsfrms
    call deallocate_and_reallocate_DS_trnsfrmVars
    call readjst_all_trnsfrms
    
    call store_all_gradient_variables
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
    write(*,*) "FINALLY CAME HERE IC"
    
  end subroutine demolish_spring_and_redefine_system
  
  subroutine adding_node_inIC_ApclMem_and_redefine_system
    implicit none
    integer :: cnt
    
    write(*,*) N_mvCoordnte,"N_mvCoordnte Bfr"
    
    call store_SysVars_and_Arrays_adding_node_inIC_ApclMem
    call get_Sysvars_addingNode_inIC_ApclMem
    call deallocate_and_reallocate_arrays_wwo_dmlsh_spr ! 4 calls inside
    
    call get_NodeVars_adding_node_inIC_ApclMem
    call get_SprVars_adding_node_inIC_ApclMem
    call get_CgVars_adding_node_inIC_ApclMem
    call get_kphi_and_nodePhiTyp_adding_node_inIC_ApclMem
    
    call store_all_moving_coordnte_variables
    call deallocate_moving_coordnte_variables_wo_StrVars 
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call store_DS_trnsfrms
    call deallocate_and_reallocate_adding_node_inIC_ApclMem_trnsfrmVars
    call readjst_all_trnsfrms_inIC_ApclMem
    
    call store_all_gradient_variables
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
    write(*,*) "REDEFINING inIC_Apcl"
    
  end subroutine adding_node_inIC_ApclMem_and_redefine_system
  
  
  subroutine combining_cntrl_and_neighApcl_sprs_and_redefine_system_aft_CM2 !CM2=CellsMeet2
    implicit none
    real*8 :: E,Ea,Es,Eg
    real*8 :: Es1,Ea1,Eg1,Eb1
    
    ! ############################################################################################
    ! The following deallocate and allocate routines are NEEDED ONLY if
    ! you have multiple REDEFINE_systems in the same simulation procedures
    
    call deallocate_moving_coordnte_variables
    call get_all_moving_coordnte_variables
    call deallocate_and_reallocate_transfrmStr_variables
    call deallocate_all_gradient_variables_StrVars
    call get_all_gradient_variables_StrVars
    
    ! ###########################################################################################
    
    call store_SysVars_and_Arrays_combining_cntrl_and_neighApcl_sprs
    call get_Sysvars_combining_cntrl_and_neighApcl_sprs
    call deallocate_and_reallocate_arrays_wwo_dmlsh_spr ! 4 calls inside
    
    call get_NodeVars_combining_cntrl_and_neighApcl_sprs
    call get_SprVars_combining_cntrl_and_neighApcl_sprs
    call get_CgVars_combining_cntrl_and_neighApcl_sprs
    call get_kphi_and_nodePhiTyp_combining_cntrl_and_neighApcl_sprs
    
    call store_all_moving_coordnte_variables
    call deallocate_moving_coordnte_variables_wo_StrVars
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call store_DS_trnsfrms
    call deallocate_and_reallocate_combining_cntrl_and_neighApcl_sprs
    call readjst_all_trnsfrms_frm_reading_cellsmeet_file
    
    call store_all_gradient_variables
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
    write(*,*) node_xy(21,1:2),"21_1"
    write(*,*) node_xy(138,1:2),"138_1"
    
    
    E = Energy(node_xy,l0,A0)
    write(*,*) E,"E1"
    
    Es=0.0d0 ; Ea=0.0d0 ; Eg=0.0d0
    
    do i = 1,N_spr
       Es = Es + k_spr(i)*(l(i)-l0(i))**2
       write(*,*) i,k_spr(i),l(i),l0(i),Es,"Es"
    enddo
    
    do i = 1,N_cell
       Ea = Ea + k_area(i)*(A(i)-A0(i))**2
       write(*,*) i,k_area(i),A(i),A0(i),Ea,"Ea"
    enddo
    
    do i = 1,N_node
       Eg = Eg + CgXNode(i)*(node_xy(i,1)) + CgYNode(i)*(node_xy(i,2))
       write(*,*) i,CgXNode(i),CgYNode(i),node_xy(i,1:2),Eg,"Eg"
    enddo 
    
    Es1 = spr_E(node_xy,l0)
    Ea1 = area_E(node_xy,A0)
    Eg1 = grvtnl_E(node_xy)
    Eb1 = bend_E(node_xy)
    
    write(*,*) Es1,Ea1,Eg1,Eb1,"Es1-Ea1-Eg1-Eb1"
    call sleep(2)
    E = Energy(node_xy,l0,A0)
    write(*,*) E,"E2"
    
    call Find_Analytical_and_Numerical_Mismatch
    !write(*,*) "stopped aft mismatch chk"
    !stop
    
  end subroutine combining_cntrl_and_neighApcl_sprs_and_redefine_system_aft_CM2
  
  
  subroutine combining_cntrl_and_neighApcl_sprs_and_redefine_system_aft_CM1
    implicit none
    real*8 :: E,Ea,Es,Eg
    real*8 :: Ea1,Es1,Eg1,Eb1
    
    ! ############################################################################################
    ! The following deallocate and allocate routines are NEEDED ONLY if
    ! you have multiple REDEFINE_systems in the same simulation procedures
    
    Es1 = spr_E(node_xy,l0)
    Ea1 = area_E(node_xy,A0)
    Eg1 = grvtnl_E(node_xy)
    Eb1 = bend_E(node_xy)
    
    write(*,*) Es1,Ea1,Eg1,Eb1,"Es1-Ea1-Eg1-Eb1 bfr restrcting"
    
    Es=0.0d0 ; Ea=0.0d0 ; Eg=0.0d0
    
    do i = 1,N_spr
       if (i==1) write(*,*) Es,"Es begin"
       Es = Es + 0.50d0*k_spr(i)*(l(i)-l0(i))**2
       write(*,*) i,k_spr(i),l(i),l0(i),Es,"Es bfr restrcting"
    enddo
    
    do i = 1,N_cell
       Ea = Ea + 0.50d0*k_area(i)*(A(i)-A0(i))**2
       write(*,*) i,k_area(i),A(i),A0(i),Ea,"Ea bfr restrcting"
    enddo
    
    do i = 1,N_node
       Eg = Eg + CgXNode(i)*(node_xy(i,1)) + CgYNode(i)*(node_xy(i,2))
       write(*,*) i,CgXNode(i),CgYNode(i),node_xy(i,1:2),Eg,"Eg bfr restrcting"
    enddo 
    
    
    call deallocate_moving_coordnte_variables
    call get_all_moving_coordnte_variables
    call deallocate_and_reallocate_transfrmStr_variables
    call deallocate_all_gradient_variables_StrVars
    call get_all_gradient_variables_StrVars
    
    ! ###########################################################################################
    
    call store_SysVars_and_Arrays_combining_cntrl_and_neighApcl_sprs
    call get_Sysvars_combining_cntrl_and_neighApcl_sprs
    call deallocate_and_reallocate_arrays_wwo_dmlsh_spr ! 4 calls inside
    
    call get_NodeVars_combining_cntrl_and_neighApcl_sprs
    call get_SprVars_combining_cntrl_and_neighApcl_sprs
    call get_CgVars_combining_cntrl_and_neighApcl_sprs
    call get_kphi_and_nodePhiTyp_combining_cntrl_and_neighApcl_sprs
    
    call store_all_moving_coordnte_variables
    call deallocate_moving_coordnte_variables_wo_StrVars
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call store_DS_trnsfrms
    call deallocate_and_reallocate_combining_cntrl_and_neighApcl_sprs
    call readjst_all_trnsfrms_frm_reading_cellsmeet_file
    
    call store_all_gradient_variables
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
    !write(*,*) node_xy(21,1:2),"21_1"
    !write(*,*) node_xy(138,1:2),"138_1"
    
    E = Energy(node_xy,l0,A0)
    write(*,*) E,"E1"
    
    
    Es=0.0d0 ; Ea=0.0d0 ; Eg=0.0d0
    
    do i = 1,N_spr
       if (i==1) write(*,*) Es,"Es begin"
       Es = Es + 0.50d0*k_spr(i)*(l(i)-l0(i))**2
       write(*,*) i,k_spr(i),l(i),l0(i),Es,"Es aft E1"
    enddo
    
    do i = 1,N_cell
       Ea = Ea + 0.50d0*k_area(i)*(A(i)-A0(i))**2
       write(*,*) i,k_area(i),A(i),A0(i),Ea,"Ea aft E1"
    enddo
    
    do i = 1,N_node
       Eg = Eg + CgXNode(i)*(node_xy(i,1)) + CgYNode(i)*(node_xy(i,2))
       write(*,*) i,CgXNode(i),CgYNode(i),node_xy(i,1:2),Eg,"Eg aft E1"
    enddo 

    Es1 = spr_E(node_xy,l0)
    Ea1 = area_E(node_xy,A0)
    Eg1 = grvtnl_E(node_xy)
    Eb1 = bend_E(node_xy)
    
    write(*,*) Es1,Ea1,Eg1,Eb1,"Es1-Ea1-Eg1-Eb1 bfr E2"
    
    
    E = Energy(node_xy,l0,A0)
    write(*,*) E,"E2"
    
    call Find_Analytical_and_Numerical_Mismatch
    !write(*,*) "stopped aft mismatch chk"
    
  end subroutine combining_cntrl_and_neighApcl_sprs_and_redefine_system_aft_CM1
  
  
  subroutine redefine_system_wo_demolishSpr
    implicit none
    
    call deallocate_and_reallocate_StrVars
    call store_SysVars_and_Arrays_wwo_dmlsh_spr
    call deallocate_and_reallocate_arrays_wwo_dmlsh_spr
    
    call get_NodeVars_wwo_dmlsh_spr
    call get_SprVars_wo_demlsh_spr
    call get_Cgvars_wwo_dmlsh_spr
    call get_kphi_and_nodePhiTyp_wwo_dmlsh_spr
    
    call store_all_moving_coordnte_variables
    call deallocate_moving_coordnte_variables_wo_StrVars 
    call get_all_moving_coordnte_variables_wo_StrVars
    call nodes_to_coordntes(node_xy,coordntes_xy)
    
    call store_DS_trnsfrms
    call deallocate_and_reallocate_DS_trnsfrmVars
    call readjst_all_trnsfrms_wo_dmlsh
    
    call store_all_gradient_variables
    call deallocate_all_gradient_variables_wo_StrVars
    call get_all_gradient_variables_wo_StrVars
    
    write(*,*) "FINALLY CAME HERE WO"
    
  end subroutine redefine_system_wo_demolishSpr
  
  
  
  subroutine deallocate_and_reallocate_StrVars
    implicit none
    
    deallocate(node_xyStr,node_typStr,count_this_dnStr)
    deallocate(node_cnnctdStr,double_nodeStr)
    
    deallocate(typ_sprStr,k_sprStr,l0_Str,l_Str)
    deallocate(nodePhi_typStr,k_phiStr,CgXNode_Str)
    deallocate(coordntes_xyStr)
    
    allocate(node_xyStr(1:N_node,1:N_dmnsn))
    allocate(node_typStr(1:N_node),count_this_dnStr(1:N_node))
    allocate(node_cnnctdStr(1:N_node),double_nodeStr(1:N_node,1:N_dmnsn))
    
    allocate(typ_sprStr(1:N_spr),k_sprStr(1:N_spr))
    allocate(l0_Str(1:N_spr),l_str(1:N_spr))
    
    allocate(nodePhi_typStr(1:N_node),k_phiStr(1:N_node,1:max_Phi_node))
    allocate(CgXNode_Str(1:N_node))
    allocate(coordntes_xyStr(1:N_mvCoordnte))
    
    node_xyStr       = -1.0d20  ; node_typStr    = -1
    count_this_dnStr = -1       ; node_cnnctdStr =  0
    double_nodeStr   = -1

    typ_sprStr  = -1      ; k_sprStr    = -1.0d20
    l0_Str      = -1.0d25 ; l_Str       = -1.0d25
    
    nodePhi_typStr  = -10     ; k_phiStr = -1.0d30
    CgXNode_Str     = -1.0d30
    coordntes_xyStr = -1.0d30
    
  end subroutine deallocate_and_reallocate_StrVars
  
  subroutine store_SysVars_and_Arrays_wwo_dmlsh_spr
    implicit none
    
    node_xyStr       = node_xy
    node_typStr      = node_typ   
    node_cnnctdStr   = node_cnnctd 
    double_nodeStr   = double_node
    count_this_dnStr = count_this_dn
    
    typ_sprStr = typ_spr
    k_sprStr   = k_spr   
    l0_Str     = l0 
    l_Str      = l
    
    nodePhi_typStr = nodePhi_typ
    k_phiStr       = k_phi
    CgXNode_Str     = CgXNode
    
    N_nodeS = N_node
    N_sprS  = N_spr
    N_phiS  = N_phi
    
    coordntes_xyStr = coordntes_xy
    
  end subroutine store_SysVars_and_Arrays_wwo_dmlsh_spr
  
  subroutine store_SysVars_and_Arrays_adding_node_inIC_ApclMem
    implicit none ! AN = Adding Node
    
    node_xyStrAN       = node_xy
    node_typStrAN      = node_typ   
    node_cnnctdStrAN   = node_cnnctd 
    double_nodeStrAN   = double_node
    count_this_dnStrAN = count_this_dn
    
    typ_sprStrAN = typ_spr
    k_sprStrAN   = k_spr   
    l0_StrAN     = l0 
    l_StrAN      = l
    
    nodePhi_typStrAN = nodePhi_typ
    k_phiStrAN       = k_phi
    CgXNode_StrAN     = CgXNode
    
    N_nodeSAN = N_node
    N_sprSAN  = N_spr
    N_phiSAN  = N_phi
    
    coordntes_xyStrAN = coordntes_xy
    
  end subroutine store_SysVars_and_Arrays_adding_node_inIC_ApclMem
  
  
  subroutine store_SysVars_and_Arrays_combining_cntrl_and_neighApcl_sprs
    implicit none
    
    node_xyStrES       = node_xy
    node_typStrES      = node_typ   
    node_cnnctdStrES   = node_cnnctd 
    double_nodeStrES   = double_node
    count_this_dnStrES = count_this_dn
    
    typ_sprStrES = typ_spr
    k_sprStrES   = k_spr
    l0_StrES     = l0 
    l_StrES      = l
    
    nodePhi_typStrES = nodePhi_typ
    k_phiStrES       = k_phi
    CgXNode_StrES    = CgXNode
    CgYNode_StrES    = CgYNode
    
    N_nodeSES = N_node
    N_sprSES  = N_spr
    N_phiSES  = N_phi
    
    coordntes_xyStrES = coordntes_xy
    
    write(*,*) modelID,"modelID in store"
    
  end subroutine store_SysVars_and_Arrays_combining_cntrl_and_neighApcl_sprs
  
  subroutine get_Sysvars_dmlsh_spr
    implicit none
    
    if (stageNo==1 .and. stageType==1) then
       N_node = N_node+1 !only from S1T1 to S2T1
       N_spr  = N_spr-1  !only from S1T1 to S2T1
    else
       write(*,*) "fl:redefine_sys,sb:get_Sysvars_dmlsh_spr"
    endif
    
  end subroutine get_Sysvars_dmlsh_spr
  
  subroutine get_Sysvars_addingNode_inIC_ApclMem
    implicit none
    N_node = N_node+2
  end subroutine get_Sysvars_addingNode_inIC_ApclMem
  
  subroutine get_Sysvars_combining_cntrl_and_neighApcl_sprs
    implicit none
    
    N_node = N_node-1
    N_spr  = N_spr-1  
    
  end subroutine get_Sysvars_combining_cntrl_and_neighApcl_sprs
  
  subroutine deallocate_and_reallocate_arrays_wwo_dmlsh_spr !wwo=with or without
    implicit none
    
    deallocate(node_xy,node_typ,node_cnnctd,double_node,count_this_dn)
    deallocate(typ_spr,k_spr,l0,l)
    deallocate(nodePhi_typ,k_phi,CgXNode,CgYNode)
    
    call allocate_and_initialize_node_variables_wo_StrVars
    call allocate_and_initialize_spring_variables_wo_StrVars
    call allocate_and_initialize_grvVars_wo_StrVars
    call allocate_and_initialize_bend_variables_wo_StrVars
    
  end subroutine deallocate_and_reallocate_arrays_wwo_dmlsh_spr
  
  
  subroutine get_NodeVars_wwo_dmlsh_spr
    implicit none
    integer :: i
    integer :: lft_nodeAct,rght_nodeAct 
    integer :: lim1,lim2,lim3,lim4
    
    if (dmlshDecsn==1) then
       node_xy(1:N_nodeS,1:N_dmnsn) = node_xyStr(1:N_nodeS,1:N_dmnsn)
       write(*,*) origin(1:2) , "origin"
       node_xy(N_nodeS+1,1:N_dmnsn) = origin(1:2)
       
    elseif (dmlshDecsn==0) then
       node_xy(1:N_nodeS,1:N_dmnsn) = node_xyStr(1:N_nodeS,1:N_dmnsn)
    endif
    
    
    lft_nodeAct  = lft_node-2*CellsMeet  !I am not changing presuming nothing changes
    rght_nodeAct = rght_node-2*CellsMeet !no need of dmlshDecsn
    
    lim1 = lft_nodeAct
    lim2 = lft_node
    lim3 = lft_node+rght_nodeAct
    lim4 = lft_node+rght_node
    
    write(*,*) lft_nodeAct,lft_node,rght_nodeAct,rght_node,"nodes"
    write(*,*) dmlshDecsn,lim1,lim2,lim3,lim4,"lims"
    
    do i = 1,N_node
       
       if (i.le.lim1) then
          node_typ(i) = node_typStr(i)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          node_typ(i) = 1
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          node_typ(i) = node_typStr(i)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          node_typ(i) = 1
          
       elseif (i.gt.lim4) then
          node_typ(i) = 0
          
          if (i.ne.N_node) write(*,*) i,N_node,"fl:redefine_sys,sb:rdf_nodeTyp"
          if (i.ne.N_node) stop
          
       endif   
       
    enddo
    
    lft_endNode(0)=2 ; rght_endNode(0)=2
    
    lft_endNode(1)  = lft_endNode(1)-2
    lft_endNode(2)  = lft_endNode(2)-2
    rght_endNode(1) = rght_endNode(1)-2
    rght_endNode(2) = rght_endNode(2)-2
    
    call get_list_of_double_nodes_method2
    call nodes_cnnctd_and_count_this_dn
    call print_NodeVars
    
  end subroutine get_NodeVars_wwo_dmlsh_spr
  
  subroutine get_NodeVars_adding_node_inIC_ApclMem
    implicit none
    integer :: lim1,lim2,lim3,lim4,lim5
    integer :: i,j
    integer :: lftsideTopN,rghtsideTopN
    
    lim1 = (Hlf_Ncell)*(2)    ! =22
    lim2 = lim1+2             ! =24
    lim3 = lim2+(Hlf_Ncell*2) ! =46
    lim4 = lim3+2             ! =48
    lim5 = lim4+2             ! =50
    
    write(*,*) lim1,lim2,lim3,lim4,lim5,"lims inside get_NodeVars_adding_node_inIC_ApclMem"
    
    ydis_frmVitlnMem = 0.20d0
    write(*,*) ydis_frmVitlnMem,"ydis"
    
    open(unit=382,file='addingNodeIC_nodeVarschk.dat')
    
    do i = 1,N_node
       
       if (i.le.lim1) then ! (1-22)
          
          node_xy(i,1:N_dmnsn) = node_xyStrAN(i,1:N_dmnsn)
          node_typ(i)          = node_typStrAN(i)
          
       elseif ((i.gt.lim1) .and. (i.le.lim2)) then ! (23-24)
          
          if ((i-lim1)==1) then ! 23
             node_xy(i,1) = node_xyStrAN(i,1)
             node_xy(i,2) = node_xyStrAN(i,2) - ydis_frmVitlnMem
             node_typ(i)  = 1 ! free
             
          elseif ((i-lim1)==2) then ! 24
             node_xy(i,1:N_dmnsn) = node_xyStrAN(i,1:N_dmnsn)
             node_typ(i)          = node_typStrAN(i)
          endif
          
       elseif ((i.gt.lim2) .and. (i.le.lim3)) then ! (25-46)
          node_xy(i,1:N_dmnsn) = node_xyStrAN(i,1:N_dmnsn)
          node_typ(i)          = node_typStrAN(i)
          
       elseif ((i.gt.lim3) .and. (i.le.lim4)) then ! (47-48)
          
          if ((i-lim3)==1) then ! 47
             node_xy(i,1) = node_xyStrAN(i,1)
             node_xy(i,2) = node_xyStrAN(i,2) - ydis_frmVitlnMem
             node_typ(i)  = 1 ! free
             
          elseif ((i-lim3)==2) then ! 48
             node_xy(i,1:N_dmnsn) = node_xyStrAN(i,1:N_dmnsn)
             node_typ(i)          = node_typStrAN(i)
          endif
          
       elseif ((i.gt.lim4) .and. (i.le.lim5)) then ! (49-50)
          
          if ((i-lim4)==1) then
             
             lftsideTopN          = (Hlf_Ncell)*2 + 1
             node_xy(i,1:N_dmnsn) = node_xyStrAN(lftsideTopN,1:N_dmnsn)
             node_typ(i)          = node_typStrAN(lftsideTopN)
             
          elseif ((i-lim4)==2) then
             
             rghtsideTopN         = (Hlf_Ncell+1)*2 + (Hlf_Ncell)*2 + 1 
             node_xy(i,1:N_dmnsn) = node_xyStrAN(rghtsideTopN,1:N_dmnsn)
             node_typ(i)          = node_typStrAN(rghtsideTopN)
          endif
          
       endif
       
       write(382,*) i,node_xy(i,1:N_dmnsn),node_typ(i)
       
    enddo
    
    close(382)
    
    call get_list_of_double_nodes_method2
    call nodes_cnnctd_and_count_this_dn
    call print_NodeVars
    
  end subroutine get_NodeVars_adding_node_inIC_ApclMem
  
  
  subroutine get_NodeVars_combining_cntrl_and_neighApcl_sprs
    implicit none
    integer :: lim1,lim2,lim3,lim4,lim5,lim6,lim7,lim8
    integer :: i,j
    integer :: tbdn1,tbdn2 ! to be double node 1 and 2 
    
    ydis_aftCmbingSpr = 0.05d0
    
    if (CellsMeet==1) then
       
       lim1 = (Hlf_Ncell-1)*2 + 2 ! =22
       lim2 = lim1 + 2            ! =24
       
       lim3 = lim2 + lim1         ! =46
       lim4 = lim3 + 2            ! =48
       
       lim5 = lim4 + 1
       lim6 = N_node
       
       tbdn1 = lim1+1 ; tbdn2 = lim3+1
       write(*,*) tbdn1,tbdn2,"tbdn1 and tbdn2"
       write(*,*) origin(1:2),"origin_Val"
       
       do i = 1,N_node
          
          if (i.le.lim1) then ! (1~22)
             
             node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
             node_typ(i)          = node_typStrES(i)
             
          elseif ((i.gt.lim1) .and. (i.le.lim2)) then ! (23~24)
             
             if ((i-lim1)==1) then ! 23
                node_xy(i,1) = origin(1)
                node_xy(i,2) = (node_xyStrES(tbdn1,2) + node_xyStrES(tbdn2,2))*(0.50d0)
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim1)==2) then ! 24
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
             
          elseif ((i.gt.lim2) .and. (i.le.lim3)) then ! (25~46)
             
             node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
             node_typ(i)          = node_typStrES(i)
             
          elseif ((i.gt.lim3) .and. (i.le.lim4)) then 
             
             if ((i-lim3)==1) then ! 47
                node_xy(i,1) = origin(1)
                node_xy(i,2) = node_xy(tbdn1,2) 
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim3)==2) then ! 48
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
             
          elseif ((i.gt.lim4) .and. (i.le.lim5)) then
             
             node_xy(i,1:N_dmnsn) = origin(1:N_dmnsn)
             node_typ(i)          = 0 ! fixed node
             
          elseif ((i.gt.lim5) .and. (i.le.lim6)) then
              node_xy(i,1:N_dmnsn) = node_xyStrES(i+1,1:N_dmnsn)
              node_typ(i)          = node_typStrES(i+1)
          endif
          
       enddo
       
    elseif (CellsMeet==2) then
       
       lim1 = (Hlf_Ncell-2)*2 + 2 ! =20
       lim2 = lim1 + 2            ! =22
       lim3 = lim2 + 2            ! =24
       
       lim4 = lim3 + lim1         ! =44
       lim5 = lim4 + 2            ! =46
       lim6 = lim5 + 2            ! =48
       
       lim7  = lim6 + 1           ! =49
       lim8  = N_node             ! =139
       
       tbdn1 = lim2+1 ; tbdn2 = lim5+1
       write(*,*) tbdn1,tbdn2,"tbdn1 and tbdn2"
       write(*,*) origin(1:2),"origin_Val"
       
       do i = 1,N_node
       
          if (i.le.lim1) then ! (1~20)
             
             node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
             node_typ(i)          = node_typStrES(i)
             
          elseif ((i.gt.lim1) .and. (i.le.lim2)) then ! (21~22)
             
             if ((i-lim1)==1) then ! 21 
                node_xy(i,1) = origin(1)
                node_xy(i,2) = origin(2) - ydis_aftCmbingSpr
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim1)==2) then ! 22
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
          
          elseif ((i.gt.lim2) .and. (i.le.lim3)) then ! (23~24)
             
             if ((i-lim2)==1) then ! 23
                node_xy(i,1) = origin(1)
                node_xy(i,2) = (node_xyStrES(tbdn1,2) + node_xyStrES(tbdn2,2))*(0.50d0)
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim2)==2) then ! 24
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
          
          elseif ((i.gt.lim3) .and. (i.le.lim4)) then ! (25~44)
             
             node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
             node_typ(i)          = node_typStrES(i)
             
          elseif ((i.gt.lim4) .and. (i.le.lim5)) then ! (45~46)
             
             if ((i-lim4)==1) then ! 45 
                node_xy(i,1) = origin(1)
                node_xy(i,2) = origin(2) - ydis_aftCmbingSpr
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim4)==2) then ! 46
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
             
             
          elseif ((i.gt.lim5) .and. (i.le.lim6)) then
             
             
             if ((i-lim5)==1) then ! 47
                node_xy(i,1) = origin(1)
                node_xy(i,2) = node_xy(tbdn1,2) 
                node_typ(i)  = 1 ! free
                
             elseif ((i-lim5)==2) then ! 48
                node_xy(i,1:N_dmnsn) = node_xyStrES(i,1:N_dmnsn)
                node_typ(i)          = node_typStrES(i)
             endif
             
          elseif ((i.gt.lim6) .and. (i.le.lim7)) then ! 49
             
             node_xy(i,1:N_dmnsn) = origin(1:N_dmnsn)
             node_typ(i)          = 0 ! fixed node
             
          elseif ((i.gt.lim7) .and. (i.le.lim8)) then ! (50~Rest)
             
             node_xy(i,1:N_dmnsn) = node_xyStrES(i+1,1:N_dmnsn)
             node_typ(i)          = node_typStrES(i+1)
          endif
          
       enddo
       
    endif
    
    call get_list_of_double_nodes_method2
    call nodes_cnnctd_and_count_this_dn
    call print_NodeVars
    
  end subroutine get_NodeVars_combining_cntrl_and_neighApcl_sprs
  
  subroutine get_SprVars_dmlsh_spr
    implicit none
    integer :: i,count_spr
    
    count_spr=0
    
    do i = 1,N_sprS
       
       if (i.lt.sprDem) then
          
          count_spr          = count_spr+1
          typ_spr(count_spr) = typ_sprStr(i)
          k_spr(count_spr)   = k_sprStr(i)
          l0(count_spr)      = l0_Str(i)
          l(count_spr)       = l_Str(i)
          
       elseif (i==SprDem) then
          continue
          
       elseif (i.gt.SprDem) then
          
          count_spr          = count_spr+1
          typ_spr(count_spr) = typ_sprStr(i)
          k_spr(count_spr)   = k_sprStr(i)
          l0(count_spr)      = l0_Str(i)
          l(count_spr)       = l_Str(i)
          
       endif
       
       if (count_spr==(lft_endSpring(1)-2) .or. count_spr==(rght_endSpring(1)-2)) then
          write(*,*) lft_endSpring(1),rght_endSpring(1)
          typ_spr(count_spr) = 6
       endif
       
       !write(*,*) typ_spr(count_spr),k_spr(count_spr),l0(count_spr),count_spr,N_sprS,"sprProp"
    enddo
    
    write(*,*) "Adjust l of invaginating sprs, adding length (dist_to_pulley+dwnwrds dist)"
    call sleep(1)
    
  end subroutine get_SprVars_dmlsh_spr
  
  subroutine get_SprVars_wo_demlsh_spr
    implicit none
    integer :: meetingApclSprlft,meetingApclSprRght
    integer :: PrVmeetingApclSprlft,PrVmeetingApclSprRght
    
    typ_spr(1:N_sprS) = typ_sprStr(1:N_sprS)
    k_spr(1:N_sprS)   = k_sprStr(1:N_sprS)
    l0(1:N_sprS)      = l0_Str(1:N_sprS)
    l(1:N_sprS)       = l_Str(1:N_sprS)
    
    meetingApclSprlft     = lft_endSpring(1)  -2-(CellsMeet-1)*nsecl
    meetingApclSprRght    = rght_endSpring(1) -2-(CellsMeet-1)*nsecr
    PrVmeetingApclSprlft  = lft_endSpring(1)  -2-(CellsMeet-2)*nsecl
    PrVmeetingApclSprrght = rght_endSpring(1) -2-(CellsMeet-2)*nsecl
    
    typ_spr(meetingApclSprlft)  = 6
    typ_spr(meetingApclSprRght) = 6
    
    typ_spr(PrVmeetingApclSprlft)  = 3
    typ_spr(PrVmeetingApclSprRght) = 3
    
    write(*,*) meetingApclSprlft,meetingApclSprRght,"meetCell"
    write(*,*) PrVmeetingApclSprlft,PrVmeetingApclSprRght,"PrVmeetCell"
    
    !do i = 1,N_spr
     !  write(*,*) k_spr(i),l0(i),typ_spr(i),i,"sprProp"
    !enddo
    
  end subroutine get_SprVars_wo_demlsh_spr
  
  subroutine get_SprVars_adding_node_inIC_ApclMem
    implicit none
    
    ! nothing_changes in this case
    
    write(*,*) N_spr,N_sprSAN
    if (N_spr.ne.N_sprSAN) then
       write(*,*) "N_spr =/ N_sprSAN"
       stop
    endif
    
    typ_spr(1:N_sprSAN) = typ_sprStrAN(1:N_sprSAN)
    k_spr(1:N_sprSAN)   = k_sprStrAN(1:N_sprSAN)
    l0(1:N_sprSAN)      = l0_StrAN(1:N_sprSAN)
    l(1:N_sprSAN)       = l_StrAN(1:N_sprSAN)
    
  end subroutine get_SprVars_adding_node_inIC_ApclMem
  
  subroutine get_SprVars_combining_cntrl_and_neighApcl_sprs
    implicit none
    integer :: i,count_spr
    integer :: lim1,lim2,lim3,lim4
    integer :: nsprsInACell
    integer :: unChngdCellL,unChngdCellR
    real*8  :: kval(1:2),lval(1:2),l0val(1:2)
    
    if (modelID==1) then
       write(*,*) "modelID should be 2 instead of",modelID
       stop
    endif
    
    nsprsInACell = (NAEC_Apcl+1)+(NAEC_Bsal+1)+(NAEC_Ltrl+1)
    unChngdCellL = Hlf_Ncell-1 ; unChngdCellR = (unChngdCellL)+(Hlf_Ncell-1)
    count_spr    = 0
    
    write(*,*) nsprsInACell,unChngdCellL,unChngdCellR,"get_SprVars in combining_cntrl_and_nei"
    write(*,*) count_spr,"count_spr"
    
    sprDemForCmbning = (N_cell-1)*(nsprsInACell)+1; write(*,*) sprDemForCmbning,"sprDemForCmb"
    
    call get_spr_prp_aft_cmbining_spr(kval,lval,l0val)
    
    lim1 = (unChngdCellL*nsprsInACell)
    lim2 = lim1 + nsprsInACell
    lim3 = lim2 + lim1
    lim4 = lim3 + nsprsInACell
    
    write(*,*) N_sprSES,"SES"
    
    do i = 1,N_sprSES
       
       if (i.le.lim1) then
          
          count_spr          = count_spr+1
          typ_spr(count_spr) = typ_sprStrES(i)
          k_spr(count_spr)   = k_sprStrES(i)
          l0(count_spr)      = l0_StrES(i)
          l(count_spr)       = l_StrES(i)
          
       elseif ((i.gt.lim1) .and. (i.le.lim2)) then
          
          if ((i-lim1)==1) then
             
             count_spr          = count_spr+1
             typ_spr(count_spr) = typ_sprStrES(i)
             k_spr(count_spr)   = kval(1)
             l0(count_spr)      = l0val(1)
             l(count_spr)       = lval(1)
             
          elseif ((i-lim1).gt.1) then
             
             count_spr          = count_spr+1
             typ_spr(count_spr) = typ_sprStrES(i)
             k_spr(count_spr)   = k_sprStrES(i)
             l0(count_spr)      = l0_StrES(i)
             l(count_spr)       = l_StrES(i)
             
          endif
          
       elseif ((i.gt.lim2) .and. (i.le.lim3)) then
          
          count_spr          = count_spr+1
          typ_spr(count_spr) = typ_sprStrES(i)
          k_spr(count_spr)   = k_sprStrES(i)
          l0(count_spr)      = l0_StrES(i)
          l(count_spr)       = l_StrES(i)
          
       elseif ((i.gt.lim3) .and. (i.le.lim4)) then
          
          if ((i-lim3)==1) then
             
             count_spr          = count_spr+1
             typ_spr(count_spr) = typ_sprStrES(i)
             k_spr(count_spr)   = kval(2)
             l0(count_spr)      = l0val(2)
             l(count_spr)       = lval(2)
             
          elseif ((i-lim3).gt.1) then
             
             count_spr          = count_spr+1
             typ_spr(count_spr) = typ_sprStrES(i)
             k_spr(count_spr)   = k_sprStrES(i)
             l0(count_spr)      = l0_StrES(i)
             l(count_spr)       = l_StrES(i)
             
          endif
          
       elseif (i==SprDemForCmbning) then
          continue
          
       elseif ((i.gt.SprDemForCmbning) .and. (i.le.N_sprSES)) then
          
          count_spr          = count_spr+1
          typ_spr(count_spr) = typ_sprStrES(i)
          k_spr(count_spr)   = k_sprStrES(i)
          l0(count_spr)      = l0_StrES(i)
          l(count_spr)       = l_StrES(i)
          
       endif
       
    enddo
    
    call print_sprVars_wo_Lt_alpha_optmSpr
    
  end subroutine get_SprVars_combining_cntrl_and_neighApcl_sprs
  
  
  subroutine get_spr_prp_aft_cmbining_spr(kval,lval,l0val)
    implicit none
    real*8, intent(out) :: kval(1:2),lval(1:2),l0val(1:2)
    
    integer :: nsprsInACell,caseV
    integer :: sprL,sprC,sprR
    
    real*8  :: ks_L,ks_C,ks_R
    real*8  :: l0_L,l0_C,l0_R
    real*8  :: l_L ,l_C ,l_R
    
    real*8  :: ks_C1,ks_C2,ks_P,ks_P1,ks_P2
    real*8  :: l0_C1,l0_C2,l0_P,l0_P1,l0_P2
    real*8  :: l_C1 ,l_C2 ,l_P ,l_P1 ,l_P2
    
    real*8  :: ks_spltL(1:2),l0_spltL(1:2),l_spltL(1:2)
    real*8  :: ks_spltC(1:2),l0_spltC(1:2),l_spltC(1:2)
    real*8  :: ks_spltR(1:2),l0_spltR(1:2),l_spltR(1:2)
    
    integer             :: NsprToPr,NsprToCm
    real*8, allocatable :: ks_bcm1(:),l0_bcm1(:),l_bcm1(:)
    real*8, allocatable :: ks_bcm2(:),l0_bcm2(:),l_bcm2(:)
    real*8, allocatable :: ks_bcm3(:),l0_bcm3(:),l_bcm3(:)
    real*8, allocatable :: ks_bcm4(:),l0_bcm4(:),l_bcm4(:)
    
    real*8  :: ks_acm1,ks_acm2,ks_acm3,l0_acm1,l0_acm2,l0_acm3,l_acm1,l_acm2,l_acm3
    real*8  :: ks_acm4,l0_acm4,l_acm4
    integer :: choice
    
    open(unit=389,file='get_spr_cmbining.dat')
    
    nsprsInACell = (NAEC_Apcl+1+NAEC_Bsal+1+NAEC_Ltrl+1)
    
    sprL = (Hlf_Ncell-1)*(nsprsInACell) + 1
    sprC = (N_cell-1)*(nsprsInACell)    + 1
    sprR = (N_cell-2)*(nsprsInACell)    + 1
    
    write(*,*) sprL,sprC,sprR,"sprS bfr Cmbining"
    
    if (CellsMeet == 1) then
       
       write(389,*) k_sprStrES(sprL),k_sprStrES(sprC),k_sprStrES(sprR),"k_spr strES"
       write(389,*) l0_strES(sprL),l0_strES(sprC),l0_strES(sprR),"l0 strES"
       write(389,*) l_strES(sprL),l_strES(sprC),l_strES(sprR),"l strES"
       
       choice = 2
       call split_spr_srialy_with_ratio_tbd(sprL,choice,ks_spltL,l0_spltL,l_spltL) 
       call split_spr_srialy_equal_len(sprC,choice,ks_spltC,l0_spltC,l_spltC)
       call split_spr_srialy_with_ratio_tbd(sprR,choice,ks_spltR,l0_spltR,l_spltR)
       
       write(389,*) ks_spltL(1:2),ks_spltC(1:2),ks_spltR(1:2),"ks_splt L-C-R"
       write(389,*) l0_spltL(1:2),l0_spltC(1:2),l0_spltR(1:2),"l0_splt L-C-R"
       write(389,*) l_spltL(1:2), l_spltC(1:2), l_spltR(1:2), "l_splt L-C-R"
       
       NsprToPr = 2
       allocate(ks_bcm1(1:NsprToPr),l0_bcm1(1:NsprToPr),l_bcm1(1:NsprToPr))
       allocate(ks_bcm2(1:NsprToPr),l0_bcm2(1:NsprToPr),l_bcm2(1:NsprToPr))
       
       ks_bcm1=-1.0d30 ; l0_bcm1=-1.0d30 ; l_bcm1=-1.0d30
       ks_bcm2=-1.0d30 ; l0_bcm2=-1.0d30 ; l_bcm2=-1.0d30
       
       NsprToCm = 2
       caseV    = 1
       call get_ks_prop_bfr_comb(NsprToCm,caseV,ks_spltL,ks_spltC,ks_spltR,ks_bcm1)
       call get_l0_prop_bfr_comb(NsprToCm,caseV,l0_spltL,l0_spltC,l0_spltR,l0_bcm1)
       call get_l_prop_bfr_comb(NsprToCm, caseV,l_spltL ,l_spltC ,l_spltR ,l_bcm1 )
       
       write(389,*) " "
       write(389,*) ks_bcm1(1:2),l0_bcm1(1:2),l_bcm1(1:2),"bcm1"
       
       caseV = 2
       call get_ks_prop_bfr_comb(NsprToCm,caseV,ks_spltL,ks_spltC,ks_spltR,ks_bcm2)
       call get_l0_prop_bfr_comb(NsprToCm,caseV,l0_spltL,l0_spltC,l0_spltR,l0_bcm2)
       call get_l_prop_bfr_comb(NsprToCm, caseV,l_spltL ,l_spltC ,l_spltR ,l_bcm2 )
       
       write(389,*) ks_bcm2(1:2),l0_bcm2(1:2),l_bcm2(1:2),"bcm2"
       
       call cmbine_spr_prlelly_cnnctd(NsprToCm,ks_bcm1,l0_bcm1,l_bcm1,ks_acm1,l0_acm1,l_acm1)
       call cmbine_spr_prlelly_cnnctd(NsprToCm,ks_bcm2,l0_bcm2,l_bcm2,ks_acm2,l0_acm2,l_acm2)
       
       write(389,*) " "
       write(389,*) ks_acm1,l0_acm1,l_acm1,"acm1"
       write(389,*) ks_acm2,l0_acm2,l_acm2,"acm2"
       
       NsprToCm   = 2
       allocate(ks_bcm3(1:NsprToCm),l0_bcm3(1:NsprToCm),l_bcm3(1:NsprToCm))
       
       ks_bcm3(1) = ks_spltL(1) ; ks_bcm3(2) = ks_acm1
       l0_bcm3(1) = l0_spltL(1) ; l0_bcm3(2) = l0_acm1
       l_bcm3(1)  = l_spltL(1)  ; l_bcm3(2)  = l_acm1
       write(389,*) ks_bcm3(1:2),l0_bcm3(1:2),l_bcm3(1:2),"bcm3"
       
       call cmbine_spr_serially_cnnctd(NsprToCm,ks_bcm3,l0_bcm3,l_bcm3,ks_acm3,l0_acm3,l_acm3)
       write(389,*) ks_acm3,l0_acm3,l_acm3,"acm3"
       
       NsprToCm   = 2
       allocate(ks_bcm4(1:NsprToCm),l0_bcm4(1:NsprToCm),l_bcm4(1:NsprToCm))
       
       ks_bcm4(1) = ks_spltR(1) ; ks_bcm4(2) = ks_acm2
       l0_bcm4(1) = l0_spltR(1) ; l0_bcm4(2) = l0_acm2
       l_bcm4(1)  = l_spltR(1)  ; l_bcm4(2)  = l_acm2
       write(389,*) ks_bcm4(1:2),l0_bcm4(1:2),l_bcm4(1:2),"bcm4"
       
       call cmbine_spr_serially_cnnctd(NsprToCm,ks_bcm4,l0_bcm4,l_bcm4,ks_acm4,l0_acm4,l_acm4)
       write(389,*) ks_acm4,l0_acm4,l_acm4,"acm4"
       
       kval(1)  = ks_acm3 ; kval(2)  = ks_acm4
       l0val(1) = l0_acm3 ; l0val(2) = l0_acm4
       lval(1)  = l_acm3  ; lval(2)  = l_acm4
       
       write(389,*) kval(1:2), "kval"
       write(389,*) l0val(1:2),"l0val"
       write(389,*) lval(1:2), "lval"
       
    elseif (CellsMeet == 2) then
       
       ks_L = k_sprStrES(sprL) ; ks_C = k_sprStrES(sprC) ; ks_R = k_sprStrES(sprR)
       l0_L = l0_StrES(sprL)   ; l0_C = l0_StrES(sprC)   ; l0_R = l0_StrES(sprR) 
       l_L  = l_StrES(sprL)    ; l_C  = l_StrES(sprC)    ; l_R  = l_StrES(sprR)
       
       write(*,*) ks_L,ks_C,ks_R,"ks bfr L,C,R"
       write(*,*) l0_L,l0_C,l0_R,"l0 bfr L,C,R"
       write(*,*) l_L ,l_C ,l_R, "l  bfr L,C,R"
       
       ! Splitting Central Spring into Two
       
       ks_C1 = (ks_C)*(2.0d0) ; ks_C2 = ks_C1
       l0_C1 = (l0_C)/(2.0d0) ; l0_C2 = l0_C1
       l_C1  = (l_C) /(2.0d0) ; l_C2  = l_C1
       
       write(*,*) ks_C1,ks_C2,"ks_C's aft split"
       write(*,*) l0_C1,l0_C2,"l0_C's aft split"
       write(*,*) l_C1 ,l_C2 ,"l_C's aft split"
       
       ! combinging four springs
       
       ks_P = ks_L + ks_C1 + ks_C2 + ks_R
       l0_P = (1.0d0/4.0d0) * (l0_L + l0_C1 + l0_C2 + l0_R)
       l_P  = (1.0d0/4.0d0) * (l_L  + l_C1  + l_C2  + l_R )
       
       write(*,*) ks_P,l0_P,l_P,"ks-l0-l P's aft combining"
       
       ! splitting single spring parallelly into two
       
       ks_P1 = ks_P/2.0d0 ; ks_P2 = ks_P1
       l0_P1 = l0_P       ; l0_P2 = l0_P1
       l_P1  = l_P        ; l_P2  = l_P1
       
       write(*,*) ks_P1,ks_P2,"ks_P1 & ks_P2"
       write(*,*) l0_P1,l0_P2,"l0_P1 & l0_P2"
       write(*,*) l_P1 ,l_P2 ,"l_P1  & l_P2 "
       
       kval(1)  = ks_P1 ; kval(2)  = ks_P2
       l0val(1) = l0_P1 ; l0val(2) = l0_P2
       lval(1)  = l_P1  ; lval(2)  = l_P2
       
       write(*,*) kval(1:2),"kval"
       write(*,*) l0val(1:2),"l0val"
       write(*,*) lval(1:2),"lval"
       
    endif
    
    close(389)
    
  end subroutine get_spr_prp_aft_cmbining_spr
  
  subroutine get_ks_prop_bfr_comb(NsprToCm,caseV,ks_spltL,ks_spltC,ks_spltR,ks_bp)
    implicit none
    integer, intent(in)  :: NsprToCm,caseV
    real*8 , intent(in)  :: ks_spltL(1:2),ks_spltC(1:2),ks_spltR(1:2)
    real*8 , intent(out) :: ks_bp(1:NsprToCm)
    
    if (caseV==1) then
       ks_bp(1) = ks_spltL(2) ; ks_bp(2) = ks_spltC(1)
       write(*,*) ks_bp(1:2),"ks_bp1"
    elseif (caseV==2) then   
       ks_bp(1)=ks_spltR(2) ; ks_bp(2)=ks_spltC(2)
       write(*,*) ks_bp(1:2),"ks_bp2"
    endif
    
  end subroutine get_ks_prop_bfr_comb
  
  subroutine get_l0_prop_bfr_comb(NsprToCm,caseV,l0_spltL,l0_spltC,l0_spltR,l0_bp)
    implicit none
    integer, intent(in)  :: NsprToCm,caseV
    real*8 , intent(in)  :: l0_spltL(1:2),l0_spltC(1:2),l0_spltR(1:2)
    real*8 , intent(out) :: l0_bp(1:NsprToCm)
    
    if (caseV==1) then
       l0_bp(1) = l0_spltL(2) ; l0_bp(2) = l0_spltC(1)
       write(*,*) l0_bp(1:2),"l0_bp1"
    elseif (caseV==2) then   
       l0_bp(1)=l0_spltR(2) ; l0_bp(2)=l0_spltC(2)
       write(*,*) l0_bp(1:2),"l0_bp2"
    endif
    
  end subroutine get_l0_prop_bfr_comb
  
  subroutine get_l_prop_bfr_comb(NsprToCm,caseV,l_spltL,l_spltC,l_spltR,l_bp)
    implicit none
    integer, intent(in)  :: NsprToCm,caseV
    real*8 , intent(in)  :: l_spltL(1:2),l_spltC(1:2),l_spltR(1:2)
    real*8 , intent(out) :: l_bp(1:NsprToCm)
    
    if (caseV==1) then
       l_bp(1) = l_spltL(2) ; l_bp(2) = l_spltC(1)
       write(*,*) l_bp(1:2),"l_bp1"
    elseif (caseV==2) then   
       l_bp(1)=l_spltR(2) ; l_bp(2)=l_spltC(2)
       write(*,*) l_bp(1:2),"l_bp2"
    endif
    
  end subroutine get_l_prop_bfr_comb
  
  
  
  subroutine get_Cgvars_wwo_dmlsh_spr
    implicit none
    
    CgXNode(1:N_node)  = 0.00d0
    CgXNode(1:N_nodeS) = CgXNode_Str(1:N_nodeS)
    CgYNode(1:N_node)  = 0.00d0
    
  end subroutine get_Cgvars_wwo_dmlsh_spr
  
  subroutine get_CgVars_adding_node_inIC_ApclMem
    implicit none
    
    CgXNode(1:N_node)    = 0.00d0
    CgXNode(1:N_nodeSAN) = CgXNode_StrAN(1:N_nodeSAN)
    CgYNode(1:N_node)    = 0.00d0
    
  end subroutine get_CgVars_adding_node_inIC_ApclMem
  
  subroutine get_CgVars_combining_cntrl_and_neighApcl_sprs
    implicit none
    integer :: lim1,lim2,lim3,lim4,lim5,lim6
    integer :: i,j
    integer :: cnnctdNodeVal
    
    lim1 = (Hlf_Ncell+1)*4 + 1 ! =49
    lim2 = N_node              ! =139
    
    do i = 1,N_node
       
       if (i.le.lim1) then
          
          if (node_cnnctd(i) == 0) then
             CgXNode(i) = CgXNode_StrES(i)
             CgYNode(i) = CgYNode_StrES(i)
             
          elseif (node_cnnctd(i) .ne. 0) then
             cnnctdNodeVal = node_cnnctd(i)
             write(*,*) cnnctdNodeVal,"cnnNodeVal"
             
             CgXNode(i)    = CgXNode_StrES(i) + CgXNode_StrES(cnnctdNodeVal) 
             CgYNode(i)    = CgYNode_StrES(i) + CgYNode_StrES(cnnctdNodeVal)
             
          endif
          
       elseif ((i.gt.lim1).and.(i.le.lim2)) then ! inserted nodes
          
          CgXNode(i) = CgXNode_StrES(i+1)
          CgYNode(i) = CgYNode_StrES(i+1)
          
       endif
       
    enddo
    
  end subroutine get_CgVars_combining_cntrl_and_neighApcl_sprs
  
  subroutine get_kphi_and_nodePhiTyp_wwo_dmlsh_spr
    implicit none
    real*8  :: k_phiVal
    
    k_phiVal = k_phiStr(1,1)
    k_phi(1:N_node,1:max_Phi_node) = k_phiVal !that's because no bending
    
    nodePhi_typ(1:N_node) = 1
    
  end subroutine get_kphi_and_nodePhiTyp_wwo_dmlsh_spr
  
  subroutine get_kphi_and_nodePhiTyp_adding_node_inIC_ApclMem
    implicit none
    real*8 :: k_phiVal
    
    k_phiVal                       = k_phiStrAN(1,1)
    k_phi(1:N_node,1:max_Phi_node) = k_phiVal !that's because no bending
    nodePhi_typ(1:N_node)          = 1
    
  end subroutine get_kphi_and_nodePhiTyp_adding_node_inIC_ApclMem
  
  subroutine get_kphi_and_nodePhiTyp_combining_cntrl_and_neighApcl_sprs
    implicit none
    real*8 :: k_phiVal
    
    k_phiVal                       = k_phiStrES(1,1)
    k_phi(1:N_node,1:max_Phi_node) = k_phiVal !that's because no bending
    nodePhi_typ(1:N_node)          = 1
    
  end subroutine get_kphi_and_nodePhiTyp_combining_cntrl_and_neighApcl_sprs
  
  
  subroutine store_DS_trnsfrms
    implicit none
    !Renaming purpose, DS=Demolished Spring,TN=Terminal Node
    
    call store_TN_trnsfrms
    
  end subroutine store_DS_trnsfrms
  
  subroutine deallocate_and_reallocate_DS_trnsfrmVars
    implicit none
    
    deallocate(node_spr,node_area)
    deallocate(spr_node,spr_area)
    deallocate(area_node,area_spr)
    deallocate(Nlist)
    
    max_node_spr  = max_node_sprS
    max_area_spr  = max_area_sprS
    max_spr_node  = max_spr_nodeS
    max_area_node = max_area_nodeS 
    max_node_area = max_node_areaS 
    max_spr_area  = max_spr_areaS
    
    allocate(spr_node(1:N_spr,0:max_node_spr))
    allocate(spr_area(1:N_spr,0:max_area_spr))    
    allocate(node_spr(1:N_node,0:max_spr_node))
    allocate(node_area(1:N_node,0:max_area_node))
    allocate(area_spr(1:N_cell,0:max_spr_area))
    allocate(area_node(1:N_cell,0:max_node_area))
    allocate(Nlist(1:N_node,1:max_area_node,1:2))
    
    spr_node  = -1 ; spr_area  = -1 
    node_spr  = -1 ; node_area = -1
    area_node = -1 ; area_spr  = -1
    
    Nlist     = -1
    
  end subroutine deallocate_and_reallocate_DS_trnsfrmVars
  
  
  subroutine deallocate_and_reallocate_adding_node_inIC_ApclMem_trnsfrmVars
    implicit none
    
    deallocate(node_spr,node_area)
    deallocate(spr_node,spr_area)
    deallocate(area_node,area_spr)
    deallocate(Nlist)
    
    max_node_spr  = max_node_sprS+1
    max_area_spr  = max_area_sprS
    max_spr_node  = max_spr_nodeS
    max_area_node = max_area_nodeS 
    max_node_area = max_node_areaS+1
    max_spr_area  = max_spr_areaS
    
    allocate(spr_node(1:N_spr,0:max_node_spr))
    allocate(spr_area(1:N_spr,0:max_area_spr))    
    allocate(node_spr(1:N_node,0:max_spr_node))
    allocate(node_area(1:N_node,0:max_area_node))
    allocate(area_spr(1:N_cell,0:max_spr_area))
    allocate(area_node(1:N_cell,0:max_node_area))
    allocate(Nlist(1:N_node,1:max_area_node,1:2))
    
    spr_node  = -1 ; spr_area  = -1 
    node_spr  = -1 ; node_area = -1
    area_node = -1 ; area_spr  = -1
    
    Nlist     = -1
    
  end subroutine deallocate_and_reallocate_adding_node_inIC_ApclMem_trnsfrmVars
  
  
  subroutine deallocate_and_reallocate_combining_cntrl_and_neighApcl_sprs
    implicit none
    
    deallocate(node_spr,node_area)
    deallocate(spr_node,spr_area)
    deallocate(area_node,area_spr)
    deallocate(Nlist)
    
    max_node_spr  = max_node_sprS-1
    max_area_spr  = max_area_sprS
    max_spr_node  = max_spr_nodeS
    max_area_node = max_area_nodeS 
    max_node_area = max_node_areaS-1
    max_spr_area  = max_spr_areaS
    
    allocate(spr_node(1:N_spr,0:max_node_spr))
    allocate(spr_area(1:N_spr,0:max_area_spr))    
    allocate(node_spr(1:N_node,0:max_spr_node))
    allocate(node_area(1:N_node,0:max_area_node))
    allocate(area_spr(1:N_cell,0:max_spr_area))
    allocate(area_node(1:N_cell,0:max_node_area))
    allocate(Nlist(1:N_node,1:max_area_node,1:2))
    
    spr_node  = -1 ; spr_area  = -1 
    node_spr  = -1 ; node_area = -1
    area_node = -1 ; area_spr  = -1
    
    Nlist     = -1
    
  end subroutine deallocate_and_reallocate_combining_cntrl_and_neighApcl_sprs
  
  subroutine readjst_all_trnsfrms
    implicit none
    
    call readjst_node_to_other_trnsfrms
    call readjst_spr_to_other_trnsfrms
    call readjst_area_to_other_trnsfrms
    call get_Nlist
    call print_all_trnsfrms
    
  end subroutine readjst_all_trnsfrms
  
  subroutine readjst_node_to_other_trnsfrms
    implicit none
    integer :: i
    integer :: lft_nodeAct,rght_nodeAct 
    integer :: lim1,lim2,lim3,lim4
    integer :: dn11,dn12
    
    lft_nodeAct  = lft_node-2
    rght_nodeAct = rght_node-2
    
    lim1 = lft_nodeAct
    lim2 = lft_node
    lim3 = lft_node+rght_nodeAct
    lim4 = lft_node+rght_node
    
    do i = 1,N_node
       write(*,*) node_xy(i,1:2),i,"nodes,i"
    enddo
    
    do i = 1,N_node
       
       if (i.le.lim1) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          
          if ((i-lim1)==1) then
             
             dn11 = double_node(1,1)
             dn12 = double_node(1,2)
             
             write(*,*) i,dn11,dn12,"i,dn11,dn12, if i.ne.dn11, stop next line"
             !if (i.ne.dn11) stop
             
             node_spr(i,0) = 4
             node_spr(i,1) = node_sprS(dn11,1)
             node_spr(i,2) = node_sprS(dn11,2)
             node_spr(i,3) = node_sprS(dn12,1)
             node_spr(i,4) = node_sprS(dn12,2)
             
             node_area(i,0) = 3
             node_area(i,1) = node_areaS(dn11,1)
             node_area(i,2) = node_areaS(dn12,1)
             node_area(i,3) = node_areaS(dn11,2)
             
          elseif ((i-lim1)==2) then
             node_spr(i,0:2) = node_sprS(i,0:2)
             node_spr(i,3)   = node_sprS(i,3)-1 !49=50-1
             
             node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
             
          endif 
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          
          if ((i-lim3)==1) then
             write(*,*) i,dn11,dn12,"i,dn11,dn12, if i.ne.dn12, stop next line"
             !if (i.ne.dn12) stop
             
             node_spr(i,0:max_spr_node)   = node_spr(dn11,0:max_spr_node)
             node_area(i,0:max_area_node) = node_area(dn11,0:max_area_node)
             
          elseif ((i-lim3)==2) then
             node_spr(i,0:2) = node_sprS(i,0:2)
             node_spr(i,3)   = node_sprS(i,3)-1 !49=50-1
             
             node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
             
          endif
          
          
       elseif (i.gt.lim4) then
          
          if (i.ne.N_node) write(*,*) i,N_node,"fl:redefine_sys,sb:readjst_all"
          if (i.ne.N_node) stop
          
          node_spr(i,0)   = 4
          node_spr(i,1:2) = 3*ncl-2 !3*8-2 
          node_spr(i,3:4) = 3*(ncl+ncr)-2 !3*16-2
          
          node_area(i,0) = 2
          node_area(i,1) = ncl
          node_area(i,2) = ncl+ncr
          
       endif   
       
    enddo
    
  end subroutine readjst_node_to_other_trnsfrms
  
  
  subroutine readjst_spr_to_other_trnsfrms
    implicit none
    integer :: i
    integer :: nslAct,nsrAct 
    integer :: lim1,lim2,lim3,lim4
    
    nslAct = nsl-nsecl !I am not changing presuming nothing changes
    nsrAct = nsr-nsecr
    
    lim1 = nslAct
    lim2 = nsl
    lim3 = nsl+nsrAct
    lim4 = nsl+nsr
    
    do i = 1,N_spr
       
       if (i.le.lim1) then
          spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
          spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then

          if ((i-lim1)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_nodeS+1 !origin
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim1)==2) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr) 
             
          elseif ((i-lim1)==3) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          endif
             
       elseif (i.gt.lim2 .and. i.le.lim3) then
          spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
          spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          
          if ((i-lim3)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_nodeS+1 !origin
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim3)==2) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim3)==3) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          endif
          
       elseif (i.gt.lim4) then 
          spr_node(i,0:max_node_spr) = spr_nodeS(i+1,0:max_node_spr) !49 & 50
          spr_area(i,0:max_area_spr) = spr_areaS(i+1,0:max_area_spr) !49 & 50
          
       endif
       
    enddo
    
  end subroutine readjst_spr_to_other_trnsfrms
  
  
  subroutine readjst_area_to_other_trnsfrms
    implicit none
    integer :: i
    integer :: nclAct,ncrAct 
    integer :: lim1,lim2,lim3,lim4
    
    nclAct = ncl-1
    ncrAct = ncr-1
    
    lim1 = nclAct
    lim2 = ncl
    lim3 = ncl+ncrAct
    lim4 = ncl+ncr
    write(*,*) lim1,lim2,lim3,lim4,"lims"
    !call sleep(10)
    
    do i = 1,N_cell
       
       if (i.le.lim1) then
          area_node(i,0:max_node_area) = area_nodeS(i,0:max_node_area)
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          area_node(i,0)   = 5
          area_node(i,1:4) = area_nodeS(i,1:4)
          area_node(i,5)   = N_nodeS+1

          area_spr(i,0:max_spr_area) = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          area_node(i,0:max_node_area) = area_nodeS(i,0:max_node_area)
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          area_node(i,0)   = 5
          area_node(i,1)   = area_nodeS(i,1)
          area_node(i,2)   = N_nodeS+1
          area_node(i,3:5) = area_nodeS(i,2:4)
          
          area_spr(i,0:max_spr_area)  = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim4) then
          area_node(i,0)   = 3
          area_node(i,1:3) = area_nodeS(i,2:4)
          
          area_spr(i,0) = 3
          area_spr(i,1) = area_sprS(i,1)
          area_spr(i,2) = area_sprS(i,2)
          area_spr(i,3) = area_sprS(i,4)
          
          if (i.ne.N_cell) write(*,*) i,N_cell,"i=/N_cell"
          if (i.ne.N_cell) stop
          
       endif
       
    enddo
    
  end subroutine readjst_area_to_other_trnsfrms
  
  
  subroutine readjst_all_trnsfrms_wo_dmlsh
    implicit none
    
    call readjst_node_to_other_trnsfrms_wo_dmlsh
    call readjst_spr_to_other_trnsfrms_wo_dmlsh
    call readjst_area_to_other_trnsfrms_wo_dmlsh
    call get_Nlist
    call print_all_trnsfrms
    
  end subroutine readjst_all_trnsfrms_wo_dmlsh
  
  subroutine readjst_node_to_other_trnsfrms_wo_dmlsh
    implicit none
    integer :: i
    integer :: lft_nodeAct,lft_nodePrvAct
    integer :: rght_nodeAct,rght_nodePrvAct
    integer :: lim1,lim2,lim3,lim4,lim5,lim6
    integer :: dn11,dn12

    open(unit=142,file='readjst_lims.dat',position='append')
    
    lft_nodeAct  = lft_node-CellsMeet*2  ; lft_nodePrvAct  = lft_node-(CellsMeet-1)*2
    rght_nodeAct = rght_node-CellsMeet*2 ; rght_nodePrvAct = rght_node-(CellsMeet-1)*2
    
    lim1 = lft_nodeAct !14 to 12
    lim2 = lft_nodePrvAct !16 to 14
    lim3 = lft_node !18
    lim4 = lft_node+rght_nodeAct
    lim5 = lft_node+rght_nodePrvAct
    lim6 = lft_node+rght_node
    
    
    write(142,*)lft_nodeAct,lft_nodePrvAct,"lft"
    write(142,*)rght_nodeAct,rght_nodePrvAct,"rght"
    write(142,*)lft_node,rght_node,"LRN"
    
    write(142,*) lim1,lim2,lim3,lim4,lim5,lim6,"lims"
    write(142,*) CellsMeet,"CM "
    
    write(*,*) lim1,lim2,lim3,lim4,lim5,lim6,"lims"
    write(*,*) CellsMeet,"CM "
    
    close(142)
    
    do i = 1,N_node
       
       if (i.le.lim1) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          
          if ((i-lim1)==1) then
             dn11 = double_node(1,1)
             dn12 = double_node(1,2)
             write(*,*) i,dn11,dn12,"i,dn11,dn12, if i.ne.dn11, stop next line"
             if (i.ne.dn11) stop
             
             node_spr(i,0) = 6
             node_spr(i,1) = node_sprS(dn11,1)
             node_spr(i,2) = node_sprS(dn11,2)
             node_spr(i,3) = node_sprS(dn12,1)
             node_spr(i,4) = node_sprS(dn12,2)
             node_spr(i,5) = node_sprS(dn11,3)
             node_spr(i,6) = node_sprS(dn12,3)
             
             node_area(i,0) = 4
             node_area(i,1) = node_areaS(dn11,1)
             node_area(i,2) = node_areaS(dn12,1)
             node_area(i,3) = node_areaS(dn11,2)
             node_area(i,4) = node_areaS(dn12,2)
             
          elseif ((i-lim1)==2) then
             node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
             node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
             
          endif
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim4 .and. i.le.lim5) then
          
          if ((i-lim4)==1) then
             write(*,*) i,dn11,dn12,"i,dn11,dn12, if i.ne.dn12, stop next line"
             if (i.ne.dn12) stop
             
             node_spr(i,0:max_spr_node)   = node_spr(dn11,0:max_spr_node)
             node_area(i,0:max_area_node) = node_area(dn11,0:max_area_node)
          elseif ((i-lim4)==2) then
             node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
             node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
             
          endif
          
       elseif (i.gt.lim5 .and. i.le.lim6) then
          node_spr(i,0:max_spr_node)   = node_sprS(i,0:max_spr_node)
          node_area(i,0:max_area_node) = node_areaS(i,0:max_area_node)
          
       elseif (i.gt.lim6) then
          node_spr(i,0) = node_sprS(i,0)
          node_spr(i,1) = node_sprS(i,1)-nsecl
          node_spr(i,2) = node_sprS(i,2)-nsecl
          node_spr(i,3) = node_sprS(i,3)-nsecr
          node_spr(i,4) = node_sprS(i,4)-nsecr
          
          node_area(i,0) = node_areaS(i,0)
          node_area(i,1) = node_areaS(i,1)-1
          node_area(i,2) = node_areaS(i,2)-1
       endif
       
    enddo
    
  end subroutine readjst_node_to_other_trnsfrms_wo_dmlsh
  
  subroutine readjst_spr_to_other_trnsfrms_wo_dmlsh
    implicit none
    integer :: i
    integer :: nslAct,nslPrvAct
    integer :: nsrAct,nsrPrvAct 
    integer :: lim1,lim2,lim3,lim4,lim5,lim6
    
    nslAct = nsl-CellsMeet*nsecl ; nslPrvAct = nsl-(CellsMeet-1)*nsecl
    nsrAct = nsr-CellsMeet*nsecr ; nsrPrvAct = nsr-(CellsMeet-1)*nsecr
    
    lim1 = nslAct !18 to 15
    lim2 = nslPrvAct !21 to 18
    lim3 = nsl !24
    lim4 = nsl+nsrAct
    lim5 = nsl+nsrPrvAct
    lim6 = nsl+nsr
    
    
    do i = 1,N_spr
       
       if (i.le.lim1) then !1-15
          spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
          spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then !16-18
          
          if ((i-lim1)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_node !37
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim1)==2 .or. (i-lim1)==3) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          endif
          
       elseif (i.gt.lim2 .and. i.le.lim3) then !19-24
          
          if ((i-lim2)==1) then
             spr_node(i,0) = 2
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = spr_nodeS(i,3)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim2).gt.1) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          endif
          
       elseif (i.gt.lim3 .and. i.le.lim4) then !25-39
          spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
          spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim4 .and. i.le.lim5) then !40-42
          
          if ((i-lim4)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_node
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim4)==2 .or. (i-lim4)==3) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          endif
          
       elseif (i.gt.lim5 .and. i.le.lim6) then !43-48
          
          if ((i-lim5)==1) then
             spr_node(i,0) = 2
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = spr_nodeS(i,3)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim5).gt.1) then
             spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          endif
          
       elseif (i.gt.lim6) then
          spr_node(i,0:max_node_spr) = spr_nodeS(i,0:max_node_spr)
          spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
       endif
       
    enddo
    
  end subroutine readjst_spr_to_other_trnsfrms_wo_dmlsh
  
  
  subroutine readjst_area_to_other_trnsfrms_wo_dmlsh
    implicit none
    integer :: i
    integer :: nclAct,nclPrvAct
    integer :: ncrAct,ncrPrvAct 
    integer :: lim1,lim2,lim3,lim4,lim5,lim6
    
    nclAct = ncl-CellsMeet ; nclPrvAct = ncl-(CellsMeet-1)
    ncrAct = ncr-CellsMeet ; ncrPrvAct = ncr-(CellsMeet-1)
    
    lim1 = nclAct !5
    lim2 = nclPrvAct !6
    lim3 = ncl !8
    lim4 = ncl+ncrAct
    lim5 = ncl+ncrPrvAct
    lim6 = ncl+ncr
    
    write(*,*) lim1,lim2,lim3,lim4,lim5,lim6,"lims in Area_to_other_adjst"
    
    do i = 1,N_cell
       
       if (i.le.lim1) then !1-5
          area_node(i,0:max_node_area) = area_nodeS(i,0:max_node_area)
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then !6 
          area_node(i,0) = 5
          area_node(i,1:4) = area_nodeS(i,1:4)
          area_node(i,5)   = N_node
          
          area_spr(i,0:max_spr_area) = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim2 .and. i.le.lim3) then !7-8
          area_node(i,0)   = 4
          area_node(i,1:4) = area_nodeS(i,1:4)
          
          area_spr(i,0:max_spr_area) = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then !9-13
          area_node(i,0:max_node_area) = area_nodeS(i,0:max_node_area)
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim4 .and. i.le.lim5) then !14
          area_node(i,0)   = 5
          area_node(i,1)   = area_nodeS(i,1)
          area_node(i,2)   = N_node
          area_node(i,3:5) = area_nodeS(i,2:4)
          
          area_spr(i,0:max_spr_area) = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim5 .and. i.le.lim6) then !15-16

          if ((i-lim5)==1) then
             area_node(i,0)   = 4
             area_node(i,1)   = area_nodeS(i,1)
             area_node(i,2:4) = area_nodeS(i,3:5)
          elseif ((i-lim5).gt.1) then
             area_node(i,0)   = 4
             area_node(i,1:4) = area_nodeS(i,1:4)
          endif
          
          area_spr(i,0:max_spr_area)  = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim6) then
          area_node(i,0:max_node_area) = area_nodeS(i,0:max_node_area)
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       endif
       
    enddo
    
    
  end subroutine readjst_area_to_other_trnsfrms_wo_dmlsh
  
  
  subroutine readjst_all_trnsfrms_inIC_ApclMem
    implicit none
    
    call readjst_node_to_other_trnsfrms_inIC_ApclMem
    call readjst_spr_to_other_trnsfrms_inIC_ApclMem
    call readjst_area_to_other_trnsfrms_inIC_ApclMem
    call print_all_trnsfrms
    write(*,*) "MADE UPTO THIS"
    call get_Nlist
    call print_all_trnsfrms
    
  end subroutine readjst_all_trnsfrms_inIC_ApclMem
  
  subroutine readjst_node_to_other_trnsfrms_inIC_ApclMem
    implicit none
    integer :: i,j,imax,jmax
    
    write(*,*) N_nodeS,N_node,"N_nodes bfr aft"
    write(*,*) node_spr(N_nodeS+1,0:max_spr_node),"node_spr for N_node+1"
    write(*,*) node_spr(N_nodeS+2,0:max_spr_node),"node_spr for N_node+2"
    
    write(*,*) node_area(N_nodeS+1,0:max_area_node),"node_area for N_node+1"
    write(*,*) node_area(N_nodeS+2,0:max_area_node),"node_area for N_node+2"
    
    node_spr(1:N_nodeS,0:max_spr_node) = node_sprS(1:N_nodeS,0:max_spr_node)
    
    node_spr(N_nodeS+1,0)   = 4
    node_spr(N_nodeS+1,1:2) = 3*ncl-2
    node_spr(N_nodeS+1,3:4) = 3*(ncl+ncr)+1
    
    node_spr(N_nodeS+2,0)   = 4
    node_spr(N_nodeS+2,1:2) = 3*(ncl+ncr)-2
    node_spr(N_nodeS+2,3:4) = 3*(ncl+ncr)+1
    
    node_area(1:N_nodeS,0:max_area_node) = node_areaS(1:N_nodeS,0:max_area_node)
    
    node_area(N_nodeS+1,0) = 2
    node_area(N_nodeS+1,1) = ncl
    node_area(N_nodeS+1,2) = N_cell
    
    node_area(N_nodeS+2,0) = 2
    node_area(N_nodeS+2,1) = ncl+ncr
    node_area(N_nodeS+2,2) = N_cell
    
    open(unit=443,file='readjst_NTO_inIC_ApclMem.dat')
    
    imax = 2
    jmax = N_node
    
    do i = 1,imax
       
       jmax = N_node
       
       do j = 1,jmax
          if (i==1) write(443,*) j,node_spr(j,0:max_spr_node)
          if (i==2) write(443,*) j,node_area(j,0:max_area_node)
       enddo
       write(443,*) " "
    enddo
    
    close(443)
    
  end subroutine readjst_node_to_other_trnsfrms_inIC_ApclMem
  
  
  subroutine readjst_spr_to_other_trnsfrms_inIC_ApclMem
    implicit none
    integer :: lim1,lim2,lim3,lim4,lim5
    integer :: i,j,imax,jmax
    
    lim1 = (Hlf_Ncell-1)*3         ! unchanged left [1 ~ 10*3=30]
    lim2 = lim1 + 3                ! total left spr [31 ~ 33]
    lim3 = lim2 + (Hlf_Ncell-1)*3  ! unchaned right [34 ~ 63]
    lim4 = lim3 + 3                ! total right [64 ~ 66]
    lim5 = lim4 + 2                ! two central cells
    
    write(*,*) lim1,lim2,lim3,lim4,lim5, "lims in "
    write(*,*) N_spr,N_sprS,"N_spr"
    
    !IMPORTANT Explanation: In this system I need the max_node_spr=4 which was =3 in previous cases, that is why
    !whenever I was copying spr_node(i,0:~) = spr_nodeS(i,0:~), I was writing (max_node_spr-1) in the place of ~
    
    do i = 1,N_spr
       
       if (i.le.lim1) then
          spr_node(i,0:max_node_spr-1) = spr_nodeS(i,0:max_node_spr-1)
          spr_area(i,0:max_area_spr)   = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          
          if ((i-lim1)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_nodeS+1
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim1) .gt. 1) then
             spr_node(i,0:max_node_spr-1) = spr_nodeS(i,0:max_node_spr-1)
             spr_area(i,0:max_area_spr)   = spr_areaS(i,0:max_area_spr) 
          endif
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          spr_node(i,0:max_node_spr-1) = spr_nodeS(i,0:max_node_spr-1)
          spr_area(i,0:max_area_spr)   = spr_areaS(i,0:max_area_spr)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          
          if ((i-lim3)==1) then
             spr_node(i,0) = 3
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_nodeS+2
             spr_node(i,3) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim3) .gt. 1) then
             spr_node(i,0:max_node_spr-1) = spr_nodeS(i,0:max_node_spr-1)
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
          endif
          
       elseif (i.gt.lim4 .and. i.le.lim5) then ! central springs 
          
          if ((i-lim4)==1) then
             spr_node(i,0) = 4
             spr_node(i,1) = spr_nodeS(i,1)
             spr_node(i,2) = N_nodeS+1
             spr_node(i,3) = N_nodeS+2
             spr_node(i,4) = spr_nodeS(i,2)
             
             spr_area(i,0:max_area_spr) = spr_areaS(i,0:max_area_spr)
             
          elseif ((i-lim4)==2) then
             spr_node(i,0:max_node_spr-1) = spr_nodeS(i,0:max_node_spr-1)
             spr_area(i,0:max_area_spr)   = spr_areaS(i,0:max_area_spr)
          endif
          
       endif
       
    enddo

    
    open(unit=445,file='readjst_STO_inIC_ApclMem.dat')
    
    imax = 2
    jmax = N_spr
    
    do i = 1,imax
       
       jmax = N_spr
       
       do j = 1,jmax
          if (i==1) write(445,*) j,spr_node(j,0:max_node_spr)
          if (i==2) write(445,*) j,spr_area(j,0:max_area_spr)
       enddo
       write(445,*) " "
    enddo
    
    close(445)
    
  end subroutine readjst_spr_to_other_trnsfrms_inIC_ApclMem
  
  subroutine readjst_area_to_other_trnsfrms_inIC_ApclMem
    implicit none
    integer :: lim1,lim2,lim3,lim4,lim5
    integer :: nodesInAreaBfr,nodesInAreaAft
    integer :: i,j,imax,jmax
    
    lim1 = Hlf_Ncell-1
    lim2 = Hlf_Ncell
    lim3 = lim2+(Hlf_Ncell-1)
    lim4 = lim3+1
    lim5 = N_cell

    write(*,*) lim1,lim2,lim3,lim4,lim5,"lims in readjst_ATO_trnsfr_inIC_ApclMem"
    call sleep(3)
    
    !IMPORTANT Explntn: In this systm I need the max_node_area=6 which was =5 in previous cases, that is why
    !whenever I was copying area_node(i,0:~) = area_nodeS(i,0:~), I was writing (max_node_area-1) in place of ~
    
    do i = 1,N_cell
       
       if (i.le.lim1) then
          area_node(i,0:max_node_area-1) = area_nodeS(i,0:max_node_area-1)
          area_spr(i,0:max_spr_area)     = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim1 .and. i.le.lim2) then
          
          nodesInAreaBfr = area_nodeS(i,0)
          nodesInAreaAft = nodesInAreaBfr+1
          
          area_node(i,0) = nodesInAreaAft
          area_node(i,1:nodesInAreaBfr) = area_nodeS(i,1:nodesInAreaBfr)
          area_node(i,nodesInAreaAft)   = N_nodeS+1
          
          area_spr(i,0:max_spr_area) = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim2 .and. i.le.lim3) then
          
          area_node(i,0:max_node_area-1) = area_nodeS(i,0:max_node_area-1)
          area_spr(i,0:max_spr_area)     = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim3 .and. i.le.lim4) then
          
          nodesInAreaBfr = area_nodeS(i,0)
          nodesInAreaAft = nodesInAreaBfr+1
          
          area_node(i,0)                = nodesInAreaAft
          area_node(i,1)                = area_nodeS(i,1)
          area_node(i,2)                = N_nodeS+2
          area_node(i,3:nodesInAreaAft) = area_nodeS(i,2:nodesInAreaBfr)
          
          area_spr(i,0:max_spr_area)   = area_sprS(i,0:max_spr_area)
          
       elseif (i.gt.lim4 .and. i.le.lim5) then
          
          nodesInAreaBfr = area_nodeS(i,0)
          nodesInAreaAft = nodesInAreaBfr+2
          
          area_node(i,0)                = nodesInAreaAft
          area_node(i,1)                = area_nodeS(i,1)
          area_node(i,2)                = N_nodeS+2
          area_node(i,3)                = N_nodeS+1
          area_node(i,4:nodesInAreaAft) = area_nodeS(i,2:nodesInAreaBfr)
          
          area_spr(i,0:max_spr_area)    = area_sprS(i,0:max_spr_area)
          
       endif
       
    enddo
    
    open(unit=446,file='readjst_ATO_inIC_ApclMem.dat')
    
    imax = 2
    jmax = N_cell
    
    do i = 1,imax
       
       jmax = N_cell
       
       do j = 1,jmax
          if (i==1) write(446,*) j,area_node(j,0:max_node_area)
          if (i==2) write(446,*) j,area_spr(j,0:max_spr_area)
       enddo
       write(446,*) " "
    enddo
    
    close(446)
    
  end subroutine readjst_area_to_other_trnsfrms_inIC_ApclMem
  
  
  subroutine readjst_all_trnsfrms_frm_reading_cellsmeet_file
    implicit none
    
    call readjst_node_to_other_trnsfrms_frm_reading_cellsmeet_file
    call readjst_spr_to_other_trnsfrms_frm_reading_cellsmeet_file
    call readjst_area_to_other_trnsfrms_frm_reading_cellsmeet_file
    
    call get_Nlist
    write(*,*) Nlist(1,1,1:2),"1"
    call print_all_trnsfrms
    write(*,*) Nlist(1,1,1:2),"2"
    
    write(*,*) "IT works upTo readjst_file"
    !stop
    
  end subroutine readjst_all_trnsfrms_frm_reading_cellsmeet_file
  
  subroutine readjst_node_to_other_trnsfrms_frm_reading_cellsmeet_file
    implicit none
    integer :: i,j
    integer :: nodeV

    if (CellsMeet==1) then
       open(unit=271,file='node_spr_cellsMeets_eq_1.dat')
       open(unit=272,file='node_area_cellsMeets_eq_1.dat')
       open(unit=371,file='node_other_cellsMeets_eq_1.dat')
    elseif (CellsMeet==2) then
       open(unit=271,file='node_spr_cellsMeets_eq_2.dat')
       open(unit=272,file='node_area_cellsMeets_eq_2.dat')
       open(unit=371,file='node_other_cellsMeets_eq_2.dat')
    else
       write(*,*) "CellsMeet not equal to anyone 01"
       stop
    endif
    
    
    do i = 1,2
       
       do j = 1,N_node
          
          if (i==1) then
             read(271,*)  nodeV,node_spr(nodeV,0:max_spr_node) 
             write(371,*) nodeV,node_spr(nodeV,0:max_spr_node)
          elseif (i==2) then
             read(272,*)  nodeV,node_area(nodeV,0:max_area_node)
             write(371,*) nodeV,node_area(nodeV,0:max_area_node)
          endif
          
       enddo
       
       write(371,*) " "
    enddo
    
    close(271)
    close(272)
    close(371)
    
  end subroutine readjst_node_to_other_trnsfrms_frm_reading_cellsmeet_file
  
  subroutine readjst_spr_to_other_trnsfrms_frm_reading_cellsmeet_file 
    implicit none
    integer :: i,j
    integer :: sprV
    
    if (CellsMeet==1) then
       open(unit=273,file='spr_node_cellsMeets_eq_1.dat')
       open(unit=274,file='spr_area_cellsMeets_eq_1.dat')
       open(unit=373,file='spr_other_cellsMeets_eq_1.dat')
    elseif (CellsMeet==2) then
       open(unit=273,file='spr_node_cellsMeets_eq_2.dat')
       open(unit=274,file='spr_area_cellsMeets_eq_2.dat')
       open(unit=373,file='spr_other_cellsMeets_eq_2.dat')
    else
       write(*,*) "CellsMeet not equal to anyone 02"
       stop
    endif
    
    do i = 1,2
       
       do j = 1,N_spr
          
          if (i==1) then
             read(273,*)  sprV,spr_node(sprV,0:max_node_spr)
             write(373,*) sprV,spr_node(sprV,0:max_node_spr)
          elseif (i==2) then
             read(274,*)  sprV,spr_area(sprV,0:max_area_spr)
             write(373,*) sprV,spr_area(sprV,0:max_area_spr)
          endif
          
       enddo
       write(373,*) " "
    enddo
    
    close(273)
    close(274)
    close(373)
    
  end subroutine readjst_spr_to_other_trnsfrms_frm_reading_cellsmeet_file
  
  subroutine readjst_area_to_other_trnsfrms_frm_reading_cellsmeet_file
    implicit none
    integer :: i,j
    integer :: areaV
    
    if (CellsMeet==1) then
       open(unit=275,file='area_node_cellsMeets_eq_1.dat')
       open(unit=276,file='area_spr_cellsMeets_eq_1.dat')
       open(unit=375,file='area_other_cellsMeets_eq_1.dat')
    elseif (CellsMeet==2) then
       open(unit=275,file='area_node_cellsMeets_eq_2.dat')
       open(unit=276,file='area_spr_cellsMeets_eq_2.dat')
       open(unit=375,file='area_other_cellsMeets_eq_2.dat')
    else
       write(*,*) "CellsMeet not equal to anyone 03"
       stop
    endif
    
    
    do i = 1,2
       
       do j = 1,N_cell
          
          if (i==1) then
             read(275,*)  areaV,area_node(areaV,0:max_node_area)
             write(375,*) areaV,area_node(areaV,0:max_node_area)
          elseif (i==2) then
             read(276,*)  areaV,area_spr(areaV,0:max_spr_area)
             write(375,*) areaV,area_spr(areaV,0:max_spr_area)
          endif
          
       enddo
       write(375,*) " "
    enddo
    
    close(275)
    close(276)
    close(375)
    
  end subroutine readjst_area_to_other_trnsfrms_frm_reading_cellsmeet_file
  
  subroutine redefining_system_parameters_stage1_type1
    implicit none
    
    open(unit=167,file='redefine_system_S1T1.dat')
    
    !lft_ladder_params
    ncl  = ncl - 1
    nsl  = nsecl * ncl
    nvsl = ncl + 1
    
    lft_node = 2*nvsl
    
    write(167,*) ncl,nsl,nvsl,lft_node,"ncl,nsl,nvsl,lft_Node"
    
    !rght_ladder_params
    ncr  = ncr - 1
    nsr  = nsecr * ncr
    nvsr = ncr + 1
    
    rght_node = 2*nvsr
    
    write(167,*) ncr,nsr,nvsr,rght_node,"ncl,nsl,nvsl,lft_Node"
    
    
    
    write(167,*) N_node,N_spr,N_cell,Hlf_Ncell,"N_node,N_spr,N_cell,Hlf_Ncell"
    write(167,*) N_lftCells,N_rghtCells,"N lft and rght Cells"

    close(167)
    
  end subroutine redefining_system_parameters_stage1_type1

  
  
  
end module redefining_system_module
