module mltiple_calls_together
  use sys_building_info
  use cell_info
  use end_info
  use system_parameters
  use generating_the_shape 
  use perturbing_the_shape
  use node_variables
  use spring_variables
  use area_variables
  use spr_to_other_transfrm
  use node_to_other_transfrm
  use area_to_other_transfrm
  use neighbour_info_and_trnsfrmInfo_dependent_info
  use transfrm_info
  use conversion_routines
  use gradient_module
  
  implicit none
  
contains

  subroutine get_blcks
    
    implicit none
    integer :: i
    
    call generate_blck_params
    call generate_blcks
    call get_list_of_double_nodes_method2
    call nodes_cnnctd_and_count_this_dn
    call get_CgNode
    call get_bend_variable_values
    
    do i = 1,N_node
       write(*,*) node_xy(i,1:2),i,"node_xy"
    enddo
    
  end subroutine get_blcks

  
  subroutine get_all_the_transfrms
    implicit none

    if (wntbb(0) .eq. 0) then
       write(*,*) "NOTHING TO BE BUILT, Loc : get_all_transfrms sbrtn of transfrm_variables.f08"
       
    else
       
       if (modelID==1) then
          
          call allocate_and_initialize_transfrm_variables
          
          call get_spr_to_node_transfrm
          call get_spr_to_area_transfrm
          
          call get_node_to_spr_transfrm
          call get_node_to_area_transfrm
          
          call get_area_to_spr_transfrm
          call get_area_to_node_transfrm
          
          call get_Nlist
          
          call allocate_and_initialize_phiInfo
          call get_phiInfo
          
          call allocate_and_initialize_curve_variables
          
       elseif (modelID==2) then
          
          call get_spr_to_node_transfrm
          call get_spr_to_area_transfrm
          
          call get_node_to_spr_transfrm
          call get_node_to_area_transfrm
          
          call get_area_to_spr_transfrm
          call get_area_to_node_transfrm
          
          call get_Nlist
          
       endif
       
    endif
    
  end subroutine get_all_the_transfrms
  
  
  subroutine deallocate_and_reallocate_arrays_with_N_mvCoordnte_withl0A0
    implicit none
    
    integer :: NMCWL0A0_Prev,NMCWL0A0_Curr
    real*8, allocatable :: coordntes_Prev(:),grd_mvPrev(:)
    
    integer :: i,j,jmax
    integer :: cnt_Prev,cnt_Curr
    
    NMCWL0A0_Prev = N_mvCoordnte_withl0A0

    allocate(coordntes_Prev(1:NMCWL0A0_Prev))
    allocate(grd_mvPrev(1:NMCWL0A0_Prev))
    
    coordntes_Prev = coordntes
    grd_mvPrev = grd_mv
    
    call get_N_mvCoordnte_withl0A0
    !write(*,*) N_mvCoordnte_withl0A0,"N_mvCoordnte_withl0A0"
    NMCWL0A0_Curr = N_mvCoordnte_withl0A0
    
    deallocate(coordntes)
    allocate(coordntes(1:NMCWL0A0_Curr))
    coordntes = -1.d30
    
    deallocate(grd_mv)
    allocate(grd_mv(1:N_mvCoordnte_withl0A0))
    grd_mv = 10.d5


    if (NMCWL0A0_Curr.le.NMCWL0A0_Prev) then

       cnt_Prev = 0 ; cnt_Curr = 0
       
       coordntes(1:N_mvCoordnte) = coordntes_Prev(1:N_mvCoordnte)
       grd_mv(1:N_mvCoordnte)    = grd_mvPrev(1:N_mvCoordnte)
       
       cnt_Prev = N_mvCoordnte
       cnt_Curr = N_mvCoordnte


       do i = 1,2
          if (i==1) jmax = N_spr
          if (i==2) jmax = N_cell
          
          do j = 1,jmax
             
             if (i==1) then
                
                if (optmSpr(j)==1) then
                   cnt_Prev=cnt_Prev+1
                   cnt_Curr=cnt_Curr+1
                   
                   coordntes(cnt_Curr) = coordntes_Prev(cnt_Prev)
                   grd_mv(cnt_Curr)    = grd_mvPrev(cnt_Prev) 
                   
                elseif (optmSpr(j)==0) then
                   cnt_Prev = cnt_Prev+1
                   
                elseif (optmSpr(j).ne.0 .AND. optmSpr(j).ne.1) then
                   write(*,*) "Neither 0 nor 1, flnm:mltiple_calls_together"
                endif
           
                
             elseif (i==2) then
             
                if (optmCell(j)==1) then
                   cnt_Prev=cnt_Prev+1
                   cnt_Curr=cnt_Curr+1
                   
                   coordntes(cnt_Curr) = coordntes_Prev(cnt_Prev)
                   grd_mv(cnt_Curr)    = grd_mvPrev(cnt_Prev) 
                   
                elseif (optmCell(j)==0) then
                   cnt_Prev = cnt_Prev+1
                   
                elseif (optmCell(j).ne.0 .AND. optmCell(j).ne.1) then
                   write(*,*) "Neither 0 nor 1, flnm:mltiple_calls_together"
                endif
                
             endif
             
          enddo
       enddo
       
       
       if (cnt_Prev.ne.NMCWL0A0_Prev) then
          write(*,*) "cnt_Prev =/ NMCWL0A0_Prev"
          stop
       endif

       if (cnt_Curr.ne.NMCWL0A0_Curr) then
          write(*,*) "cnt_Curr =/ NMCWL0A0_Curr"
          stop
       endif


    endif

    do i = 1,(N_mvCoordnte+1)
       if (i.le.N_mvCoordnte) then
          write(*,*) coordntes(i),coordntes_Prev(i)
       elseif (i.gt.N_mvCoordnte) then
          cnt_Prev = N_mvCoordnte
          cnt_Curr = N_mvCoordnte
          
          do j = 1,N_spr
             if (optmSpr(j)==1) then
                cnt_Prev = cnt_Prev+1
                cnt_Curr = cnt_Curr+1
                write(*,*) coordntes(cnt_Curr),coordntes_Prev(cnt_Prev),cnt_Curr,cnt_Prev
                
             elseif (optmSpr(j)==0) then
                cnt_Prev=cnt_Prev+1
                write(*,*) coordntes_Prev(cnt_Prev)
             endif
             
          enddo

          do j = 1,N_cell

             if (optmCell(j)==1) then
                cnt_Prev = cnt_Prev+1
                cnt_Curr = cnt_Curr+1
                !write(*,*) coordntes(cnt_Curr),coordntes_Prev(cnt_Prev),cnt_Curr,cnt_Prev
                
             elseif (optmCell(j)==0) then
                cnt_Prev=cnt_Prev+1
                !write(*,*) coordntes_Prev(cnt_Prev)
             endif
             
          enddo
       endif
    enddo
    
    !stop
  end subroutine deallocate_and_reallocate_arrays_with_N_mvCoordnte_withl0A0
  
  
  subroutine print_all_trnsfrms
    implicit none
    integer :: i,j
    integer :: max
    integer :: N_itm
    
    N_itm = 2
    
    open(unit=145,file='Transform_Variables.dat')
    open(unit=545,file='Transform_Variables_V2.dat')
    
    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "node_spr:"
       if (i==2) write(145,fmt=*) "node_area:"
       if (i==1) write(545,fmt=*) "node_spr:"
       if (i==2) write(545,fmt=*) "node_area:"
       
       do j = 1,N_node
          write(145,fmt=*) "node_nm =",j
          
          if (i==1) then
             write(145,fmt=*) node_spr(j,0:max_spr_node)
             write(545,fmt=*) "(",j,node_spr(j,0),")",node_spr(j,1:max_spr_node)
             
          elseif (i==2) then
             write(145,fmt=*) node_area(j,0:max_area_node)
             write(545,fmt=*) "(",j,node_area(j,0),")",node_area(j,1:max_area_node)
          endif
          
       enddo
       write(145,fmt=*) " "
       write(545,fmt=*) " "
       
    enddo
    
    
    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "spr_node:"
       if (i==2) write(145,fmt=*) "spr_area:"
       if (i==1) write(545,fmt=*) "spr_node:"
       if (i==2) write(545,fmt=*) "spr_area:"
       
       do j = 1,N_spr
          write(145,fmt=*) "spr_nm =",j
          
          if (i==1) then
             write(145,fmt=*) spr_node(j,0:max_node_spr)
             write(545,fmt=*) "(",j,spr_node(j,0),")",spr_node(j,1:max_node_spr)
             
          elseif (i==2) then
             write(145,fmt=*) spr_area(j,0:max_area_spr)
             write(545,fmt=*) "(",j,spr_area(j,0),")",spr_area(j,1:max_area_spr)
          endif

       enddo
       write(145,fmt=*) " "
       write(545,fmt=*) " "
       
    enddo

    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "area_node:"
       if (i==2) write(145,fmt=*) "area_spr:"
       if (i==1) write(545,fmt=*) "area_node:"
       if (i==2) write(545,fmt=*) "area_spr:"
       
       do j = 1,N_cell
          write(145,fmt=*) "area_nm =",j
          
          if (i==1) then
             write(145,fmt=*) area_node(j,0:max_node_area)
             write(545,fmt=*) "(",j,area_node(j,0),")",area_node(j,1:max_node_area)
             
          elseif (i==2) then
             write(145,fmt=*) area_spr(j,0:max_spr_area)
             write(545,fmt=*) "(",j,area_spr(j,0),")",area_spr(j,1:max_spr_area)
          endif
          
       enddo
       write(145,fmt=*) " "
       write(545,fmt=*) " "
       
    enddo
    
    
    do i = 1,N_node
       write(145,fmt=*) "Nlist for  node_nm =",i
       write(545,fmt=*) "Nlist for  node_nm =",i
    
       do j = 1,max_area_node
          write(145,fmt=*) node_area(i,j),Nlist(i,j,1:2)
          write(545,fmt=*) node_area(i,j),Nlist(i,j,1:2)
       enddo

       write(145,fmt=*) ""
       write(545,fmt=*) ""
    enddo

    write(145,fmt=*) N_phi,"N_phi"
    write(545,fmt=*) N_phi,"N_phi"

    write(145,fmt=*) "phi_no"," ","node_nm"," ","area_nm"
    write(545,fmt=*) "phi_no"," ","node_nm"," ","area_nm"
    
    do i = 1,N_phi
       write(145,fmt=*) i,phi_info(i,1:2)
       write(545,fmt=*) i,phi_info(i,1:2)
    enddo

    close(145)
    close(545)
    
  end subroutine print_all_trnsfrms

  subroutine print_all_stored_trnsfrms
    implicit none
    integer :: i,j,jmax
    integer :: N_itm

    N_itm = 2
    
    open(unit=145,file='Stored_Transform_Variables.dat')
    
    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "node_spr:"
       if (i==2) write(145,fmt=*) "node_area:"
       
       do j = 1,N_nodeS
          write(145,fmt=*) "node_nm =",j
          
          if (i==1) then
             write(145,fmt=*) node_sprS(j,0:max_spr_nodeS)
          elseif (i==2) then
             write(145,fmt=*) node_areaS(j,0:max_area_nodeS)
          endif
          
       enddo
       write(145,fmt=*) " "
       
    enddo
    
    
    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "spr_node:"
       if (i==2) write(145,fmt=*) "spr_area:"
       
       do j = 1,N_sprS
          write(145,fmt=*) "spr_nm =",j
          
          if (i==1) then
             write(145,fmt=*) spr_nodeS(j,0:max_node_sprS) 
          elseif (i==2) then
             write(145,fmt=*) spr_areaS(j,0:max_area_sprS)
          endif
          
       enddo
       write(145,fmt=*) " "
       
    enddo
    
    do i = 1,N_itm
       if (i==1) write(145,fmt=*) "area_node:"
       if (i==2) write(145,fmt=*) "area_spr:"
       
       do j = 1,N_cell
          write(145,fmt=*) "area_nm =",j
          
          if (i==1) then
             write(145,fmt=*) area_nodeS(j,0:max_node_areaS) 
          elseif (i==2) then
             write(145,fmt=*) area_sprS(j,0:max_spr_areaS)
          endif
          
       enddo
       write(145,fmt=*) " "
       
    enddo
    
       
    do i = 1,N_nodeS
       write(145,fmt=*) "Nlist for  node_nm =",i

       jmax = node_areaS(i,0)
       
       do j = 1,jmax
          write(145,fmt=*) NlistS(i,j,1:2)
       enddo
       
       write(145,fmt=*) " "
    enddo

    write(145,fmt=*) N_phi,"N_phi"
    write(145,fmt=*) max_node_sprS,max_area_sprS,"max_node_spr,max_area_spr"
    write(145,fmt=*) max_spr_nodeS,max_area_nodeS,"max_spr_node,max_area_node"
    write(145,fmt=*) max_node_areaS,max_spr_areaS,"max_node_area,max_spr_area"
    
    write(145,fmt=*) N_node,N_nodeS,"N_node,N_nodeS"
    write(145,fmt=*) N_spr,N_sprS,"N_spr,N_sprS"
    
    close(145)
    
  end subroutine print_all_stored_trnsfrms
  
  subroutine print_NI_trnsfrms_inFiles
    implicit none
    integer :: i,j
    integer :: max
    integer :: N_itm
    
    N_itm = 2

    if (CellsMeet==1) then
       
       open(unit=144,file='node_spr_cellsMeets_eq_1.dat')
       open(unit=145,file='node_area_cellsMeets_eq_1.dat')
       open(unit=146,file='spr_node_cellsMeets_eq_1.dat')
       open(unit=147,file='spr_area_cellsMeets_eq_1.dat')
       open(unit=148,file='area_node_cellsMeets_eq_1.dat')
       open(unit=149,file='area_spr_cellsMeets_eq_1.dat')
       
    elseif (CellsMeet==2) then
       
       open(unit=144,file='node_spr_cellsMeets_eq_2.dat')
       open(unit=145,file='node_area_cellsMeets_eq_2.dat')
       open(unit=146,file='spr_node_cellsMeets_eq_2.dat')
       open(unit=147,file='spr_area_cellsMeets_eq_2.dat')
       open(unit=148,file='area_node_cellsMeets_eq_2.dat')
       open(unit=149,file='area_spr_cellsMeets_eq_2.dat')
       
    else
       write(*,*) "CellsMeet not equal to anyone 00"
       stop
    endif
    
    do i = 1,N_itm
       do j = 1,N_node
          if (i==1) write(144,*) j,node_spr(j,0:max_spr_node)
          if (i==2) write(145,*) j,node_area(j,0:max_area_node)
       enddo
       if (i==1) write(144,*) " "
       if (i==2) write(145,*) " "
    enddo
    
    
    do i = 1,N_itm
       do j = 1,N_spr
          if (i==1) write(146,*) j,spr_node(j,0:max_node_spr)
          if (i==2) write(147,*) j,spr_area(j,0:max_area_spr)
       enddo
       if (i==1) write(146,*) " "
       if (i==2) write(147,*) " "
    enddo
    
    do i = 1,N_itm   
       do j = 1,N_cell
          if (i==1) write(148,*) j,area_node(j,0:max_node_area)
          if (i==2) write(149,*) j,area_spr(j,0:max_spr_area)
       enddo
       write(148,*) " "
       write(149,*) " "
    enddo
    
    close(144)
    close(145)
    close(146)
    close(147)
    close(148)
    close(149)
    
  end subroutine print_NI_trnsfrms_inFiles
  
  
end module mltiple_calls_together
