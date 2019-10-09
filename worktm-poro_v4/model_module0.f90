!         Author: Ruijie Liu
!
!
module truth 
   integer, parameter :: single = 4
   integer, parameter :: double = 8
   integer, parameter :: byte = 1
   integer, parameter :: max_char = 80
   integer, parameter :: zero = 0
   integer, parameter :: one = 1
   integer, parameter :: two = 2 
   integer, parameter :: three = 3   
 !  integer, parameter :: err_out = 0
   real(kind=double), parameter :: zero_d = 0.0_double
   real(kind=double), parameter :: one_d = 1.0_double
   real(kind=double), parameter :: half_d = 0.5_double   
   real(kind=double), parameter :: two_d = 2.0_double
   real(kind=double), parameter :: three_d = 3.0_double
  ! character(len=max_char) err_line1, err_line2
end module truth

!
!
!Parallel Data Structure
!
module my_cpu_commu
use truth
type cpu_commu
    integer ncpus_to_send
    integer ncpus_to_recv
    integer, pointer :: cpu_id_to_send(:)
    integer, pointer :: cpu_id_to_recv(:)
    integer, pointer :: node_tl_to_send(:)
    integer, pointer :: node_tl_to_recv(:)
    integer, pointer :: node_lid_to_send(:,:)
    integer, pointer :: node_lid_to_recv(:,:)
end type cpu_commu
end module my_cpu_commu

module sys_node
use truth
type node_obj
    integer dof
    integer affine(8)  !nodes sharing the same coordinates with this node
    integer tl_nb_elem !total number of elements related to this node
    integer tl_nb_node !total number of neiboring nodes
    integer nb_elem_id(8) !total number of elements related to this node
    integer nb_node_id(64) !total number of neiboring nodes
end type node_obj 
end module sys_node

module ele_strct
use truth
type element
    integer ele_type
    integer tl_node
    integer ngauss
    integer mat_no
    real(kind=double) center(3)
    real(kind=double) vstrain
    real(kind=double) pore_pressure
    real(kind=double) initial_stress(6)
    real(kind=double) initial_pore_pressure
    integer, pointer :: node(:)
    integer, pointer :: node_st_dof(:)
    integer, pointer :: gpt_yield(:)    
    real(kind=double), pointer :: gpt_strss(:,:)
    real(kind=double), pointer :: gpt_strss_t(:,:)
    real(kind=double), pointer :: gpt_strn(:,:) 
    real(kind=double), pointer :: gpt_strn_t(:,:)
    real(kind=double), pointer :: gpt_estrn(:,:) 
    real(kind=double), pointer :: gpt_estrn_t(:,:)
    real(kind=double), pointer :: gpt_pstrn(:,:) 
    real(kind=double), pointer :: gpt_pstrn_t(:,:)
    real(kind=double), pointer :: gpt_vstate(:,:) 
    real(kind=double), pointer :: gpt_vstate_t(:,:)  
end type element 
end module ele_strct

module boundary_face
use truth
type bnd_face
    integer orien
    integer element
    integer pressure
    real(kind=double) pressure_value
    real(kind=double) traction(3)
end type bnd_face
end module boundary_face


module mat_config                                     
use truth                                             
type material                                                                 
    integer model_id
    integer assoc_model 
    integer harden_id
    integer sld_prp_itm
    integer yield_prp_itm
    integer flow_prp_itm
    integer iso_prp_itm
    integer kin_prp_itm
    real(kind=double), pointer :: prpty_sld(:) 
    real(kind=double), pointer :: yield_para(:)       
    real(kind=double), pointer :: flow_para(:)       
    real(kind=double), pointer :: iso_harden(:)       
    real(kind=double), pointer :: kin_harden(:)   
end type material                                     
end module mat_config                                 

module model_type
use truth
use ele_strct
integer model,ele_type,ele_dof_3,ele_dof_4,first_time_step
integer node_dof,b_dof,ele_dof,sc,ngauss,strss_dof,dof_st
integer ele_node,ele_dof_1,ele_dof_2,ngauss_face,b_dof_L,b_dof_R
integer e_id_1,e_id_2,node_dof_L,node_dof_R,R_dof_st,L_dof_st
integer ele_node_L,ele_node_R,ele_dof_L,ele_dof_R,dof_dt
integer ele_node_1,ele_node_2,ele_node_3,ele_node_4,ele_L,ele_R
real(kind=double) L_2_u,L_2_delta_u,time_current
real(kind=double) wght_8(8),face_wght_4(4),flux,flux_fld,p0,bc_flux
real(kind=double) wght_27(27),face_wght_9(9),flux_sld,flux_tmp
real(kind=double) gauss_coord_8(8,3),gauss_coord_27(27,3),delta_p
!
!----solid system
!
integer jtime,no_iteration
type (element), allocatable :: hexa(:)            
real(kind=double), allocatable:: bmatx_L(:,:),bmatx_R(:,:) 
real(kind=double), allocatable:: N_matx_L(:,:),N_matx_R(:,:) 
real(kind=double), allocatable:: mat_L(:,:),mat_R(:,:) 
end module model_type

module system
use truth
use ele_strct
use mat_config
use sys_node
use boundary_face
use my_cpu_commu
!use crack_fracture
!
!system
!
integer ele_tl,node_tl,dof_tl,number_node_out,TOTAL_NR_NUMBER
integer bandL,load_only,NO_LU,NR_NUMBER,cut,nload,load_step
integer int_face_tl,linear,ynelem,mstep,sld_fld_tmp
integer tl_0_dch,dirnode,beta_zero,tl_non_zeros,mat_nprop_max
! added the following on 3rd may
integer bd_face_count
! added the above on 3rd may
integer gmsh_order(8),IPARS_order(8),IPARS_face_order(6)
integer hexa_face_node(4,6)
integer IPARS_face_node(4,6)
real(kind=double) load_scaling
real(kind=double) gravity_vector(3)
real(kind=double), allocatable:: coord(:,:),coord_n(:,:)
real load_scale,load_factor,deltaT,time_past,scale_point,load_factor_old
!
!solid subsystem
!
integer prpty_grp_sld,tl_load_point,cg_tl_node,pnode
integer flag_cpl,flag_gravity,flag_initial,flag_solve,tl_pressured_face,tl_traction_face
integer tl_cracked_face,tl_crack_bdface,total_frac_node,total_frac_elem
integer total_node_ipars,total_frac_interior_node
integer, allocatable:: node_out(:),cg_node(:,:),gstiff_row_index(:),gstiff_col_index(:)      
real(kind=double), allocatable:: gstiff(:),residue(:),gstiff_save(:)
real(kind=double), allocatable:: load_tl_zero(:),zero_value(:)
! added the following on 3rd may
real(kind=double), allocatable:: zero_add1(:),zero_add2(:)
! added the above on 3rd may
real(kind=double), allocatable:: u_n(:),u(:),delta_u(:),tiny_u(:),u_0(:)
real(kind=double), allocatable:: delta_load(:),delta_load_save(:),internal_force(:),internal_force_save(:)
real(kind=double), allocatable:: load_cnst(:),delta_load_naut(:),tl_load(:)
! added the following on 4th may
real(kind=double), allocatable:: load_cnst1(:),residue1(:)
! added the following on 4th may
real(kind=double), allocatable:: frac_node_coord(:,:),frac_node_width(:,:)
integer, allocatable:: frac_elem_connect(:,:),frac_node_global_id(:),frac_interior_node_id(:)
integer, allocatable:: frac_interior_node_affine(:) 
integer, allocatable:: zero_dch(:),node_st_dof(:),indx(:),dirbcnode(:),pnode_cn(:),cnode_p(:)
type (node_obj), allocatable :: node_nb(:)  
type (bnd_face), allocatable :: b_face(:) 
type (cpu_commu) my_cpu
!
!bw 
INTEGER TOTAL_DISP_BC, TOTAL_DISP_FBC,&
        TOTAL_TRAC_BC, TOTAL_TRAC_FBC,&
        TOTAL_FACE_LOAD_BC,&
        TOTAL_PRES_LOAD_BC,TOTAL_PRES_LOAD_FBC,&
        TOTAL_CRAC_FACE
REAL(KIND=DOUBLE),ALLOCATABLE:: TRAC_BC(:,:),TRAC_FBC(:,:),PRES_LOAD_FBC(:,:)
REAL(KIND=DOUBLE), ALLOCATABLE:: DISP_BC_RESIDUE(:),OFNODE_DISP_TMP(:,:)
INTEGER, ALLOCATABLE:: NODE_ST_GDOF(:),ACTIVE_NODE(:),HYPRE_ROWS(:),OFNODE_GNUM_TMP(:),&
                       LID_2IJK(:,:),ACTIVE_NODE_LID(:)

!NODE_ST_GDOF: START OF GLOBAL DOF FOR LOCAL NODE
!ACTIVE_NODE:  FLAG ARRAY, WHETHER THE NODE IS AN ACTIVE NODE IN CURRENT PROCESSOR

!bw
!
integer fld_tmp_prp_itm
integer ntime          
!                                                  
!----material groups                               
!                                                  
!                                                  
integer mat_grp_tl                                 
integer plastic_model,drucker_itm     
type (material), allocatable :: mat_prpty(:)       

! bag8
logical :: tecexists = .false.

end module system

module control    !bw control parameters for plasticity
use truth                                                
   integer, parameter :: max_iter = 10000 ! allowed maxmum iteration                                                          
   real(kind=double), parameter :: ytol = 1.e-3 ! yield tolerance
   real(kind=double), parameter :: rtol = 1.e-3 !  rooting tolerance
   real(kind=double), parameter :: sstol = 1.0e-3 ! substepping 
                                              ! tolerance   
   real(kind=double), parameter :: error_control = 1.0e-4 !tolerance 
   real(kind=double) MAT_NR_TOL
   real(kind=double) ELE_NR_TOL 
   real(kind=double), parameter :: TOLY = 1.0e-4 
   integer MAT_MAX_ITER  
   integer ELM_MAX_ITER 
   integer cut_back_flag
end module control

!module crack_fracture
!use truth
!type  crack
!    integer face_total_number
!    integer, pointer::face_shared_elm(:,:)
!    integer, pointer:: face_node_id(:,:)
!    real(kind=double), pointer :: face_node_coord(:,:,:)
!    real(kind=double), pointer :: face_node_jump_u(:,:,:)
!end type  crack 
!end module crack_fracture
!
