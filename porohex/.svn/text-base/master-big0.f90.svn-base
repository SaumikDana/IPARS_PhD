! master_big0.f90

subroutine module_elasticity2(&
        TIME,GCITER,&
        IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
        JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,&
        LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,&
        MYPRC,NUMPRC,NNTIM,&
        NUMFRAC,NUMFRACFACE,&
        MXFRAC,MXFRACFACE,FRACFACE,PFN,&
        XC,YC,ZC,KEYOUT_CR,GELEI,&
        ELEM_LID,NODE_LID,OFNODE_LID,&
        OFNODE_GID,OFNODE_L2GID,OFNODE_AFFINE,&
        OFNODE_KEYOUT,&
        MODUL,POISS,BIOTA,BIOTM,BULKDEN,&
        PASSO,YIELD_SIG0,YIELD_ALPHA,FLOW_ALPHA,&
        HARDEN_MODEL,HARDEN_C1,HARDEN_C2,&
        STRXX_INIT,STRYY_INIT,STRZZ_INIT,STRXY_INIT,STRYZ_INIT,&
        STRXZ_INIT,PRESS,PREF,&
        BDTYP1,BDTYP2,BDTYP3,BDTYP4,BDTYP5,BDTYP6,&
        BDDISP1,BDDISP2,BDDISP3,BDDISP4,BDDISP5,BDDISP6,&
        BDTRAC1,BDTRAC2,BDTRAC3,BDTRAC4,BDTRAC5,BDTRAC6,&
        BDFTRAC1,BDFTRAC2,BDFTRAC3,BDFTRAC4,BDFTRAC5,BDFTRAC6,&
        PRESFACE1,PRESFACE2,PRESFACE3,PRESFACE4,PRESFACE5,PRESFACE6,&
        CRAC_IBC_face,FNODE_TYPE,OFNODE_DISP,OFNODE_GNUM,&
        poro_neighbor,&
        node_displacement,&     ! ipars displacement u
        change_displacement,&   ! ipars change in displacement u-u0
        elm_vstrain,&           ! ipars element-based volume strain;
        rhs_at_n,&              ! prevous time step righ hand side load vector;
        gravity,&               ! gravity vector;
        solve_flag,&            ! 1: successfully solved.
        fracfaceproc,&
        PRESSVAL)
!
!Author: Ruijie Liu
!
!
!Poro-elasto-plasticity---mechanics solver
!
!
!use mpi
use truth
use control
use model_type
use system

implicit none
include 'emodel.h'
include 'mpif.h'

REAL*8  time,L2
integer solve_flag
integer ncase
integer :: dbg = 0

INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
INTEGER LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,MYPRC,NUMPRC,NNTIM,GCITER
INTEGER NUMFRAC,MXFRAC,MXFRACFACE,NUMFRACFACE(MXFRAC),&
        FRACFACE(4,MXFRACFACE,MXFRAC)
REAL*8  PFN(MXFRACFACE,MXFRAC)
REAL*8  XC(IDIM+1,JDIM+1,KDIM+1),YC(IDIM+1,JDIM+1,KDIM+1),&
        ZC(IDIM+1,JDIM+1,KDIM+1)
INTEGER KEYOUT_CR(IDIM,JDIM,KDIM),GELEI(IDIM,JDIM,KDIM)
INTEGER ELEM_LID(IDIM,JDIM,KDIM)
INTEGER NODE_LID(IDIM,JDIM,KDIM),OFNODE_LID(IDIM,JDIM,KDIM)
INTEGER OFNODE_GID(IDIM,JDIM,KDIM),OFNODE_L2GID(LFALLSIZE)
INTEGER OFNODE_AFFINE(9,LFALLSIZE),OFNODE_KEYOUT(LFALLSIZE)
INTEGER OFNODE_GNUM(GFSIZE)
INTEGER PASSO(IDIM,JDIM,KDIM),HARDEN_MODEL(IDIM,JDIM,KDIM)
REAL*8  OFNODE_DISP(3,GFSIZE)
REAL*8  MODUL(IDIM,JDIM,KDIM),POISS(IDIM,JDIM,KDIM),&
        BIOTA(IDIM,JDIM,KDIM),BIOTM(IDIM,JDIM,KDIM),&
        YIELD_SIG0(IDIM,JDIM,KDIM),YIELD_ALPHA(IDIM,JDIM,KDIM),&
        FLOW_ALPHA(IDIM,JDIM,KDIM),HARDEN_C1(IDIM,JDIM,KDIM),&
        HARDEN_C2(IDIM,JDIM,KDIM),&
        BULKDEN(IDIM,JDIM,KDIM),STRXX_INIT(IDIM,JDIM,KDIM),&
        STRYY_INIT(IDIM,JDIM,KDIM),STRZZ_INIT(IDIM,JDIM,KDIM),&
        STRXY_INIT(IDIM,JDIM,KDIM),STRYZ_INIT(IDIM,JDIM,KDIM),&
        STRXZ_INIT(IDIM,JDIM,KDIM),PRESS(IDIM,JDIM,KDIM),&
        PREF(IDIM,JDIM,KDIM)
INTEGER BDTYP1(JDIM,KDIM,3),BDTYP2(JDIM,KDIM,3),&
        BDTYP3(IDIM,KDIM,3),BDTYP4(IDIM,KDIM,3),&
        BDTYP5(IDIM,JDIM,3),BDTYP6(IDIM,JDIM,3)
REAL*8  BDDISP1(JDIM,KDIM,3),BDDISP2(JDIM,KDIM,3),&
        BDDISP3(IDIM,KDIM,3),BDDISP4(IDIM,KDIM,3),&
        BDDISP5(IDIM,JDIM,3),BDDISP6(IDIM,JDIM,3)
REAL*8  BDTRAC1(JDIM,KDIM,3),BDTRAC2(JDIM,KDIM,3),&
        BDTRAC3(IDIM,KDIM,3),BDTRAC4(IDIM,KDIM,3),&
        BDTRAC5(IDIM,JDIM,3),BDTRAC6(IDIM,JDIM,3)
REAL*8  BDFTRAC1(JDIM,KDIM,3),BDFTRAC2(JDIM,KDIM,3),&
        BDFTRAC3(IDIM,KDIM,3),BDFTRAC4(IDIM,KDIM,3),&
        BDFTRAC5(IDIM,JDIM,3),BDFTRAC6(IDIM,JDIM,3)
REAL*8  PRESFACE1(JDIM,KDIM),PRESFACE2(JDIM,KDIM),&
        PRESFACE3(IDIM,KDIM),PRESFACE4(IDIM,KDIM),&
        PRESFACE5(IDIM,JDIM),PRESFACE6(IDIM,JDIM)
REAL*8  PRESSVAL(IDIM,JDIM,KDIM)
INTEGER JL1,JL2,ST_DOF,ST_GDOF
INTEGER FNODE_TYPE(IDIM,JDIM,KDIM),FRACFACEPROC(MXFRACFACE,MXFRAC)
INTEGER no_iter,ielem,CTR,I,J,K,idof,index,inode,LID,&
        iload,igauss,itmp
integer poro_neighbor(6,idim*jdim*kdim)
real(kind=double) crac_ibc_face(3,total_cracked_face)
real(kind=double) node_displacement(idim,jdim,kdim,3)
real(kind=double) change_displacement(idim,jdim,kdim,3)
real(kind=double) elm_vstrain(idim,jdim,kdim)
real(kind=double) rhs_at_n(3*idim*jdim*kdim)
real(kind=double) gravity(3),scaling,L2_SUM,U3(3)
INTEGER ierr,fhandle,count,STATUS(MPI_STATUS_SIZE)
INTEGER(KIND=MPI_OFFSET_KIND) view
!
!initialize common block
!
flag_cpl=COUPLE_FLAG
flag_gravity=GRAVITY_FLAG
flag_initial=INITIAL_FLAG
flag_solve=solve_flag
!
!Ruijie add plasticity: initialize nonlinear iteration control parameters
!
if(NNTIM==2) return
MAT_NR_TOL = MAT_TOL
ELE_NR_TOL = EP_TOL
MAT_MAX_ITER = MAX_ITERATION_LOC
ELM_MAX_ITER = MAX_ITERATION_GL
cut_back_flag=0
EP_CUTBACK_FLAG=0

node_tl=LALLSIZE+LFALLSIZE
ele_tl=LALLELEM
mat_grp_tl=LALLELEM
mat_nprop_max=10
tl_cracked_face=total_cracked_face
gravity_vector=gravity(1:3)
if (flag_gravity.eq.0) gravity_vector=0.d0
call timon(43)

!--------------------------------------------------------------------
! bag8 - allows time dependent mechanics boundary conditions
!--------------------------------------------------------------------
IF (MECH_BC_NCASE.NE.0) THEN
  CALL CHANGE_MECH_BC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
        JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,TIME,&
        MYPRC,NUMPRC,NNTIM,&
        XC,YC,ZC,KEYOUT_CR,NODE_LID,&
        MODUL,POISS,BIOTA,BIOTM,BULKDEN,&
        PASSO,YIELD_SIG0,YIELD_ALPHA,FLOW_ALPHA,&
        HARDEN_MODEL,HARDEN_C1,HARDEN_C2,&
        STRXX_INIT,STRYY_INIT,STRZZ_INIT,STRXY_INIT,STRYZ_INIT,&
        STRXZ_INIT,PRESS,PREF,&
        BDTYP1,BDTYP2,BDTYP3,BDTYP4,BDTYP5,BDTYP6,&
        BDDISP1,BDDISP2,BDDISP3,BDDISP4,BDDISP5,BDDISP6,&
        BDTRAC1,BDTRAC2,BDTRAC3,BDTRAC4,BDTRAC5,BDTRAC6,&
        PRESFACE1,PRESFACE2,PRESFACE3,PRESFACE4,PRESFACE5,PRESFACE6,&
        PRESSVAL)
ENDIF
!--------------------------------------------------------------------

call PRE_PRCSS2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
        JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,&
        LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,&
        MYPRC,NUMPRC,NNTIM,&
        NUMFRAC,NUMFRACFACE,&
        MXFRAC,MXFRACFACE,FRACFACE,PFN,&
        XC,YC,ZC,KEYOUT_CR,GELEI,&
        ELEM_LID,NODE_LID,OFNODE_LID,&
        OFNODE_GID,OFNODE_L2GID,OFNODE_AFFINE,&
        OFNODE_KEYOUT,&
        MODUL,POISS,BIOTA,BIOTM,BULKDEN,&
        PASSO,YIELD_SIG0,YIELD_ALPHA,FLOW_ALPHA,&
        HARDEN_MODEL,HARDEN_C1,HARDEN_C2,&
        STRXX_INIT,STRYY_INIT,STRZZ_INIT,STRXY_INIT,STRYZ_INIT,&
        STRXZ_INIT,PRESS,PREF,&
        BDTYP1,BDTYP2,BDTYP3,BDTYP4,BDTYP5,BDTYP6,&
        BDDISP1,BDDISP2,BDDISP3,BDDISP4,BDDISP5,BDDISP6,&
        BDTRAC1,BDTRAC2,BDTRAC3,BDTRAC4,BDTRAC5,BDTRAC6,&
        BDFTRAC1,BDFTRAC2,BDFTRAC3,BDFTRAC4,BDFTRAC5,BDFTRAC6,&
        PRESFACE1,PRESFACE2,PRESFACE3,PRESFACE4,PRESFACE5,PRESFACE6,&
        CRAC_IBC_FACE,FNODE_TYPE,FRACFACEPROC)
call timoff(43)
ierr=0
!
!Ruijie add nonlinear iteration procedures for solving plasticity problems
!
   !
   !Select a stress-like parameter for nondimensionalizing
   !residual norm
   !
if (myprc.eq.0) then
   scaling=1.0d0
   !if(abs(hexa(1)%initial_pore_pressure)>1.0e-6) then
   !   scaling=abs(hexa(1)%initial_pore_pressure)
   !else
   !   scaling=mat_prpty(1)%prpty_sld(1)/100.0d0
   !end if
endif
call MPI_Bcast(scaling, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
load_scale=1.0d0
if(NNTIM.NE.0) then
   no_iter=0
   L2=1.0d0
   delta_u=0.0d0
   NODE_DISPLACEMENT=0.0d0
   do while(.true.)
      call timon(44)
      call g_stiff(MODEL_EP,NNTIM,cut_back_flag,no_iter,gciter)

! bag8 - if one processor cuts back, then all must
      itmp = cut_back_flag
      CALL mpi_allreduce(itmp,cut_back_flag,1,MPI_INTEGER,MPI_MAX,&
                         MPI_COMM_WORLD,IERR)

      if(cut_back_flag>0) then
         if (myprc.eq.0) then
           write(*,*)'No Convergence at a Material Point.', &
             ' Time Step Will be Reduced'
         endif
         EP_CUTBACK_FLAG=1
         RETURN
      endif

      call load
      !
      !setting displacement B.C.to
      !all zero after the first iteration
      !
      ncase=3
      if(no_iter==0) then
        ncase=1
      end if
      if(MODEL_EP<1.and.no_iter>0) then
         ncase=2
      end if
      call bc(ncase,gciter)   !bw Dirichlet Boundary Condition
      !
      !Compute L2 based on active node!!!!
      !
      L2=0.0d0
      I=0
      do inode=1,node_tl
         !
         !check if it is a active node
         !
         if(ACTIVE_NODE(inode)>0) then
           I=I+1
           L2=L2+RESIDUE(NODE_ST_DOF(inode))*&
                 RESIDUE(NODE_ST_DOF(inode))+&
                 RESIDUE(NODE_ST_DOF(inode)+1)*&
                 RESIDUE(NODE_ST_DOF(inode)+1)+&
                 RESIDUE(NODE_ST_DOF(inode)+2)*&
                 RESIDUE(NODE_ST_DOF(inode)+2)
         end if
      end do
      L2=L2/scaling/scaling
      L2_SUM=0.0d0
      if(NUMPRC>1) then
        call MPI_Allreduce(L2, L2_SUM, 1, MPI_DOUBLE_PRECISION, &
                           MPI_SUM,MPI_COMM_WORLD,ierr)
        L2=sqrt(L2_SUM)
      else
        L2=sqrt(L2)
      end if
      if (myprc.eq.0) then
        if (model_ep.eq.0) then
          write(*,'(1P,A,I4,A,E11.4)')' Elasticity Newton iter = ',&
            no_iter,', nonlin. resid = ',L2
        else
          write(*,'(1P,A,I4,A,E11.4)')' Plasticity Newton iter = ',&
            no_iter,', nonlin. resid = ',L2
        endif
      endif
      call timoff(44)

! bag8
      if ((model_ep.gt.0).or.(.not.no_elastic_newton)) then
        if (L2<EP_TOL.and.no_iter>0) goto 50
      elseif ((model_ep.eq.0).and.(no_elastic_newton)) then
        if (no_iter.eq.1) goto 50
      endif

! SOLVER ROUTINE

      tiny_u=0.0d0

      if(EP_SOLVER_FLAG==0) then
        CALL HYPRE_POROHEX(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
          KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,GSTIFF,&
          GSTIFF_ROW_INDEX,GSTIFF_COL_INDEX,ACTIVE_NODE,RESIDUE,&
          TL_LOAD,NODE_ST_DOF,NODE_ST_GDOF,&
          HYPRE_ROWS, OFNODE_DISP,&
          OFNODE_DISP_TMP,OFNODE_KEYOUT,NNTIM,TL_NON_ZEROS,NODE_TL,&
          OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)
$SUPERLU      elseif(EP_SOLVER_FLAG==1) then
$SUPERLU        CALL SUPERLU_POROHEX(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
$SUPERLU          JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
$SUPERLU          GSTIFF,GSTIFF_ROW_INDEX,GSTIFF_COL_INDEX,&
$SUPERLU          ACTIVE_NODE,RESIDUE,TL_LOAD,NODE_DISPLACEMENT,&
$SUPERLU          NODE_ST_DOF,NODE_ST_GDOF,HYPRE_ROWS,OFNODE_DISP,&
$SUPERLU          OFNODE_DISP_TMP,OFNODE_KEYOUT,NNTIM,TL_NON_ZEROS,&
$SUPERLU          NODE_TL,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)
      elseif(EP_SOLVER_FLAG==2) then
        call PARDISO_POROHEX(NUMPRC)  ! intel direct sparse solver pardiso
      else
        stop 'Unknown EP_SOLVER_FLAG'
      end if

      !
      !Fill solution for active nodes
      !
      if(numprc>1) then
        do inode=1,IUPPER-ILOWER+1
           LID=active_node_lid(inode)
           tiny_u(3*(active_node_lid(inode)-1)+1:&
                   3*active_node_lid(inode))=&
           RESIDUE(3*(inode-1)+1:3*inode)
        end do
      !
      !Fill solution for ghost nodes
      !
        call fem_update_ghost_node_solution(myprc,numprc)
      else
        tiny_u=RESIDUE
      end if
      !
      !Updating incremental displacement
      !
      delta_u=delta_u+tiny_u
      u=u_n+delta_u

      no_iter=no_iter+1
      if(no_iter.gt.ELM_MAX_ITER) then
         EP_CUTBACK_FLAG=1
         write(*,*) 'No Convergence in solid N-R iteration. Cut time step.'
         RETURN
      end if
      !!
      !!set up initial guess for hypre solution
      !!
      !CTR=0
      !DO K=1,KDIM
      !   DO J=1,JDIM
      !      DO I=IL1,IL2+1
      !         IF (KEYOUT_CR(I,J,K).EQ.1) THEN
      !            CTR=CTR+1
      !            NODE_DISPLACEMENT(I,J,K,1)=RESIDUE((CTR-1)*3+1)
      !            NODE_DISPLACEMENT(I,J,K,2)=RESIDUE((CTR-1)*3+2)
      !            NODE_DISPLACEMENT(I,J,K,3)=RESIDUE((CTR-1)*3+3)
      !         ENDIF
      !      ENDDO
      !   ENDDO
      !ENDDO
      NODE_DISPLACEMENT=0.0d0
    end do  !N-R iteration at element level convergres

50  continue
    call updating_after_coupled_field_converge

    TL_LOAD=u
    CALL POROHEX_COPY_EDISP(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
            KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
            TL_LOAD,NODE_DISPLACEMENT,&
            NODE_ST_DOF,NODE_ST_GDOF,&
            OFNODE_DISP,OFNODE_DISP_TMP,OFNODE_KEYOUT,NODE_TL,&
            NODE_LID,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)

    TL_LOAD=u-u_0
    CALL POROHEX_COPY_EDISP(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
            KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
            TL_LOAD,CHANGE_DISPLACEMENT,&
            NODE_ST_DOF,NODE_ST_GDOF,&
            OFNODE_DISP,OFNODE_DISP_TMP,OFNODE_KEYOUT,NODE_TL,&
            NODE_LID,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)

! saumik - moved vstrain calculation to post_prcss2
!
!    call get_vstrain_at_element_centor
!    !
!    !Transfer to IPARS IJK for element volume total strain
!    !
!    DO K=KL1-1,KL2+1
!       JL1 = MIN(JL1V(K-1),JL1V(K),JL1V(K+1))
!       JL2 = MAX(JL2V(K-1),JL2V(K),JL2V(K+1))
!       DO J=JL1-1,JL2+1
!          DO I=IL1,IL2
!          ielem=elem_lid(i,j,k)
!          if (ielem.gt.0) then
!             elm_vstrain(i,j,k)=hexa(ielem)%vstrain
!          endif
!          enddo
!        enddo
!     enddo

ELSE    !solve for initialization

   do ielem =1, ele_tl
      hexa(ielem)%gpt_strss_t(1:8,1:6)=0.0d0
   end do

   do iload=1,NLOADSTEPS

      load_scaling = iload/real(NLOADSTEPS)

      if (myprc.eq.0) then
        write(*,*)repeat('-',72)
        write(*,'(1p,a,i4,a,i4,a,e11.4)')'Load step ',iload,' of ',NLOADSTEPS,&
          ' ,factor=',load_scaling
      endif

      !
      !proportionally add input initial stress
      !
       do ielem =1, ele_tl
          do igauss=1,8
           hexa(ielem)%gpt_strss_t(igauss,1:6) = &
             hexa(ielem)%gpt_strss_t(igauss,1:6) + &
             hexa(ielem)%initial_stress(1:6)/real(NLOADSTEPS)
          end do
       end do
      !
      no_iter=0
      L2=1.0d0
      delta_u=0.0d0
      NODE_DISPLACEMENT=0.0d0
      do while(.true.)
         call timon(44)
         call g_stiff(MODEL_EP,NNTIM,cut_back_flag,no_iter,gciter)
         !
         !assume no cut_back
         !(if cut back, NO RETURUN but MUST in the do loop iload!!!
      call load
      !
      !setting displacement B.C.to
      !all zero after the first iteration
      !
      ncase=3
      if(no_iter==0) then
        ncase=1
      end if
      if(MODEL_EP<1.and.no_iter>0) then
         ncase=2
      end if
      call bc(ncase,gciter)   !bw Dirichlet Boundary Condition
      !
      !Compute L2 based on active node!!!!
      !
      L2=0.0d0
      I=0
      do inode=1,node_tl
         !
         !check if it is a active node
         !
         if(ACTIVE_NODE(inode)>0) then
           I=I+1
           L2=L2+RESIDUE(NODE_ST_DOF(inode))*&
                 RESIDUE(NODE_ST_DOF(inode))+&
                 RESIDUE(NODE_ST_DOF(inode)+1)*&
                 RESIDUE(NODE_ST_DOF(inode)+1)+&
                 RESIDUE(NODE_ST_DOF(inode)+2)*&
                 RESIDUE(NODE_ST_DOF(inode)+2)
         end if
      end do
      L2=L2/scaling/scaling
      L2_SUM=0.0d0
      if(NUMPRC>1) then
        call MPI_Allreduce(L2, L2_SUM, 1, MPI_DOUBLE_PRECISION, &
                           MPI_SUM,MPI_COMM_WORLD,ierr)
        L2=sqrt(L2_SUM)
      else
        L2=sqrt(L2)
      end if
      if (myprc.eq.0) then
        if (model_ep.eq.0) then
          write(*,'(1P,A,I4,A,E11.4)')' Elasticity Newton iter = ',&
            no_iter,', nonlin. resid = ',L2
        else
          write(*,'(1P,A,I4,A,E11.4)')' Plasticity Newton iter = ',&
            no_iter,', nonlin. resid = ',L2
        endif
      endif
      call timoff(44)

! bag8
      if ((model_ep.gt.0).or.(.not.no_elastic_newton)) then
        if (L2<EP_TOL.and.no_iter>0) goto 100
      elseif ((model_ep.eq.0).and.(no_elastic_newton)) then
        if (no_iter.eq.1) goto 100
      endif

! SOLVER ROUTINE

      tiny_u=0.0d0

      if(EP_SOLVER_FLAG==0) then
        CALL HYPRE_POROHEX(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
          KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,GSTIFF,&
          GSTIFF_ROW_INDEX,GSTIFF_COL_INDEX,ACTIVE_NODE,RESIDUE,&
          TL_LOAD,NODE_ST_DOF,NODE_ST_GDOF,&
          HYPRE_ROWS, OFNODE_DISP,&
          OFNODE_DISP_TMP,OFNODE_KEYOUT,NNTIM,TL_NON_ZEROS,NODE_TL,&
          OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)
$SUPERLU      elseif(EP_SOLVER_FLAG==1) then
$SUPERLU        CALL SUPERLU_POROHEX(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
$SUPERLU          JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
$SUPERLU          GSTIFF,GSTIFF_ROW_INDEX,GSTIFF_COL_INDEX,&
$SUPERLU          ACTIVE_NODE,RESIDUE,TL_LOAD,NODE_DISPLACEMENT,&
$SUPERLU          NODE_ST_DOF,NODE_ST_GDOF,HYPRE_ROWS,OFNODE_DISP,&
$SUPERLU          OFNODE_DISP_TMP,OFNODE_KEYOUT,NNTIM,TL_NON_ZEROS,&
$SUPERLU          NODE_TL,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)
      elseif(EP_SOLVER_FLAG==2) then
        call PARDISO_POROHEX(NUMPRC)  ! intel direct sparse solver pardiso
      else
        stop 'Unknown EP_SOLVER_FLAG'
      end if

      !
      !Fill solution for active nodes
      !
      if(numprc>1) then
        do inode=1,IUPPER-ILOWER+1
           LID=active_node_lid(inode)
           tiny_u(3*(active_node_lid(inode)-1)+1:&
                   3*active_node_lid(inode))=&
           RESIDUE(3*(inode-1)+1:3*inode)
        end do
        !
        !Fill solution for ghost nodes
        !
        call fem_update_ghost_node_solution(myprc,numprc)
      else
        tiny_u=RESIDUE
      end if
      !
      ! Updating incremental displacement
      !
      delta_u=delta_u+tiny_u
      u=u_n+delta_u

      no_iter=no_iter+1
      if(no_iter.gt.ELM_MAX_ITER) then
         if (myprc.eq.0) then
           write(*,*) 'No Convergence in solid N-R iteration during init...'
           write(*,*) 'Try increasing loading steps.'
         endif
         STOP 99
      end if
      NODE_DISPLACEMENT=0.0d0
    end do  !N-R iteration at element level convergres
    !
    !Once this load step converges, updating displacement, strain..... HERE!!!
    !because updating here, we do want to cancel another call outside the
    !solid!!!
    !
100 continue
    call updating_after_coupled_field_converge
    !
    !Re-initilize displacement and strain  to zero and
    !Setup total effective stresses to initial stresses
    !if this is for initilization
    !
  end do

  u_0=u

  TL_LOAD=u_0
  CALL POROHEX_COPY_EDISP(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
            KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
            TL_LOAD,NODE_DISPLACEMENT,&
            NODE_ST_DOF,NODE_ST_GDOF,&
            OFNODE_DISP,OFNODE_DISP_TMP,OFNODE_KEYOUT,NODE_TL,&
            NODE_LID,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)

! saumik - moved vstrain calculation to post_prcss2
!
!  call get_vstrain_at_element_centor
!  !
!  !Transfer to IPARS IJK for element volume total strain
!  !
!  DO K=KL1-1,KL2+1
!     JL1 = MIN(JL1V(K-1),JL1V(K),JL1V(K+1))
!     JL2 = MAX(JL2V(K-1),JL2V(K),JL2V(K+1))
!     DO J=JL1-1,JL2+1
!        DO I=IL1,IL2
!           ielem=elem_lid(i,j,k)
!           if (ielem.gt.0) then
!              elm_vstrain(i,j,k)=hexa(ielem)%vstrain
!           endif
!        enddo
!     enddo
!  enddo

END IF

return
end

!----------------------------------------------------------------------

SUBROUTINE MODULE_POST_PRCSS(&
        TIME,&           ! CURRENT SOLUTION TIME
        ICPL_FLAG,&      ! ICPL_FLAG=1: FIXED-STRESS; 2: UNDRAINED
        GRAVITY_FLAG,&   ! 1: GRAVITY TAKEN INTO ACCOUNT
        INITIAL_FLAG,&   ! 1: INITIAL TOTAL STRESS AND PORE PRESSURE CONSIDERED
        IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
        JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,&
        LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,&
        MYPRC,NUMPRC,NNTIM,&
        NUMFRAC,NUMFRACFACE,TOTAL_CRACKED_FACE,&
        MXFRAC,MXFRACFACE,FRACFACE,PFN,&
        KEYOUT_CR,GELEI,&
        ELEM_LID,NODE_LID,OFNODE_LID,&
        OFNODE_GID,OFNODE_L2GID,OFNODE_AFFINE,&
        OFNODE_KEYOUT,&
        CRAC_IBC_FACE,FNODE_TYPE,OFNODE_DISP,&
        OFNODE_GNUM,&
        PORO_NEIGHBOR,&
        NODE_DISPLACEMENT,&        !OUTPUT NODE DISPLACEMENT;
        NODE_STRESS,&              !OUTPUT NODE STRESS
        NODE_STRAIN,&              !OUTPUT NODE STRAIN
        ELEM_VSTRAIN,&             !OUTPUT ELEMENT STRAIN
        NODE_PSTRAIN,&             !OUTPUT PLASTIC STRAIN
        NODE_PSTATE,&              !OUTPUT PLASTIC STATE VARIABLES
        FRAC_WIDTH,&               !FRACTURE ELEMENT WIDTH
        FRAC_WIDTH_TMP,&
        NODE_WIDTH,&
        RHS_AT_N,&        ! PREVOUS TIME STEP RIGH HAND SIDE LOAD VECTOR;
        GRAVITY,&         ! GRAVITY VECTOR;
        SOLVE_FLAG,&      ! 1: SUCCESSFULLY SOLVED.
        FRACFACEPROC,&
        MODEL_EP)
!BW SOLVE_FLAG=0: SUCCESSFULLY SOLVED
!
!
!
!
!AUTHOR: RUIJIE LIU
!
!
!POROELASTICITY---ELASTIC SOLVER
!
!
USE TRUTH
USE CONTROL
USE MODEL_TYPE
USE SYSTEM
IMPLICIT NONE
REAL(KIND=DOUBLE) TIME
INTEGER ICPL_FLAG
INTEGER GRAVITY_FLAG
INTEGER INITIAL_FLAG
INTEGER SOLVE_FLAG
INTEGER NCASE
INTEGER :: DBG = 0

INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
INTEGER LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,MYPRC,NUMPRC,NNTIM,MODEL_EP
INTEGER NUMFRAC,MXFRAC,MXFRACFACE,NUMFRACFACE(MXFRAC),&
        FRACFACE(4,MXFRACFACE,MXFRAC)
REAL*8  PFN(MXFRACFACE,MXFRAC)
INTEGER KEYOUT_CR(IDIM,JDIM,KDIM),GELEI(IDIM,JDIM,KDIM)
INTEGER ELEM_LID(IDIM,JDIM,KDIM)
INTEGER NODE_LID(IDIM,JDIM,KDIM),OFNODE_LID(IDIM,JDIM,KDIM)
INTEGER OFNODE_GID(IDIM,JDIM,KDIM),OFNODE_L2GID(LFALLSIZE)
INTEGER OFNODE_AFFINE(9,LFALLSIZE),OFNODE_KEYOUT(LFALLSIZE)
REAL*8  OFNODE_DISP(3,GFSIZE)
INTEGER TOTAL_CRACKED_FACE
INTEGER FNODE_TYPE(IDIM,JDIM,KDIM)

INTEGER OFNODE_GNUM(GFSIZE)
INTEGER FRACFACEPROC(MXFRACFACE,MXFRAC)
INTEGER PORO_NEIGHBOR(6,IDIM*JDIM*KDIM)
REAL(KIND=DOUBLE) CRAC_IBC_FACE(3,TOTAL_CRACKED_FACE)
REAL(KIND=DOUBLE) NODE_DISPLACEMENT(IDIM,JDIM,KDIM,3)
REAL(KIND=DOUBLE) NODE_STRESS(IDIM,JDIM,KDIM,6)
REAL(KIND=DOUBLE) NODE_STRAIN(IDIM,JDIM,KDIM,6)
REAL(KIND=DOUBLE) ELEM_VSTRAIN(IDIM,JDIM,KDIM)
REAL(KIND=DOUBLE) NODE_PSTRAIN(IDIM,JDIM,KDIM,6)
REAL(KIND=DOUBLE) NODE_PSTATE(IDIM,JDIM,KDIM,3)
REAL(KIND=DOUBLE) NODE_WIDTH(IDIM,JDIM,KDIM)
REAL(KIND=DOUBLE) RHS_AT_N(3*IDIM*JDIM*KDIM)
REAL(KIND=DOUBLE) GRAVITY(3)
REAL(KIND=DOUBLE) FRAC_WIDTH(MXFRACFACE,MXFRAC)
REAL(KIND=DOUBLE) FRAC_WIDTH_TMP(MXFRACFACE,MXFRAC)

! COPY UPDATED DISPLACEMENT INFO FROM IPARS ARRAY INTO POROHEX ARRAY TL_LOAD
!IF (NNTIM.NE.2) THEN

! moved to module_elasticity above for EDISP and DDISP
!   CALL POROHEX_COPY_EDISP(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,&
!             KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEI,&
!             TL_LOAD,NODE_DISPLACEMENT,NODE_ST_DOF,NODE_ST_GDOF,&
!             OFNODE_DISP,OFNODE_DISP_TMP,OFNODE_KEYOUT,NODE_TL,&
!             NODE_LID,OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)

!   call timon(47)
!   CALL UPDATE_2
!   call timoff(47)
   call timon(45)
   CALL POST_PRCSS2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
           JL1V,JL2V,KL1,KL2,KEYOUT,NBLK,&
           LSIZE,ILOWER,IUPPER,&
           LFSIZE,IFLOWER,IFUPPUER,&
           LALLSIZE,LFALLSIZE,LALLELEM,&
           GSIZE,GFSIZE,&
           MYPRC,NUMPRC,NNTIM,&
           NUMFRAC,NUMFRACFACE,TOTAL_CRACKED_FACE,&
           MXFRAC,MXFRACFACE,FRACFACE,PFN,&
           KEYOUT_CR,GELEI,&
           ELEM_LID,NODE_LID,OFNODE_LID,&
           OFNODE_GID,OFNODE_L2GID,OFNODE_AFFINE,&
           OFNODE_KEYOUT,OFNODE_GNUM,OFNODE_DISP,&
           NODE_DISPLACEMENT,&
           NODE_STRESS,NODE_STRAIN,ELEM_VSTRAIN,NODE_PSTRAIN,&
           NODE_PSTATE,NODE_WIDTH,&
           FRAC_WIDTH,FRAC_WIDTH_TMP,FRACFACEPROC,MODEL_EP)
   call timoff(45)
!ENDIF
!bw  IF (NUMPRC.EQ.1) THEN
!bw     if(total_cracked_face>0) then
!bw        !
!bw        !computing crack jump and plot
!bw
!bw       call frac_tecplot(time,node_coord,elm_gnode_id,frac_width, &
!bw          crack_ibc_face,elm_pore_pressure,poro_neighbor)
!bw
!bw     end if
!bw  ENDIF
!
!call for_gmsh(node_displacement,node_stress,node_strain)
!

! AT THE END OF SIMULATION, CLEAN UP COMMON BLOCK MEMORY
!IF (NNTIM.EQ.2) THEN
!   CALL FREE_MEMORY
!ENDIF
RETURN
END



subroutine fem_parallel(ILOWER,IUPPER)
!use mpi
use truth
use system
implicit none
include 'mpif.h'
INTEGER ILOWER,IUPPER,ierr,&
        NODE_GID,inode,jnode,knode,mycpu,numcpus,icpu,jcpu,kcpu,&
        Add_cpu,nodes_tl_to_recv,count,i_index,&
        cpu_read_tl,cpu_read_id,read_node_st,GID,LID,&
        max_node_read,node_to_read
INTEGER fetch_two(2),start_end(2)
integer, allocatable :: cpu_ilower(:),cpu_iupper(:),&
  node_gid_to_recv(:,:),node_gid_to_send(:,:),&
  cpus_recv_tl_set(:),node_tl_to_recv_set(:),&
  cpu_recv_address_start(:),recv_info(:),sum_node_tl(:)
integer STATUS(MPI_STATUS_SIZE),fhandle
INTEGER(KIND=MPI_OFFSET_KIND) view
call MPI_Comm_Rank(MPI_COMM_WORLD, mycpu, ierr)
call MPI_Comm_Size(MPI_COMM_WORLD, numcpus, ierr)

allocate(cpu_ilower(numcpus),stat=ierr)
allocate(cpu_iupper(numcpus),stat=ierr)

call MPI_File_Open(MPI_COMM_WORLD, 'fetch_out', &
        MPI_MODE_RDWR+&
        MPI_MODE_CREATE, MPI_INFO_NULL, fhandle, ierr)
call MPI_File_CLOSE(fhandle,ierr)

start_end(1)=ILOWER
start_end(2)=IUPPER
!
!Primary and Active Nodes (Global Node Range)
!
if(mycpu.ne.0) then
   call MPI_SEND(start_end, 2, MPI_INTEGER, 0, &
     100+mycpu, MPI_COMM_WORLD, ierr )
else
   cpu_ilower(1)=ILOWER
   cpu_iupper(1)=IUPPER
   do icpu=1,numcpus-1
      call MPI_RECV(start_end, 2, MPI_INTEGER, icpu, &
        100+icpu, MPI_COMM_WORLD, status, ierr )
      cpu_ilower(icpu+1)=start_end(1)
      cpu_iupper(icpu+1)=start_end(2)
   end do
end if
call MPI_BARRIER(MPI_COMM_WORLD,ierr)
call MPI_BCAST(cpu_ilower,numcpus,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call MPI_BCAST(cpu_iupper,numcpus,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!
!compute active_node_lid
!
ALLOCATE(ACTIVE_NODE_LID(IUPPER-ILOWER+1))
do inode=1, node_tl
   if(ACTIVE_NODE(inode).eq.1) then
       NODE_GID=int((NODE_ST_GDOF(inode)-1)/3)+1
       LID=NODE_GID-ILOWER+1
       ACTIVE_NODE_LID(LID)=inode
   end if
end do
!
!TOTAL number of other CPUS from which this cpu needs to fetch
!
! loops for ghost nodes in this cpu
!
allocate(my_cpu%cpu_id_to_recv(numcpus))
my_cpu%cpu_id_to_recv=-1
my_cpu%ncpus_to_recv=0
my_cpu%cpu_id_to_recv=0
do inode=1, node_tl
   if(ACTIVE_NODE(inode).ne.1) then
      NODE_GID=int((NODE_ST_GDOF(inode)-1)/3)+1
      do icpu=0,numcpus-1
         if(icpu==mycpu) cycle
         if(NODE_GID>=cpu_ilower(icpu+1).and.&
            NODE_GID<=cpu_iupper(icpu+1)) then
            !
            !need to fetch from this cpu
            !
            if(my_cpu%ncpus_to_recv==0) then
               my_cpu%ncpus_to_recv=1
               my_cpu%cpu_id_to_recv(1)=icpu
            else
                !
                !loop over available known cpus
                !
                Add_cpu=1
                do jcpu=1,my_cpu%ncpus_to_recv
                   if(my_cpu%cpu_id_to_recv(jcpu)==icpu) then
                      Add_cpu=0
                      exit
                   end if
                end do
                if(Add_cpu==1) then
                  my_cpu%ncpus_to_recv=my_cpu%ncpus_to_recv+1
                  my_cpu%cpu_id_to_recv(my_cpu%ncpus_to_recv)=icpu
                end if
            end if
         end if
        end do
     end if
end do


allocate(my_cpu%node_tl_to_recv(my_cpu%ncpus_to_recv),&
         my_cpu%node_lid_to_recv(my_cpu%ncpus_to_recv,node_tl),&
         node_gid_to_recv(my_cpu%ncpus_to_recv,node_tl))
!
!nodes need to fetch from other cpus
!

my_cpu%node_tl_to_recv=0
my_cpu%node_lid_to_recv=0
node_gid_to_recv=0
do inode=1, node_tl
   if(ACTIVE_NODE(inode).ne.1) then
      NODE_GID=int((NODE_ST_GDOF(inode)-1)/3)+1
      do icpu=0,numcpus-1
         if(icpu==mycpu) cycle
         if(NODE_GID>=cpu_ilower(icpu+1).and.&
            NODE_GID<=cpu_iupper(icpu+1)) then
                !
                !loop over ncpus_to_recv
                !
                do jcpu=1,my_cpu%ncpus_to_recv
                   if(my_cpu%cpu_id_to_recv(jcpu)==icpu) then
 my_cpu%node_tl_to_recv(jcpu)=my_cpu%node_tl_to_recv(jcpu)+1
 my_cpu%node_lid_to_recv(jcpu,my_cpu%node_tl_to_recv(jcpu))=inode
 node_gid_to_recv(jcpu,my_cpu%node_tl_to_recv(jcpu))=NODE_GID
                   end if
                end do
         end if
      end do
   end if
end do
allocate(sum_node_tl(numcpus))
nodes_tl_to_recv=0
do jcpu=1,my_cpu%ncpus_to_recv
   nodes_tl_to_recv=nodes_tl_to_recv+my_cpu%node_tl_to_recv(jcpu)
end do
!
!Determine cpu_fetch_address_start
!(for writting each cpu fetching information out)
!
ALLOCATE(cpus_recv_tl_set(numcpus),&
      node_tl_to_recv_set(numcpus),&
   cpu_recv_address_start(numcpus))
if(mycpu.ne.0) then
   fetch_two(1)=my_cpu%ncpus_to_recv
   fetch_two(2)=nodes_tl_to_recv
   call MPI_SEND(fetch_two, 2, MPI_INTEGER, 0, &
   100+mycpu, MPI_COMM_WORLD, ierr )
else
   cpus_recv_tl_set(1)=my_cpu%ncpus_to_recv
   node_tl_to_recv_set(1)=nodes_tl_to_recv
   do icpu=1,numcpus-1
      call MPI_RECV(fetch_two, 2, MPI_INTEGER, icpu, &
      100+icpu, MPI_COMM_WORLD, status, ierr )
      cpus_recv_tl_set(icpu+1)=fetch_two(1)
      node_tl_to_recv_set(icpu+1)=fetch_two(2)
   end do
end if
call MPI_BARRIER(MPI_COMM_WORLD,ierr)
call MPI_BCAST(cpus_recv_tl_set,numcpus,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call MPI_BCAST(node_tl_to_recv_set,numcpus,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
if(mycpu==0) then
   cpu_recv_address_start(1)=0
   do icpu=1,numcpus-1
      cpu_recv_address_start(icpu+1)=&
      cpu_recv_address_start(icpu)+1+2*cpus_recv_tl_set(icpu)+&
      node_tl_to_recv_set(icpu)
   end do
   max_node_read=maxval(node_tl_to_recv_set(1:numcpus))
end if
call MPI_BCAST(cpu_recv_address_start,numcpus,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call MPI_BCAST(max_node_read,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
DEALLOCATE(node_tl_to_recv_set,cpus_recv_tl_set)
!
!
!We are now ready to write out receiving information into a file
!
!
!
!Write out (in order): (a) total number of other cpus to fetch (number of
!integer: 1);
!                      (b) each fetched cpu ID (number of integer: ncpu_fetch)
!                      (c) total number of nodes in each fetched cpu (number of
!                      integer: ncpu_fetch)
!                      (d) global node ID to write out (number of integer:
!                      nodes_tl_to_fetch)
!
call MPI_File_Open(MPI_COMM_WORLD, 'fetch_out', &
     MPI_MODE_WRONLY, MPI_INFO_NULL, fhandle, ierr)
ALLOCATE(recv_info(1+2*numcpus+max_node_read))
view=(cpu_recv_address_start(mycpu+1))*4
count=1+2*my_cpu%ncpus_to_recv+nodes_tl_to_recv
recv_info(1)=my_cpu%ncpus_to_recv
i_index=1
do icpu=1,my_cpu%ncpus_to_recv
   i_index=i_index+1
   recv_info(i_index)=my_cpu%cpu_id_to_recv(icpu)
end do
do icpu=1,my_cpu%ncpus_to_recv
   i_index=i_index+1
   recv_info(i_index)=my_cpu%node_tl_to_recv(icpu)
end do
do icpu=1,my_cpu%ncpus_to_recv
   do jnode=1,my_cpu%node_tl_to_recv(icpu)
      i_index=i_index+1
      recv_info(i_index)=node_gid_to_recv(icpu,jnode)
   end do
end do
call MPI_FILE_WRITE_AT(fhandle, view,recv_info(1),count, MPI_INTEGER, &
     STATUS,ierr)
call MPI_File_CLOSE(fhandle,ierr)
call MPI_BARRIER(MPI_COMM_WORLD,ierr)
!
!read back from each cpu to which I need to send;
!first check and determine how many cpus I need to send to
!so that I can allocate memory
!
call MPI_File_Open(MPI_COMM_WORLD, 'fetch_out', &
     MPI_MODE_RDONLY, MPI_INFO_NULL, fhandle, ierr)
my_cpu%ncpus_to_send=0
do icpu=1,numcpus
   if(icpu-1==mycpu) cycle
   view=cpu_recv_address_start(icpu)*4
   count=1
   !
   !read total number of other cpus
   !
   call MPI_FILE_READ_AT(fhandle, view, cpu_read_tl,&
        count, MPI_INTEGER,STATUS,ierr)
   view=(cpu_recv_address_start(icpu)+1)*4
   count=2*cpu_read_tl
   call MPI_FILE_READ_AT(fhandle, view, recv_info(1),&
        count, MPI_INTEGER,STATUS,ierr)
   do jcpu=1,cpu_read_tl
      cpu_read_id=recv_info(jcpu)
      if(cpu_read_id==mycpu) then
         my_cpu%ncpus_to_send=my_cpu%ncpus_to_send+1
         exit
      end if
   end do
end do
call MPI_FILE_CLOSE(fhandle,ierr)
call MPI_BARRIER(MPI_COMM_WORLD,ierr)
!
!Allocate sending array
!
Allocate(node_gid_to_send(my_cpu%ncpus_to_send,node_tl),&
         my_cpu%node_lid_to_send(my_cpu%ncpus_to_send,node_tl),&
         my_cpu%cpu_id_to_send(my_cpu%ncpus_to_send),&
         my_cpu%node_tl_to_send(my_cpu%ncpus_to_send))
call MPI_File_Open(MPI_COMM_WORLD, 'fetch_out', &
     MPI_MODE_RDONLY, MPI_INFO_NULL, fhandle, ierr)
my_cpu%ncpus_to_send=0
do icpu=1,numcpus
   if(icpu-1==mycpu) cycle
   view=cpu_recv_address_start(icpu)*4
   count=1
   !
   !read total number of other cpus
   !
   call MPI_FILE_READ_AT(fhandle, view, cpu_read_tl,&
        count, MPI_INTEGER,STATUS,ierr)
   view=(cpu_recv_address_start(icpu)+1)*4

   count=2*cpu_read_tl
   call MPI_FILE_READ_AT(fhandle, view, recv_info(1),&
        count, MPI_INTEGER,STATUS,ierr)
   do jcpu=1,cpu_read_tl
      cpu_read_id=recv_info(jcpu)
      if(cpu_read_id==mycpu) then
         my_cpu%ncpus_to_send=my_cpu%ncpus_to_send+1
         my_cpu%cpu_id_to_send(my_cpu%ncpus_to_send)=icpu-1
         my_cpu%node_tl_to_send(my_cpu%ncpus_to_send)=&
                            recv_info(jcpu+cpu_read_tl)
         read_node_st=cpu_recv_address_start(icpu)+1+2*cpu_read_tl
         do kcpu=1,jcpu-1
            read_node_st=read_node_st+recv_info(kcpu+cpu_read_tl)
         end do
         view=read_node_st*4
         count=recv_info(jcpu+cpu_read_tl)
         call MPI_FILE_READ_AT(fhandle, view, recv_info(1),&
         count, MPI_INTEGER,STATUS,ierr)
         node_gid_to_send(my_cpu%ncpus_to_send,1:count)=recv_info(1:count)
         exit
       end if
   end do
end do
call MPI_FILE_CLOSE(fhandle,ierr)
call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!
!determine node_lid_send  (node local id)
!
do icpu=1,my_cpu%ncpus_to_send
   do jnode=1,my_cpu%node_tl_to_send(icpu)
      gid=node_gid_to_send(icpu,jnode)
      do knode=1,IUPPER-ILOWER+1
         !
         !caculate Gnode
         !
         NODE_GID=ILOWER+knode-1
         if(gid==NODE_GID) then
            my_cpu%node_lid_to_send(icpu,jnode)=knode
            exit
         end if
      end do
   end do
end do
!
!
!Deallocate some arrays
!
!
deallocate(cpu_ilower,cpu_iupper,node_gid_to_send,recv_info,&
           node_gid_to_recv)
return
end

subroutine fem_update_ghost_node_solution(mycpu,numcpus)
use mpi
use truth
use system
implicit none
INTEGER mycpu,numcpus,request,ierr,&
        icpu,jnode,node_tl_to_send,cpu_id_to_send,&
        node_tl_to_recv,cpu_id_to_recv,tag,&
        node_lid_to_recv,node_lid_to_send
real*8, allocatable :: x(:,:), x2(:,:)
integer, allocatable :: MPI_STATS(:,:),MPI_REQS(:)
integer :: MC
integer :: maxsend,maxrecv
call MPI_Comm_Rank(MPI_COMM_WORLD, mycpu, ierr)
call MPI_Comm_Size(MPI_COMM_WORLD, numcpus, ierr)
ALLOCATE(MPI_STATS(MPI_STATUS_SIZE,my_cpu%ncpus_to_send+my_cpu%ncpus_to_recv))
ALLOCATE(MPI_REQS(my_cpu%ncpus_to_send+my_cpu%ncpus_to_recv))

maxsend=0
do icpu=1,my_cpu%ncpus_to_send
  maxsend=MAX(maxsend,my_cpu%node_tl_to_send(icpu))
enddo
maxrecv=0
do icpu=1,my_cpu%ncpus_to_recv
  maxrecv=MAX(maxrecv,my_cpu%node_tl_to_recv(icpu))
enddo
ALLOCATE(x(3*maxsend,my_cpu%ncpus_to_send))
ALLOCATE(x2(3*maxrecv,my_cpu%ncpus_to_recv))
!
! send solution to requesting cpus
!
MC=0
do icpu=1,my_cpu%ncpus_to_send
   node_tl_to_send=my_cpu%node_tl_to_send(icpu)
   cpu_id_to_send=my_cpu%cpu_id_to_send(icpu)
   tag=20000*mycpu+cpu_id_to_send
   do jnode=1,node_tl_to_send
      node_lid_to_send=my_cpu%node_lid_to_send(icpu,jnode)
      x(3*(jnode-1)+1:3*jnode,icpu)=&
         residue(3*(node_lid_to_send-1)+1:3*node_lid_to_send)
   end do
   MC=MC+1
   CALL MPI_ISEND(x(:,icpu),3*node_tl_to_send,MPI_DOUBLE_PRECISION,&
                  cpu_id_to_send,tag,MPI_COMM_WORLD,MPI_REQS(MC),IERR)
end do
!
! receive solutions from other cpus for ghost nodes
!
do icpu=1,my_cpu%ncpus_to_recv
   node_tl_to_recv=my_cpu%node_tl_to_recv(icpu)
   cpu_id_to_recv=my_cpu%cpu_id_to_recv(icpu)
   tag=20000*cpu_id_to_recv+mycpu
   MC=MC+1
   CALL MPI_IRECV(x2(:,icpu),3*node_tl_to_recv,MPI_DOUBLE_PRECISION,&
                  cpu_id_to_recv,tag,MPI_COMM_WORLD,MPI_REQS(MC),IERR)
end do
!
! bag8: wait here for nonblocking sends and receives to complete
!
CALL MPI_WAITALL(MC,MPI_REQS,MPI_STATS,IERR)
!
! put copy receive buffer to tiny_u array
!
do icpu=1,my_cpu%ncpus_to_recv
   node_tl_to_recv=my_cpu%node_tl_to_recv(icpu)
   do jnode=1,node_tl_to_recv
      node_lid_to_recv=my_cpu%node_lid_to_recv(icpu,jnode)
      tiny_u(3*(node_lid_to_recv-1)+1:3*node_lid_to_recv)=&
         x2(3*(jnode-1)+1:3*jnode,icpu)
   end do
end do

DEALLOCATE(x)
DEALLOCATE(x2)
DEALLOCATE(MPI_STATS)
DEALLOCATE(MPI_REQS)

return
end

subroutine free_memory
!
!Author: Ruijie Liu
!
use truth
use model_type
use system
implicit none
deallocate(gstiff)
deallocate(gstiff_col_index)
deallocate(gstiff_row_index)
!BW deallocate(cnode_p)
!bw deallocate(pnode_cn)
!bw deallocate(indx)
!bw deallocate(internal_force_save)
!bw deallocate(internal_force)
!bw deallocate(tiny_u)
deallocate(residue)
deallocate(tl_load)
!bw deallocate(delta_load_naut)
!bw deallocate(delta_load_save)
!bw deallocate(delta_load)
!bw deallocate(delta_upt)
!bw deallocate(delta_u_2)
!bw deallocate(delta_u)
deallocate(u)
deallocate(u_n,delta_u,u_0)
deallocate(coord,coord_n)
!bw deallocate(upt_old)
if(tl_pressured_face+tl_traction_face>0) then
   deallocate(b_face)
end if
deallocate(load_cnst)
if(tl_0_dch>0) then
   deallocate(zero_dch)
   deallocate(zero_value)
end if
deallocate(hexa)
deallocate(node_nb)
!bw deallocate(cg_node)
deallocate(node_st_dof)
deallocate(coord)
if(mat_grp_tl.gt.0) then
   deallocate(mat_prpty)
end if
!bw
DEALLOCATE(NODE_ST_GDOF,ACTIVE_NODE,DISP_BC_RESIDUE,HYPRE_ROWS,&
           OFNODE_DISP_TMP,OFNODE_GNUM_TMP,LID_2IJK)
!bw

return
end

