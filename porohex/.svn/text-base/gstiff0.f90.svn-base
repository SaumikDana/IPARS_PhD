subroutine g_stiff(EP_MODEL,NNTIM,cutback,no_iter,gciter)
use truth
use model_type
use system
implicit none
integer ielem,inode,jnode,st_igdof,igrow,irow,jcol,jgcol,i,j
integer st_jgdof,iface,block,jmap,iblock,id_1,id_2,iii,igauss
integer master,orien,mat_id,cindex,cutback,ele_ID,EP_MODEL,no_iter
integer NNTIM,gciter
real(kind=double) nv(3,1),area,mu,stiff(24,24),mele_load(24),&
                  face_11(24,24),face_12(24,24),face_21(24,24),&
                  face_22(24,24),sele_load(24),u_a(3),stiffG(24,24),&
                  load_solid(24)
!
!
! assembling stiffness matrix
!
! saumik - changed if-statement
if((EP_MODEL<1.and.NNTIM==0.and.no_iter==0).or.EP_MODEL>0.or.&
   (EP_MODEL<1.and.NNTIM>0.and.gciter==1.and.no_iter==0)) then
   gstiff=zero_d
end if
load_cnst=0.0d0
tl_load=0.0d0
!bw internal_force=0.0d0
do ielem=1,ele_tl
   stiff=0.0d0     !bw done inside ele_stiff
   mele_load=0.0d0    !bw done inside ele_stiff
   load_solid=0.0d0
      ele_node=hexa(ielem)%tl_node
      ele_type=hexa(ielem)%ele_type
      cutback=0
      call ele_stiff_ep(ielem,stiff,mele_load,load_solid,EP_MODEL,NNTIM,&
                        cutback,no_iter)
      if(cutback>0) then
        !write(*,*)'element failed ielem=',ielem
        return
      endif
      do inode=1,ele_node
         st_igdof=hexa(ielem)%node_st_dof(inode)
         load_cnst(st_igdof:st_igdof+2)=&
         load_cnst(st_igdof:st_igdof+2)+mele_load(3*(inode-1)+1:3*inode)
         !tl_load(st_igdof:st_igdof+2)=&
         !tl_load(st_igdof:st_igdof+2)+load_solid(3*(inode-1)+1:3*inode)

! saumik - changed cycle statement
         if(EP_MODEL<1.and.(gciter>1.or.(NNTIM==0.and.no_iter>0))) cycle

         do jnode=1,ele_node
            st_jgdof=hexa(ielem)%node_st_dof(jnode)
            do i=1,node_dof
               igrow=st_igdof+i-1
               irow=(inode-1)*node_dof+i
               do j=1,node_dof
                  jcol=(jnode-1)*node_dof+j
                  jgcol=st_jgdof+j-1
                  call mapping_gij_to_compressed_index(igrow,jgcol,cindex)
                  if(cindex>0) then
                     gstiff(cindex)=gstiff(cindex)+stiff(irow,jcol)
                  end if
               end do
            end do
         end do
      end do
end do
do iface=1,tl_traction_face+tl_pressured_face    
   ele_ID=b_face(iface)%element
   if(ele_id.eq.0) then
     write(*,*) "ele_id = 0 in B_FACE(),FACE=",IFACE
     stop 13
   endif
   mele_load=0.0d0
   call b_face_load(iface,mele_load)
   ele_node=hexa(ele_ID)%tl_node
   do inode=1,ele_node
       st_igdof=hexa(ele_ID)%node_st_dof(inode)  
       load_cnst(st_igdof:st_igdof+2)=&
       load_cnst(st_igdof:st_igdof+2)-&
       load_scaling*mele_load(3*(inode-1)+1:3*inode)  ! bag8 - pressure bc
   end do
end do
!do inode=1,dof_tl/3
!   write(*,*) 'inode F=',inode,tl_load(3*(inode-1)+1),&
!   tl_load(3*(inode-1)+2),tl_load(3*(inode-1)+3)
!end do
return
end


! Bin Wang
! ************************************************************************
!SUBROUTINE G_STIFF_STEP
!USE TRUTH
!USE MODEL_TYPE
!USE SYSTEM
!IMPLICIT NONE
!INTEGER IELEM,INODE,JNODE,ST_IGDOF,IGROW,IROW,JCOL,JGCOL,I,J
!INTEGER ST_JGDOF,IFACE,BLOCK,JMAP,IBLOCK,ID_1,ID_2,III,IGAUSS
!INTEGER MASTER,ORIEN,MAT_ID,CINDEX,CUTBACK,ELE_ID
!REAL(KIND=DOUBLE) NV(3,1),AREA,MU,MELE_LOAD(24)
!
!write(*,*)'in g_stiff_step...'
!pause
!
!!
!!
!! ASSEMBLING STIFFNESS MATRIX
!!
!!
!! GSTIFF=ZERO_D
!! INTERNAL_FORCE=0.0D0
!DO IELEM=1,ELE_TL
!!BW      STIFF=0.0D0     !BW DONE INSIDE ELE_STIFF
!!BW      MELE_LOAD=0.0D0    !BW DONE INSIDE ELE_STIFF
!   ELE_NODE=HEXA(IELEM)%TL_NODE
!   ELE_TYPE=HEXA(IELEM)%ELE_TYPE
!   CALL ELE_STIFF_STEP(IELEM,MELE_LOAD)
!   DO INODE=1,ELE_NODE
!      ST_IGDOF=HEXA(IELEM)%NODE_ST_DOF(INODE)
!      LOAD_CNST(ST_IGDOF:ST_IGDOF+2)=&
!      LOAD_CNST(ST_IGDOF:ST_IGDOF+2)+&
!      LOAD_SCALING*MELE_LOAD(3*(INODE-1)+1:3*INODE)      ! bag8 - gravity
!!bw       DO JNODE=1,ELE_NODE
!!bw          ST_JGDOF=HEXA(IELEM)%NODE_ST_DOF(JNODE)
!!bw          DO I=1,NODE_DOF
!!bw             IGROW=ST_IGDOF+I-1
!!bw             IROW=(INODE-1)*NODE_DOF+I
!!bw             DO J=1,NODE_DOF
!!bw                JCOL=(JNODE-1)*NODE_DOF+J
!!bw                JGCOL=ST_JGDOF+J-1
!!bw                CALL MAPPING_GIJ_TO_COMPRESSED_INDEX(IGROW,JGCOL,CINDEX)
!!bw                IF(CINDEX>0) THEN
!!bw                   GSTIFF(CINDEX)=GSTIFF(CINDEX)+STIFF(IROW,JCOL)
!!bw                END IF
!!bw             END DO
!!bw          END DO
!!bw        END DO
!   END DO
!END DO
!DO IFACE=1,TL_TRACTION_FACE+TL_PRESSURED_FACE    
!   CALL B_FACE_LOAD(IFACE,MELE_LOAD)
!   ELE_ID=B_FACE(IFACE)%ELEMENT
!   if(ele_id.eq.0) then
!     write(*,*) "ele_id = 0 in B_FACE(),FACE=",IFACE
!     stop 13
!   endif
!   ELE_NODE=HEXA(ELE_ID)%TL_NODE
!   DO INODE=1,ELE_NODE
!       ST_IGDOF=HEXA(ELE_ID)%NODE_ST_DOF(INODE)  
!       LOAD_CNST(ST_IGDOF:ST_IGDOF+2)=&
!       LOAD_CNST(ST_IGDOF:ST_IGDOF+2)+&
!       LOAD_SCALING*MELE_LOAD(3*(INODE-1)+1:3*INODE)  ! bag8 - more bc's?
!   END DO
!END DO
!RETURN
!END
!
!SUBROUTINE ELE_STIFF_STEP(IELEM,MELE_LOAD)
!USE TRUTH
!USE MODEL_TYPE
!USE SYSTEM
!IMPLICIT NONE
!INTEGER INODE,IGAUSS,IELEM,I,J,M,MAT_NO,OPTION,SLOT,ST_DOF,II
!INTEGER IBLOCK,IFACE,MASTER,FACE_ID,YIELD,CONVERGENT,P_TRADITION
!REAL(KIND=DOUBLE) R,S,T,ALPHA_C,DVOLU,STRESS(6),D_TENSOR(3)
!REAL(KIND=DOUBLE) ELE_PT(24,1),ELE_LOAD(24,1),STRESS0(6)
!REAL(KIND=DOUBLE) STIFF_FOR_LOAD(24,24),TMP(6,1),U_A(3)
!REAL(KIND=DOUBLE) TMP2(24,1),DSIG0DE,SIG_BAR,DELTA_U_FACE(3),&
!DELTA_STRAIN(6),DELTA_PSTRAIN(6),D_EP(6,6),STIFF(24,24),MELE_LOAD(24)
!REAL(KIND=DOUBLE) N_MATX(3,24),BMATX(6,24),WGHT(8),B_0(24),ROU_BODY,&
!ROU_G(3),PPP,TMP3(3,1),ALPHA
!! STIFF=ZERO_D
!MELE_LOAD=0.0D0
!TMP2=0.0D0
!NGAUSS=HEXA(IELEM)%NGAUSS
!WGHT=WGHT_8
!STRESS0(1:6)=HEXA(IELEM)%INITIAL_STRESS(1:6)
!MAT_NO=HEXA(IELEM)%MAT_NO
!ROU_BODY=MAT_PRPTY(MAT_NO)%PRPTY_SLD(8)
!ALPHA=MAT_PRPTY(MAT_NO)%PRPTY_SLD(6)
!ROU_G=ROU_BODY*GRAVITY_VECTOR
!PPP=HEXA(IELEM)%PORE_PRESSURE-HEXA(IELEM)%INITIAL_PORE_PRESSURE
!PPP=ALPHA*PPP
!DO IGAUSS=1,NGAUSS
!   IF(ELE_TYPE==1) THEN    
!     R=GAUSS_COORD_8(IGAUSS,1)
!     S=GAUSS_COORD_8(IGAUSS,2)
!     T=GAUSS_COORD_8(IGAUSS,3)
!   ELSE
!     R=GAUSS_COORD_27(IGAUSS,1)
!     S=GAUSS_COORD_27(IGAUSS,2)
!     T=GAUSS_COORD_27(IGAUSS,3)
!   END IF
!   ELE_DOF=3*ELE_NODE
!   NODE_DOF=3
!   B_DOF=6                 
!   CALL HEXANB0(IELEM,R,S,T,BMATX,DVOLU,N_MATX)                                         
!!   CALL MAT_SOLVER(IELEM,IGAUSS,D_EP)  !BW CONSTRUCT D_EP
!!   STIFF=STIFF+MATMUL(MATMUL(TRANSPOSE(BMATX),D_EP),&
!!         BMATX)*DVOLU*WGHT(IGAUSS)
!   !
!   !
!   !LOADING FROM INITIAL STRESS
!   !
!   !
!   TMP(1:6,1)=-STRESS0(1:6)
!   TMP2=TMP2+MATMUL(TRANSPOSE(BMATX),TMP)*DVOLU*WGHT(IGAUSS)
!   !
!   !LOADING FROM GRAVITY
!   !
!   TMP3(1:3,1)=ROU_G(1:3)
!   TMP2=TMP2+MATMUL(TRANSPOSE(N_MATX),TMP3)*DVOLU*WGHT(IGAUSS)
!   !
!   !TAKING ACCOUNT INTO PORE PRESSURE  (PPP=P-P0)
!   !  
!       !
!       !FORMING B_0 (INTERPOLATION MATRIX FOR VOLUME STRAIN) FROM BMATX
!       !
!   B_0=0.0D0
!   DO I=1,8
!      DO J=1,3
!         II=(I-1)*3+J
!         B_0(II)=BMATX(J,II)
!      END DO
!   END DO
!   TMP2(1:24,1)=TMP2(1:24,1)+PPP*B_0(1:24)*DVOLU*WGHT(IGAUSS)              
!END DO
!MELE_LOAD(1:24)=TMP2(1:24,1)
!RETURN
!END

! Bin Wang
! ************************************************************************

subroutine ele_stiff_ep(ielem,stiff,mele_load,load_solid,EP_MODEL,NNTIM,&
                        cutback,no_iter)
use truth
use model_type
use system
implicit none
integer igauss,ielem,i,j,mat_no,cutback,EP_MODEL,NNTIM,no_iter
real(kind=double) r,s,t,dvolu,stress_pred(6)
real(kind=double) tmp(6,1),tmp2(24,1),D_ep(6,6)
real(kind=double) stiff(24,24),mele_load(24),load_solid(24)
real(kind=double) N_matx(3,24),bmatx(6,24),wght(8),rou_body,&
rou_g(3),PPP,tmp3(3,1),alpha,tmp20(24,1),tmp200(6,1)
stiff=zero_d
mele_load=0.0d0
load_solid=0.0d0
tmp2=0.0d0
tmp20=0.0d0
ngauss=hexa(ielem)%ngauss
wght=wght_8
mat_no=hexa(ielem)%mat_no
rou_body=mat_prpty(mat_no)%prpty_sld(8)
alpha=mat_prpty(mat_no)%prpty_sld(6)
rou_g=rou_body*gravity_vector
PPP=HEXA(IELEM)%PORE_PRESSURE-HEXA(IELEM)%INITIAL_PORE_PRESSURE
do igauss=1,ngauss
   if(ele_type==1) then
     r=gauss_coord_8(igauss,1)
     s=gauss_coord_8(igauss,2)
     t=gauss_coord_8(igauss,3)
   else
     r=gauss_coord_27(igauss,1)
     s=gauss_coord_27(igauss,2)
     t=gauss_coord_27(igauss,3)
   end if
   ele_dof=3*ele_node
   node_dof=3
   b_dof=6
   call hexanb0(ielem,r,s,t,bmatx,dvolu,N_matx)
   cutback=0
   call mat_solver(ielem,igauss,stress_pred,D_ep,EP_MODEL,cutback)
   if(cutback>0) return
   !
   !stiffness matrix
   !

! saumik - changed if-statement
   if((EP_MODEL<1.and.NNTIM==0).or.EP_MODEL>0.or.&
      (EP_MODEL<1.and.NNTIM>0.and.no_iter==0)) then
       stiff=stiff+matmul(matmul(transpose(bmatx),D_ep),&
         bmatx)*dvolu*wght(igauss)
   end if
   !
   !
   !stress: internal total effective stress
   !tmp: internal total stress (what we really need!)
   !
   tmp(1:6,1)=stress_pred(1:6)
   tmp(1,1)=tmp(1,1)-load_scaling*alpha*PPP   ! bag8 : scale pore pressure
   tmp(2,1)=tmp(2,1)-load_scaling*alpha*PPP
   tmp(3,1)=tmp(3,1)-load_scaling*alpha*PPP
   !
   !"-" below because of residue force targeted
   !
   tmp2=tmp2-matmul(transpose(bmatx),tmp)*dvolu*wght(igauss)
 !  tmp200(1:6,1)=stress_pred(1:6)
 !  tmp20=tmp20-matmul(transpose(bmatx),tmp200)*dvolu*wght(igauss)
   !
   !loading from gravity
   !
   tmp3(1:3,1)=rou_g(1:3)
   tmp2=tmp2+matmul(transpose(N_matx),tmp3)*dvolu*wght(igauss)
   !
   !Traction load will be computed elsewhere in terms of looping
   !for loaded surfaces
   !
end do
mele_load(1:24)=tmp2(1:24,1)
!load_solid(1:24)=tmp20(1:24,1)
return
end




!subroutine ele_stiff(ielem,stiff,mele_load)
!use truth
!use model_type
!use system
!implicit none
!integer inode,igauss,ielem,i,j,m,mat_no,option,slot,st_dof,ii
!integer iblock,iface,master,face_ID,yield,convergent,p_tradition
!real(kind=double) r,s,t,alpha_c,dvolu,stress(6),d_tensor(3)
!real(kind=double) ele_pt(24,1),ele_load(24,1),stress0(6)
!real(kind=double) stiff_for_load(24,24),tmp(6,1),u_a(3)
!real(kind=double) tmp2(24,1),dsig0de,sig_bar,delta_u_face(3),&
!delta_strain(6),delta_pstrain(6),D_ep(6,6),stiff(24,24),mele_load(24)
!real(kind=double) N_matx(3,24),bmatx(6,24),wght(8),B_0(24),rou_body,&
!rou_g(3),ppp,tmp3(3,1),alpha
!stiff=zero_d
!mele_load=0.0d0
!tmp2=0.0d0
!ngauss=hexa(ielem)%ngauss
!wght=wght_8
!stress0(1:6)=hexa(ielem)%initial_stress(1:6)
!mat_no=hexa(ielem)%mat_no
!rou_body=mat_prpty(mat_no)%prpty_sld(8)
!alpha=mat_prpty(mat_no)%prpty_sld(6)
!rou_g=rou_body*gravity_vector
!!rou_g=0.0d0
!ppp=hexa(ielem)%pore_pressure-hexa(ielem)%initial_pore_pressure
!ppp=alpha*ppp
!do igauss=1,ngauss
!   if(ele_type==1) then    
!     r=gauss_coord_8(igauss,1)
!     s=gauss_coord_8(igauss,2)
!     t=gauss_coord_8(igauss,3)
!   else
!     r=gauss_coord_27(igauss,1)
!     s=gauss_coord_27(igauss,2)
!     t=gauss_coord_27(igauss,3)
!   end if
!   ele_dof=3*ele_node
!   node_dof=3
!   b_dof=6                 
!   call hexanb0(ielem,r,s,t,bmatx,dvolu,N_matx)                                         
!   call mat_solver(ielem,igauss,stress,D_ep)  !bw construct D_ep
!   stiff=stiff+matmul(matmul(transpose(bmatx),D_ep),&
!         bmatx)*dvolu*wght(igauss)
!   !
!   !
!   !loading from initial stress
!   !
!   !
!   tmp(1:6,1)=-stress0(1:6)
!   tmp2=tmp2+matmul(transpose(bmatx),tmp)*dvolu*wght(igauss)
!   !
!   !loading from gravity
!   !
!   tmp3(1:3,1)=rou_g(1:3)
!   tmp2=tmp2+matmul(transpose(N_matx),tmp3)*dvolu*wght(igauss)
!   !
!   !taking account into pore pressure  (ppp=p-p0)
!   !  
!       !
!       !forming B_0 (interpolation matrix for volume strain) from Bmatx
!       !
!   B_0=0.0d0
!   do i=1,8
!      do j=1,3
!         ii=(i-1)*3+j
!         B_0(ii)=bmatx(j,ii)
!      end do
!   end do
!   tmp2(1:24,1)=tmp2(1:24,1)+ppp*B_0(1:24)*dvolu*wght(igauss)              
!end do
!mele_load(1:24)=tmp2(1:24,1)
!return
!end






subroutine PARDISO_POROHEX(NUMPRC)
!
!Author: Ruijie Liu
!       
use truth
use model_type
use system
implicit none
!     include 'mkl_pardiso.f77'
!C.. Internal solver memory pointer for 64-bit architectures
!C.. INTEGER*8 pt(64)
!C.. Internal solver memory pointer for 32-bit architectures
!C.. INTEGER*4 pt(64)
!C.. This is OK in both cases
INTEGER NUMPRC
INTEGER pt(64)
!C.. All other variables
 INTEGER maxfct, mnum, mtype, phase, n, nrhs, error, msglvl
      INTEGER iparm(64)
      REAL*8 b(dof_tl)
      REAL*8 x(dof_tl)
      INTEGER i, idum
      REAL*8 waltime1, waltime2, ddum
!C.. Fill all arrays containing matrix data.
      DATA nrhs /1/, maxfct /1/, mnum /1/
! Verbosity parameter, 0=no output, 1=verbose output
      integer, parameter :: IVERB = 0

      write(*,*)'In PARDISO_POROHEX'

!C..
!C.. Set up PARDISO control parameter
!C..
      do i = 1, 64
         iparm(i) = 0
      end do
      iparm(1) = 1 ! no solver default
      iparm(2) = 2 ! fill-in reordering from METIS
      iparm(3) = NUMPRC ! numbers of processors
      iparm(4) = 0 ! no iterative-direct algorithm
      iparm(5) = 0 ! no user fill-in reducing permutation
      iparm(6) = 0 ! =0 solution on the first n compoments of x
      iparm(7) = 0 ! not in use
      iparm(8) = 9 ! numbers of iterative refinement steps
      iparm(9) = 0 ! not in use
      iparm(10) = 13 ! perturbe the pivot elements with 1E-13
      iparm(11) = 1 ! use nonsymmetric permutation and scaling MPS
      iparm(12) = 0 ! not in use
      iparm(13) = 1 ! maximum weighted matching algorithm is switched-on (default for non-symmetric)
      iparm(14) = 0 ! Output: number of perturbed pivots
      iparm(15) = 0 ! not in use
      iparm(16) = 0 ! not in use
      iparm(17) = 0 ! not in use
      iparm(18) = -1 ! Output: number of nonzeros in the factor LU
      iparm(19) = -1 ! Output: Mflops for LU factorization
      iparm(20) = 0 ! Output: Numbers of CG Iterations
      error = 0 ! initialize error flag
      msglvl = 0 ! print statistical information
      mtype = 11 ! real unsymmetric
!C.. Initiliaze the internal solver memory pointer. This is only
!C necessary for the FIRST call of the PARDISO solver.
      n=dof_tl
      do i = 1, 64
         pt(i) = 0
      end do
!C.. Reordering and Symbolic Factorization, This step also allocates
!C all memory that is necessary for the factorization
      phase = 11 ! only reordering and symbolic factorization
      IF (IVERB.NE.0) WRITE(*,*) 'Calling pardiso for reordering'
      CALL pardiso (pt, maxfct, mnum, mtype, phase, n, gstiff, gstiff_row_index,&
                    gstiff_col_index,idum, nrhs, iparm, msglvl, ddum, ddum, error)
    !  WRITE(*,*) 'Reordering completed ... '
      IF (error .NE. 0) THEN
         WRITE(*,*) 'The following ERROR was detected: ', error
         STOP 1
      END IF
      IF (IVERB.NE.0) WRITE(*,*) 'Number of nonzeros in factors = ',iparm(18)
      IF (IVERB.NE.0) WRITE(*,*) 'Number of factorization MFLOPS = ',iparm(19)
!C.. Factorization.
      phase = 22 ! only factorization
      IF (IVERB.NE.0) WRITE(*,*) 'Calling pardiso for factorization'
      CALL pardiso (pt, maxfct, mnum, mtype, phase, n, gstiff, gstiff_row_index,&
                    gstiff_col_index,idum, nrhs, iparm, msglvl, ddum, ddum, error)
    !  WRITE(*,*) 'Factorization completed ... '
      IF (error .NE. 0) THEN
         WRITE(*,*) 'The following ERROR was detected: ', error
         STOP 1
      ENDIF
!C.. Back substitution and iterative refinement
      iparm(8) = 2 ! max numbers of iterative refinement steps
      phase = 33 ! only factorization
      b=residue
      IF (IVERB.NE.0) WRITE(*,*) 'Calling pardiso for back substitution'
      CALL pardiso (pt, maxfct, mnum, mtype, phase, n, gstiff, gstiff_row_index,&
                    gstiff_col_index,idum, nrhs, iparm, msglvl, b, x, error)
      residue=x
     ! WRITE(700,*) 'Solve completed ... '
     ! WRITE(700,*) 'The solution of the system is '
     ! DO i = 1, n
     !    WRITE(700,*) ' x(',i,') = ', x(i)
     ! END DO
!C.. Termination and release of memory
      phase = -1 ! release internal memory
      IF (IVERB.NE.0) WRITE(*,*) 'Calling pardiso to release memory'
      CALL pardiso (pt, maxfct, mnum, mtype, phase, n, ddum, idum, idum,&
      idum, nrhs, iparm, msglvl, ddum, ddum, error)
!C     END
!      deallocate(gstiff)
      return
      end


