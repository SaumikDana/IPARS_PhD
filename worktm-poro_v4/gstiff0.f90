! ************************************************************************
subroutine g_stiff(EP_MODEL,NNTIM,cutback,no_iter,gciter)
! ************************************************************************
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
! added the following on 4th may
integer mat_no
real(kind=double) alpha,stiff1(24,24),mele_load1(24),load_solid1(24)
! added the above on 4th may
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
! added the following on 4th may
load_cnst1=0.0d0
! added the above on 4th may
do ielem=1,ele_tl
   stiff=0.0d0    
   mele_load=0.0d0 
   load_solid=0.0d0
   ele_node=hexa(ielem)%tl_node
   ele_type=hexa(ielem)%ele_type
   cutback=0
   call ele_stiff_ep(ielem,stiff,mele_load,load_solid,EP_MODEL,NNTIM,&
                     cutback,no_iter)
   if(cutback>0) return
   do inode=1,ele_node
      st_igdof=hexa(ielem)%node_st_dof(inode)
      load_cnst(st_igdof:st_igdof+2)=&
      load_cnst(st_igdof:st_igdof+2)+mele_load(3*(inode-1)+1:3*inode)

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
               if(cindex>0) &
                  gstiff(cindex)=gstiff(cindex)+stiff(irow,jcol)
            end do
         end do
      end do
   end do
   ! added the following on 4th may
   !!!!!!!! computing over Omega_1 !!!!!!!
   mat_no=hexa(ielem)%mat_no
   alpha=mat_prpty(mat_no)%prpty_sld(6)
   if(alpha.eq.0) cycle ! loop over elements not in payzone
   stiff1=0.0d0    
   mele_load1=0.0d0 
   load_solid1=0.0d0
   ele_node=hexa(ielem)%tl_node
   ele_type=hexa(ielem)%ele_type
   cutback=0
   call ele_stiff_ep(ielem,stiff1,mele_load1,load_solid1,EP_MODEL,NNTIM,&
                     cutback,no_iter)
   if(cutback>0) return
   do inode=1,ele_node
      st_igdof=hexa(ielem)%node_st_dof(inode)
      load_cnst1(st_igdof:st_igdof+2)=&
      load_cnst1(st_igdof:st_igdof+2)+mele_load1(3*(inode-1)+1:3*inode)
   enddo
   !!!!!!!! computing over Omega_1 !!!!!!!
   ! added the above on 4th may
end do
do iface=1,tl_traction_face+tl_pressured_face    
   ele_ID=b_face(iface)%element
   if(ele_id.eq.0) then
     write(*,*) "ele_id = 0 in B_FACE(),FACE=",IFACE
     stop 13
   endif
   mele_load=0.0d0
   load_scaling=1.0d0
   call b_face_load(iface,mele_load)
   ele_node=hexa(ele_ID)%tl_node
   do inode=1,ele_node
      st_igdof=hexa(ele_ID)%node_st_dof(inode)  
      load_cnst(st_igdof:st_igdof+2)=&
      load_cnst(st_igdof:st_igdof+2)-&
      load_scaling*mele_load(3*(inode-1)+1:3*inode)  ! bag8 - pressure bc
   end do
end do
return
end

! ************************************************************************
subroutine ele_stiff_ep(ielem,stiff,mele_load,load_solid,EP_MODEL,NNTIM,&
                        cutback,no_iter)
! ************************************************************************
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
   !
   !loading from gravity
   !
   tmp3(1:3,1)=rou_g(1:3)
   tmp2=tmp2+matmul(transpose(N_matx),tmp3)*dvolu*wght(igauss)
end do
mele_load(1:24)=tmp2(1:24,1)
return
end

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


