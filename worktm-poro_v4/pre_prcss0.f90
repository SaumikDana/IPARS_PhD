!
!         Author: Ruijie Liu
!
!Gaussian Integration Point Natural Coordinates 
!

subroutine setup_gip
use truth
use model_type
use system
implicit none
integer i,j,k,igauss
real(kind=double) :: pt=0.577350269189626
real(kind=double) :: pt1=0.774596669241483
real(kind=double) :: pt2=zero_d
real(kind=double) local_wght(3),ddd
local_wght(1)=0.555555555556
local_wght(2)=0.888888888889
local_wght(3)=0.555555555556
!
!
!  Setting up Gaussian Integration Point Coordinates
!  for 2 by 2 by 2
!
!
gauss_coord_8=zero_d

gauss_coord_8(1,1)=pt
gauss_coord_8(5,1)=pt
gauss_coord_8(8,1)=pt
gauss_coord_8(4,1)=pt

gauss_coord_8(1,2)=pt
gauss_coord_8(2,2)=pt
gauss_coord_8(6,2)=pt
gauss_coord_8(5,2)=pt

gauss_coord_8(5,3)=pt
gauss_coord_8(6,3)=pt
gauss_coord_8(7,3)=pt
gauss_coord_8(8,3)=pt
!
gauss_coord_8(2,1)=-pt
gauss_coord_8(6,1)=-pt
gauss_coord_8(7,1)=-pt
gauss_coord_8(3,1)=-pt

gauss_coord_8(3,2)=-pt
gauss_coord_8(7,2)=-pt
gauss_coord_8(8,2)=-pt
gauss_coord_8(4,2)=-pt

gauss_coord_8(1,3)=-pt
gauss_coord_8(2,3)=-pt
gauss_coord_8(3,3)=-pt
gauss_coord_8(4,3)=-pt
wght_8=one_d
face_wght_4=one_d
!
!
!  Setting up Gaussian Integration Point Coordinates
!  for 3 by 3 by 3
!
!
wght_27=one_d
gauss_coord_27=zero_d

gauss_coord_27(1,1)=pt1
gauss_coord_27(5,1)=pt1
gauss_coord_27(8,1)=pt1
gauss_coord_27(4,1)=pt1
gauss_coord_27(17,1)=pt1
gauss_coord_27(16,1)=pt1
gauss_coord_27(20,1)=pt1
gauss_coord_27(12,1)=pt1
gauss_coord_27(21,1)=pt1

gauss_coord_27(1,2)=pt1
gauss_coord_27(2,2)=pt1
gauss_coord_27(6,2)=pt1
gauss_coord_27(5,2)=pt1
gauss_coord_27(9,2)=pt1
gauss_coord_27(18,2)=pt1
gauss_coord_27(13,2)=pt1
gauss_coord_27(17,2)=pt1
gauss_coord_27(22,2)=pt1

gauss_coord_27(5,3)=pt1
gauss_coord_27(6,3)=pt1
gauss_coord_27(7,3)=pt1
gauss_coord_27(8,3)=pt1
gauss_coord_27(13,3)=pt1
gauss_coord_27(14,3)=pt1
gauss_coord_27(15,3)=pt1
gauss_coord_27(16,3)=pt1
gauss_coord_27(26,3)=pt1

wght_27(1)=wght_27(1)*local_wght(1)
wght_27(5)=wght_27(5)*local_wght(1)
wght_27(8)=wght_27(8)*local_wght(1)
wght_27(4)=wght_27(4)*local_wght(1)
wght_27(17)=wght_27(17)*local_wght(1)
wght_27(16)=wght_27(16)*local_wght(1)
wght_27(20)=wght_27(20)*local_wght(1)
wght_27(12)=wght_27(12)*local_wght(1)
wght_27(21)=wght_27(21)*local_wght(1)

wght_27(1)=wght_27(1)*local_wght(1)
wght_27(2)=wght_27(2)*local_wght(1)
wght_27(6)=wght_27(6)*local_wght(1)
wght_27(5)=wght_27(5)*local_wght(1)
wght_27(9)=wght_27(9)*local_wght(1)
wght_27(18)=wght_27(18)*local_wght(1)
wght_27(13)=wght_27(13)*local_wght(1)
wght_27(17)=wght_27(17)*local_wght(1)
wght_27(22)=wght_27(22)*local_wght(1)

wght_27(5)=wght_27(5)*local_wght(1)
wght_27(6)=wght_27(6)*local_wght(1)
wght_27(7)=wght_27(7)*local_wght(1)
wght_27(8)=wght_27(8)*local_wght(1)
wght_27(13)=wght_27(13)*local_wght(1)
wght_27(14)=wght_27(14)*local_wght(1)
wght_27(15)=wght_27(15)*local_wght(1)
wght_27(16)=wght_27(16)*local_wght(1)
wght_27(26)=wght_27(26)*local_wght(1)

gauss_coord_27(2,1)=-pt1
gauss_coord_27(6,1)=-pt1
gauss_coord_27(7,1)=-pt1
gauss_coord_27(3,1)=-pt1
gauss_coord_27(18,1)=-pt1
gauss_coord_27(14,1)=-pt1
gauss_coord_27(19,1)=-pt1
gauss_coord_27(10,1)=-pt1
gauss_coord_27(23,1)=-pt1

gauss_coord_27(3,2)=-pt1
gauss_coord_27(7,2)=-pt1
gauss_coord_27(8,2)=-pt1
gauss_coord_27(4,2)=-pt1
gauss_coord_27(19,2)=-pt1
gauss_coord_27(15,2)=-pt1
gauss_coord_27(20,2)=-pt1
gauss_coord_27(11,2)=-pt1
gauss_coord_27(24,2)=-pt1

gauss_coord_27(1,3)=-pt1
gauss_coord_27(2,3)=-pt1
gauss_coord_27(3,3)=-pt1
gauss_coord_27(4,3)=-pt1
gauss_coord_27(9,3)=-pt1
gauss_coord_27(10,3)=-pt1
gauss_coord_27(11,3)=-pt1
gauss_coord_27(12,3)=-pt1
gauss_coord_27(25,3)=-pt1


wght_27(2)=wght_27(2)*local_wght(1)
wght_27(6)=wght_27(6)*local_wght(1)
wght_27(7)=wght_27(7)*local_wght(1)
wght_27(3)=wght_27(3)*local_wght(1)
wght_27(18)=wght_27(18)*local_wght(1)
wght_27(14)=wght_27(14)*local_wght(1)
wght_27(19)=wght_27(19)*local_wght(1)
wght_27(10)=wght_27(10)*local_wght(1)
wght_27(23)=wght_27(23)*local_wght(1)

wght_27(3)=wght_27(3)*local_wght(1)
wght_27(7)=wght_27(7)*local_wght(1)
wght_27(8)=wght_27(8)*local_wght(1)
wght_27(4)=wght_27(4)*local_wght(1)
wght_27(19)=wght_27(19)*local_wght(1)
wght_27(15)=wght_27(15)*local_wght(1)
wght_27(20)=wght_27(20)*local_wght(1)
wght_27(11)=wght_27(11)*local_wght(1)
wght_27(24)=wght_27(24)*local_wght(1)

wght_27(1)=wght_27(1)*local_wght(1)
wght_27(2)=wght_27(2)*local_wght(1)
wght_27(3)=wght_27(3)*local_wght(1)
wght_27(4)=wght_27(4)*local_wght(1)
wght_27(9)=wght_27(9)*local_wght(1)
wght_27(10)=wght_27(10)*local_wght(1)
wght_27(11)=wght_27(11)*local_wght(1)
wght_27(12)=wght_27(12)*local_wght(1)
wght_27(25)=wght_27(25)*local_wght(1)

gauss_coord_27(9,1)=zero_d
gauss_coord_27(22,1)=zero_d
gauss_coord_27(13,1)=zero_d
gauss_coord_27(26,1)=zero_d
gauss_coord_27(15,1)=zero_d
gauss_coord_27(24,1)=zero_d
gauss_coord_27(11,1)=zero_d
gauss_coord_27(25,1)=zero_d
gauss_coord_27(27,1)=zero_d

gauss_coord_27(12,2)=zero_d
gauss_coord_27(25,2)=zero_d
gauss_coord_27(10,2)=zero_d
gauss_coord_27(23,2)=zero_d
gauss_coord_27(14,2)=zero_d
gauss_coord_27(26,2)=zero_d
gauss_coord_27(16,2)=zero_d
gauss_coord_27(21,2)=zero_d
gauss_coord_27(27,2)=zero_d

gauss_coord_27(17,3)=zero_d
gauss_coord_27(22,3)=zero_d
gauss_coord_27(18,3)=zero_d
gauss_coord_27(23,3)=zero_d
gauss_coord_27(19,3)=zero_d
gauss_coord_27(24,3)=zero_d
gauss_coord_27(20,3)=zero_d
gauss_coord_27(21,3)=zero_d
gauss_coord_27(27,3)=zero_d


wght_27(9)=wght_27(9)*local_wght(2)
wght_27(22)=wght_27(22)*local_wght(2)
wght_27(13)=wght_27(13)*local_wght(2)
wght_27(26)=wght_27(26)*local_wght(2)
wght_27(15)=wght_27(15)*local_wght(2)
wght_27(24)=wght_27(24)*local_wght(2)
wght_27(11)=wght_27(11)*local_wght(2)
wght_27(25)=wght_27(25)*local_wght(2)
wght_27(27)=wght_27(27)*local_wght(2)

wght_27(12)=wght_27(12)*local_wght(2)
wght_27(25)=wght_27(25)*local_wght(2)
wght_27(10)=wght_27(10)*local_wght(2)
wght_27(23)=wght_27(23)*local_wght(2)
wght_27(14)=wght_27(14)*local_wght(2)
wght_27(26)=wght_27(26)*local_wght(2)
wght_27(16)=wght_27(16)*local_wght(2)
wght_27(21)=wght_27(21)*local_wght(2)
wght_27(27)=wght_27(27)*local_wght(2)

wght_27(17)=wght_27(17)*local_wght(2)
wght_27(22)=wght_27(22)*local_wght(2)
wght_27(18)=wght_27(18)*local_wght(2)
wght_27(23)=wght_27(23)*local_wght(2)
wght_27(19)=wght_27(19)*local_wght(2)
wght_27(24)=wght_27(24)*local_wght(2)
wght_27(20)=wght_27(20)*local_wght(2)
wght_27(21)=wght_27(21)*local_wght(2)
wght_27(27)=wght_27(27)*local_wght(2)
igauss=one
do j=1,3
   do i=1,3
      face_wght_9(igauss)=local_wght(i)*local_wght(j)
      igauss=igauss+one
   end do
end do
return
end



subroutine get_gstiff_index
!
!Author: Ruijie Liu
!
!Setting up the row and column index for compressed global stiffness matrix
!

use truth
use model_type
use system
implicit none
integer ielem,jnode,gnode,add_elem,tl_nb_elem,kelem,&
        iface,jelem,inode,tl_nb_node,add_node,knode,&
        nb_node_id,nb_elem_id,nnode,add_nb_node,lelem,&
        temp(64),stored_number,idof,jdof,row_start_number
!
!Setting the elements related to a node
!
!
do inode=1,node_tl
    node_nb(inode)%nb_elem_id=0
    node_nb(inode)%nb_node_id=0
    node_nb(inode)%tl_nb_elem=0
    node_nb(inode)%tl_nb_node=0
end do
    !First, check the elements
do ielem=1,ele_tl
   nnode=hexa(ielem)%tl_node
   do jnode=1,nnode
      gnode=hexa(ielem)%node(jnode)
      add_elem=1
      tl_nb_elem=node_nb(gnode)%tl_nb_elem
      do kelem=1,tl_nb_elem
         nb_elem_id=node_nb(gnode)%nb_elem_id(kelem)
         if(ielem==nb_elem_id) then
            add_elem=0
            exit
         end if
      end do
      if(add_elem==1) then
         tl_nb_elem=tl_nb_elem+1
         node_nb(gnode)%tl_nb_elem=tl_nb_elem
         node_nb(gnode)%nb_elem_id(tl_nb_elem)=ielem
      end if
   end do
end do
do inode=1,node_tl
!Setting up the neighbor nodes of each node
!
   tl_nb_node=1
   node_nb(inode)%tl_nb_node=tl_nb_node
   node_nb(inode)%nb_node_id(1)=inode !bw needed for stiffness matrix
   tl_nb_elem=node_nb(inode)%tl_nb_elem
   do ielem=1,tl_nb_elem
      kelem=node_nb(inode)%nb_elem_id(ielem)
      nnode=hexa(kelem)%tl_node
      do jnode=1,nnode
         gnode=hexa(kelem)%node(jnode)
         add_node=1
         tl_nb_node=node_nb(inode)%tl_nb_node
         do knode=1,tl_nb_node
            nb_node_id=node_nb(inode)%nb_node_id(knode)
            if(gnode==nb_node_id) then
              add_node=0
              exit
            end if
         end do
         if(add_node==1) then
            tl_nb_node=tl_nb_node+1
            node_nb(inode)%tl_nb_node=tl_nb_node
            node_nb(inode)%nb_node_id(tl_nb_node)=gnode
         end if
      end do
   end do
!
!Sorting neighbor nodes in the order
!
   temp=0
   tl_nb_node=node_nb(inode)%tl_nb_node
   temp(1:tl_nb_node)=node_nb(inode)%nb_node_id(1:tl_nb_node)
   call isort(temp,tl_nb_node)
   node_nb(inode)%nb_node_id(1:tl_nb_node)=temp(1:tl_nb_node)
   node_nb(inode)%dof=3
end do
!
!(a) compute the total number of non-zero values;
!(b) compute the row index for compressed global stiffness matrix
!(c) compute the column index for compressed global stiffness matrix
!
allocate(gstiff_row_index(dof_tl+1))
gstiff_row_index=0
tl_non_zeros=0
do inode=1,node_tl
   tl_nb_node=node_nb(inode)%tl_nb_node
   tl_non_zeros=tl_non_zeros+3*tl_nb_node
end do
tl_non_zeros=3*tl_non_zeros
allocate(gstiff_col_index(tl_non_zeros))
gstiff_col_index=0
allocate(gstiff(tl_non_zeros))
gstiff=0.0d0
stored_number=0
do inode=1,node_tl
   node_dof=node_nb(inode)%dof
   tl_nb_node=node_nb(inode)%tl_nb_node
   do idof=1,node_dof  
      row_start_number=stored_number+1
      gstiff_row_index((inode-1)*node_dof+idof)=row_start_number
      do jnode=1,tl_nb_node
         nb_node_id=node_nb(inode)%nb_node_id(jnode)
         do jdof=1,node_dof
            stored_number=stored_number+1
            gstiff_col_index(stored_number)=(nb_node_id-1)*node_dof+jdof
         end do
      end do      
   end do
end do
gstiff_row_index(dof_tl+1)=tl_non_zeros+1
return
end 



subroutine mapping_gij_to_compressed_index(i,j,cindex)
!
!Given full global stiffness matrix element index i and j, 
!find the index for compressed global stiffness matrix 
!(compressed vector)
!
use truth
use model_type
use system
implicit none
integer i,j,cindex,ij,address1,address2
cindex=0
address1=gstiff_row_index(i)
address2=gstiff_row_index(i+1)-1
do ij=address1,address2
   !
   !checking col index 
   !
   if(j.ne.gstiff_col_index(ij)) cycle
   cindex=ij
   exit
end do
return
end


subroutine isort(iv,n)
use truth
implicit none
integer i,j,k,l,m,n,tmp,iv(*)
m=n
10 m=m/2
if(m.eq.0) go to 999
k=n-m
j=1
20 i=j
30 l=i+m
if(iv(i).le.iv(l)) go to 40
tmp=iv(i)
iv(i)=iv(l)
iv(l)=tmp
i=i-m
if(i.ge.1) go to 30
40 j=j+1
if(j.gt.k) goto 10
goto 20
999 return
end


SUBROUTINE PRE_PRCSS2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
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
        PASSO,YIELD_SIG0,YIELD_ALPHA,FLOW_ALPHA,&  !Ruijie add plasticity models
        HARDEN_MODEL,HARDEN_C1,HARDEN_C2,&         !Ruijie add plasticity models
        STRXX_INIT,STRYY_INIT,STRZZ_INIT,STRXY_INIT,STRYZ_INIT,&
        STRXZ_INIT,PRESS,PREF,&
        BDTYP1,BDTYP2,BDTYP3,BDTYP4,BDTYP5,BDTYP6,&
        BDDISP1,BDDISP2,BDDISP3,BDDISP4,BDDISP5,BDDISP6,&
        BDTRAC1,BDTRAC2,BDTRAC3,BDTRAC4,BDTRAC5,BDTRAC6,&
        BDFTRAC1,BDFTRAC2,BDFTRAC3,BDFTRAC4,BDFTRAC5,BDFTRAC6,&
        PRESFACE1,PRESFACE2,PRESFACE3,PRESFACE4,PRESFACE5,PRESFACE6,&
        CRAC_IBC_FACE,FNODE_TYPE,FRACFACEPROC)
!
! BIN WANG: SETUP DATA STRUCTURE IN POROHEX, USE INFO FROM IPARS
!
USE MPI 
USE TRUTH
USE MODEL_TYPE
USE SYSTEM
IMPLICIT NONE
include 'emodel.h'

INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
INTEGER LSIZE,ILOWER,IUPPER,&
        LFSIZE,IFLOWER,IFUPPUER,&
        LALLSIZE,LFALLSIZE,LALLELEM,&
        GSIZE,GFSIZE,MYPRC,NUMPRC,NNTIM
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
INTEGER PASSO(IDIM,JDIM,KDIM),HARDEN_MODEL(IDIM,JDIM,KDIM) !Ruijie add plasticity
REAL*8  MODUL(IDIM,JDIM,KDIM),POISS(IDIM,JDIM,KDIM),&
        BIOTA(IDIM,JDIM,KDIM),BIOTM(IDIM,JDIM,KDIM),&
        YIELD_SIG0(IDIM,JDIM,KDIM),YIELD_ALPHA(IDIM,JDIM,KDIM),& !Ruijie add plasticity
        FLOW_ALPHA(IDIM,JDIM,KDIM),HARDEN_C1(IDIM,JDIM,KDIM),& !Ruijie add plasticity
        HARDEN_C2(IDIM,JDIM,KDIM),& !Ruijie add plasticity
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
INTEGER FRAC,FACE,IOFF,JOFF,KOFF,IERR
REAL*8  CRAC_IBC_face(3,TOTAL_CRACKED_FACE)
INTEGER FNODE_TYPE(IDIM,JDIM,KDIM),FRACFACEPROC(MXFRACFACE,MXFRAC)
INTEGER I,J,K,II,JJ,KK,CTR,ELEM,DIR,NGID,NLID,ELEM2,JL1,JL2
INTEGER NODE_ID,GNODE,ARRSIZE
INTEGER OFFSET(3,8),FOFFSET(2,4)
DATA    OFFSET /0,0,0, 1,0,0, 0,1,0, 1,1,0,&
                0,0,1, 1,0,1, 0,1,1, 1,1,1/
DATA    FOFFSET /-1,-1, 0,-1, -1,0, 0,0/

REAL*8, ALLOCATABLE :: DISP_BC(:,:)
REAL*8, ALLOCATABLE :: DISP_FBC(:,:)
REAL*8, ALLOCATABLE :: FACE_LOAD_BC(:,:)
REAL*8, ALLOCATABLE :: PRES_LOAD_BC(:,:)
INTEGER IGROUP,JPROP,INODE,JCOOR,IELEM,JNODE,JGAUSS
INTEGER IGAUSS,IBFACE,MATT,NODE_N,III
INTEGER INC,ILOAD,NODE,ELE_ID,ISTEP,JDOF,IFACE,FACE_ID,ICOLUMN
INTEGER IFLOW,LOAD_NODE,LOAD_DOF,MODEL_ID
INTEGER SLD_PRP_ITM,YIELD_PRP_ITM,FLOW_PRP_ITM,ISO_PRP_ITM,KIN_PRP_ITM
REAL(KIND=DOUBLE) TEMP,R,S,T,X,Y,Z,NORMAL(6,3),ALPHA
STRSS_DOF=6
NODE_DOF=3
ELE_NODE=8
TOTAL_CRAC_FACE=TOTAL_CRACKED_FACE
CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)

! Skip most of the initialization process for time step run
IF (NNTIM.EQ.2) THEN
  GOTO 200
ELSEIF (NNTIM.EQ.1) THEN
  ! bag8 - added time-dependent boundary condition capability
  IF (MECH_BC_NCASE.NE.0) THEN
    GOTO 50
  ELSE
    GOTO 100
  ENDIF
ENDIF
!
!RESTATING IPARS FACE NODE NOTATIONS:
!
CALL IPARS_NODE_ON_FACE
!
!SETTING MATERIAL TO COMMON BLOCK
!
!********************************

! TOTAL NUMBER OF NODES
NODE_TL = LALLSIZE+LFALLSIZE ! TOTAL RESERVOIR AND TOTAL OPEN FRACTURE NODES

! TOTAL NUMBER OF ELEMENTS
ELE_TL = LALLELEM

! TOTAL NUMBER OF MATERIAL TYPE
MAT_GRP_TL = LALLELEM

! SETUP NUMBER OF MATERIAL PROPERTIES
MAT_NPROP_MAX = 10
IF(MAT_GRP_TL.GT.0) THEN   
   ALLOCATE(MAT_PRPTY(MAT_GRP_TL))
   DO K=KL1-1,KL2+1
      JL1 = MIN(JL1V(K-1),JL1V(K),JL1V(K+1))
      JL2 = MAX(JL2V(K-1),JL2V(K),JL2V(K+1))
      DO J=JL1-1,JL2+1
         DO I=IL1,IL2
            IF(ELEM_LID(I,J,K).GT.0) THEN
               IGROUP=ELEM_LID(I,J,K)
               MAT_PRPTY(IGROUP)%MODEL_ID=IGROUP
               MAT_PRPTY(IGROUP)%SLD_PRP_ITM=MAT_NPROP_MAX
               SLD_PRP_ITM=MAT_PRPTY(IGROUP)%SLD_PRP_ITM
               IF(SLD_PRP_ITM.GT.0) THEN
                  ALLOCATE(MAT_PRPTY(IGROUP)%PRPTY_SLD(SLD_PRP_ITM))   
                  MAT_PRPTY(IGROUP)%PRPTY_SLD(1)= MODUL(I,J,K)
                  MAT_PRPTY(IGROUP)%PRPTY_SLD(2)= POISS(I,J,K)
                  MAT_PRPTY(IGROUP)%PRPTY_SLD(6)= BIOTA(I,J,K)
                  MAT_PRPTY(IGROUP)%PRPTY_SLD(7)= BIOTM(I,J,K)
               ENDIF
!
!Ruijie add plasticity models
!  
                  MAT_PRPTY(IGROUP)%ASSOC_MODEL= &
                                          PASSO(I,J,K)
                  ALLOCATE(MAT_PRPTY(IGROUP)%YIELD_PARA(2))
                  MAT_PRPTY(IGROUP)%YIELD_PARA(1)= &
                                      YIELD_SIG0(I,J,K)
                  MAT_PRPTY(IGROUP)%YIELD_PARA(2)= &
                                     YIELD_ALPHA(I,J,K)
                  ALLOCATE(MAT_PRPTY(IGROUP)%FLOW_PARA(2))
                  MAT_PRPTY(IGROUP)%FLOW_PARA(1)= &
                                      FLOW_ALPHA(I,J,K)
                  MAT_PRPTY(IGROUP)%HARDEN_ID= &
                                   HARDEN_MODEL(I,J,K)
                  ALLOCATE(MAT_PRPTY(IGROUP)%ISO_HARDEN(2))
                  MAT_PRPTY(IGROUP)%ISO_HARDEN(1)= &
                                       HARDEN_C1(I,J,K)
                  MAT_PRPTY(IGROUP)%ISO_HARDEN(2)= &
                                       HARDEN_C2(I,J,K)
             ENDIF
          ENDDO
       ENDDO
    ENDDO
ENDIF
!SET UP GAUSSIAN INTEGRATION POINT WEIGHT AND COORDINATES
!
CALL SETUP_GIP
!
ALLOCATE(COORD(NODE_TL,3),STAT=IERR)
ALLOCATE(COORD_N(NODE_TL,3),STAT=IERR)
ALLOCATE(u_0(3*NODE_TL),STAT=IERR)
u_0=0.0d0
ALLOCATE(u(3*NODE_TL),STAT=IERR)
u=0.0d0
ALLOCATE(u_n(3*NODE_TL),STAT=IERR)
u_n=0.0d0
ALLOCATE(delta_u(3*NODE_TL),STAT=IERR)
ALLOCATE(tiny_u(3*NODE_TL),STAT=IERR)
ALLOCATE(NODE_ST_DOF(NODE_TL),STAT=IERR)  
ALLOCATE(NODE_ST_GDOF(NODE_TL),STAT=IERR)  
ALLOCATE(ACTIVE_NODE(NODE_TL),STAT=IERR)  
ALLOCATE(NODE_NB(NODE_TL),STAT=IERR) 
ALLOCATE(LID_2IJK(4,NODE_TL),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "13 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
CALL IPARS_TRANSFER_TO_ELASTIC_SOLVER   !PREPARE NODE CONVERSION
CALL IPARS_FACE_TRANSFER_TO_ELASTIC_SOLVER !PREPARE FACE CONVERSION
!
!SETTING COORDINATE TO COMMON BLOCK
!
DOF_TL=NODE_TL*NODE_DOF

LID_2IJK=0

! FIRST, REGULAR RESERVOIR NODE (ACTIVE+GHOST)
DO K = 1,KDIM
   DO J = 1,JDIM
      DO I = IL1,IL2+1
         NODE=NODE_LID(I,J,K)
         IF (NODE.GT.0) THEN
            LID_2IJK(1,NODE)=I
            LID_2IJK(2,NODE)=J
            LID_2IJK(3,NODE)=K
            LID_2IJK(4,NODE)=0
            COORD(NODE,1)=XC(I,J,K)
            COORD(NODE,2)=YC(I,J,K)
            COORD(NODE,3)=ZC(I,J,K)
         ENDIF
      ENDDO
   ENDDO
ENDDO

! SECOND, SETUP COORD() FOR OPEN FRACTURE NODES (ACTIVE+GHOST)
DO CTR=1,LFALLSIZE
   I=OFNODE_AFFINE(1,CTR)
   J=OFNODE_AFFINE(2,CTR)
   K=OFNODE_AFFINE(3,CTR)
   NODE=CTR+LALLSIZE
   LID_2IJK(1,NODE)=0
   LID_2IJK(2,NODE)=0
   LID_2IJK(3,NODE)=0
   LID_2IJK(4,NODE)=CTR
   COORD(NODE,1)=XC(I,J,K)
   COORD(NODE,2)=YC(I,J,K)
   COORD(NODE,3)=ZC(I,J,K) 
ENDDO

! SETUP NODE_ST_DOF (STARTING LOCAL DOF OF NODE):
! SETUP NODE_ST_GDOF (STARTING GLOBAL DOF OF NODE):

! FIRST, FOR LOCAL RESERVOIR NODES
ACTIVE_NODE = 0
DO K=1,KDIM
   DO J=1,JDIM
      DO I=IL1,IL2+1
         NODE=NODE_LID(I,J,K)
         IF(NODE.GT.0) THEN
           NGID=GELEI(I,J,K)
           NODE_ST_DOF(NODE)=(NODE-1)*3+1
           NODE_ST_GDOF(NODE)=(NGID-1)*3+1
           ACTIVE_NODE(NODE)=KEYOUT_CR(I,J,K)
         ENDIF
      ENDDO
   ENDDO
ENDDO

!SECOND, FOR LOCAL OPEN FRACTURE NODES
DO CTR=1,LFALLSIZE
   I=OFNODE_AFFINE(1,CTR)
   J=OFNODE_AFFINE(2,CTR)
   K=OFNODE_AFFINE(3,CTR)
   NODE=CTR+LALLSIZE
   NGID=OFNODE_L2GID(CTR)
   NODE_ST_DOF(NODE)=(NODE-1)*NODE_DOF+1
   NODE_ST_GDOF(NODE)=(NGID-1)*NODE_DOF+1
   ACTIVE_NODE(NODE)=OFNODE_KEYOUT(CTR)
ENDDO

!SETTING ELEMENT TO COMMON BOCK
!
ALLOCATE(HEXA(ELE_TL),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "14 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF

DO IELEM=1,ELE_TL
   HEXA(IELEM)%ELE_TYPE=1 
   IF(HEXA(IELEM)%ELE_TYPE==1) THEN 
     HEXA(IELEM)%TL_NODE=8
     HEXA(IELEM)%NGAUSS=8
   END IF
   IF(HEXA(IELEM)%ELE_TYPE==2) THEN 
     HEXA(IELEM)%TL_NODE=20
     HEXA(IELEM)%NGAUSS=27
   END IF
   IF(HEXA(IELEM)%ELE_TYPE==3) THEN 
     HEXA(IELEM)%TL_NODE=27
     HEXA(IELEM)%NGAUSS=27
   END IF
END DO

DO K=KL1-1,KL2+1
   JL1 = MIN(JL1V(K-1),JL1V(K),JL1V(K+1))
   JL2 = MAX(JL2V(K-1),JL2V(K),JL2V(K+1))
   DO J=JL1-1,JL2+1
      DO I=IL1,IL2
         IELEM=ELEM_LID(I,J,K)
         IF(IELEM.GT.0) THEN
           ELE_NODE=HEXA(IELEM)%TL_NODE
           NGAUSS=HEXA(IELEM)%NGAUSS
           ALLOCATE(HEXA(IELEM)%NODE(ELE_NODE),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%NODE_ST_DOF(ELE_NODE),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_STRSS(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_STRSS_T(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_STRN(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_STRN_T(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_ESTRN(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_ESTRN_T(NGAUSS,6),STAT=IERR)
           HEXA(IELEM)%GPT_STRSS=ZERO_D
           HEXA(IELEM)%GPT_STRSS_T=ZERO_D
           HEXA(IELEM)%GPT_STRN=ZERO_D
           HEXA(IELEM)%GPT_STRN_T=ZERO_D
           HEXA(IELEM)%GPT_ESTRN=ZERO_D
           HEXA(IELEM)%GPT_ESTRN_T=ZERO_D
           !
           !Ruijie add for plasticity
           !
           ALLOCATE(HEXA(IELEM)%GPT_PSTRN(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_PSTRN_T(NGAUSS,6),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_VSTATE(NGAUSS,2),STAT=IERR)
           ALLOCATE(HEXA(IELEM)%GPT_VSTATE_T(NGAUSS,2),STAT=IERR)
           IF (IERR.GT.0) THEN
              WRITE(*,*) "15 NOT ENOUGH MEMORY IN PRE_PRCSS"
              STOP 462
           ENDIF
           HEXA(IELEM)%GPT_PSTRN=ZERO_D
           HEXA(IELEM)%GPT_PSTRN_T=ZERO_D
           HEXA(IELEM)%GPT_VSTATE=ZERO_D
           HEXA(IELEM)%GPT_VSTATE_T=ZERO_D

           HEXA(IELEM)%MAT_NO=IELEM
           HEXA(IELEM)%PORE_PRESSURE=PRESS(I,J,K)  ! Needs to be changed for every time step
           !
           !Ruijie: initial stress input means initial effective stresse!!!
           !
           HEXA(IELEM)%INITIAL_STRESS(1)=STRXX_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_STRESS(2)=STRYY_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_STRESS(3)=STRZZ_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_STRESS(4)=STRXY_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_STRESS(5)=STRYZ_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_STRESS(6)=STRXZ_INIT(I,J,K)
           HEXA(IELEM)%INITIAL_PORE_PRESSURE=PREF(I,J,K)
           HEXA(IELEM)%GPT_STRSS=HEXA(IELEM)%GPT_STRSS_T
           !
           !initial shear strength sigma0
           !
           HEXA(IELEM)%GPT_VSTATE_T(1:NGAUSS,1)=&
                                         MAT_PRPTY(IGROUP)%YIELD_PARA(1)
           !
           !epql: initial equivalent plastic strain
           !
           HEXA(IELEM)%GPT_VSTATE_T(1:NGAUSS,2)=0.0d0
           HEXA(IELEM)%GPT_VSTATE=HEXA(IELEM)%GPT_VSTATE_T
           DO INODE=1,ELE_NODE   ! SETUP ELEMENT-NODE CONNECTIVITY LIST
              II = I + OFFSET(1,IPARS_ORDER(INODE))
              JJ = J + OFFSET(2,IPARS_ORDER(INODE))
              KK = K + OFFSET(3,IPARS_ORDER(INODE))
              HEXA(IELEM)%NODE(INODE)=NODE_LID(II,JJ,KK)
           END DO
           DO JNODE=1,ELE_NODE
               HEXA(IELEM)%NODE_ST_DOF(JNODE) &
               =NODE_ST_DOF(HEXA(IELEM)%NODE(JNODE))
           END DO
         ENDIF
      ENDDO
   ENDDO
ENDDO
! MODIFY ELEMENT-NODE CONNECTIVITY LIST TO ACCOUNT FOR OPEN FRACTURE NODE
DO CTR=1,LFALLSIZE
   NODE=OFNODE_AFFINE(4,CTR)
   ELEM=OFNODE_AFFINE(5,CTR)
   DO IELEM=1,ELEM
      ELEM2=OFNODE_AFFINE(5+IELEM,CTR)  
      DO INODE=1,ELE_NODE
         IF(HEXA(ELEM2)%NODE(INODE).EQ.NODE) THEN
           HEXA(ELEM2)%NODE(INODE)=CTR+LALLSIZE
           HEXA(ELEM2)%NODE_ST_DOF(INODE)=NODE_ST_DOF(CTR+LALLSIZE)
         ENDIF
      ENDDO
   ENDDO
ENDDO

! AFTER ELEMENT-NODE CONNECTIVITY BEING UPDATED, CALCULATE ELEMENT CENTER COORDINATE
DO IELEM=1,ELE_TL
   R=ZERO_D
   S=ZERO_D
   T=ZERO_D
   CALL POINT_COORD(IELEM,R,S,T,X,Y,Z)
   HEXA(IELEM)%CENTER(1)=X
   HEXA(IELEM)%CENTER(2)=Y
   HEXA(IELEM)%CENTER(3)=Z
ENDDO


50 continue

TOTAL_DISP_BC=6*(IDIM*JDIM+JDIM*KDIM+IDIM*KDIM)
TOTAL_DISP_FBC=MAX(LFALLSIZE*3,1)
TOTAL_TRAC_BC=TOTAL_DISP_BC
TOTAL_TRAC_FBC=TOTAL_DISP_FBC

IF (.NOT.ALLOCATED(DISP_BC)) ALLOCATE(DISP_BC(3,TOTAL_DISP_BC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "1 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
IF (.NOT.ALLOCATED(TRAC_BC)) ALLOCATE(TRAC_BC(3,TOTAL_TRAC_BC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "2 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
IF (.NOT.ALLOCATED(DISP_FBC)) ALLOCATE(DISP_FBC(3,TOTAL_DISP_FBC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "3 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
IF (.NOT.ALLOCATED(TRAC_FBC)) ALLOCATE(TRAC_FBC(3,TOTAL_TRAC_FBC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "4 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
DISP_BC=0.D0
TRAC_BC=0.D0
DISP_FBC=0.D0
TRAC_FBC=0.D0

TOTAL_DISP_BC=0
TOTAL_DISP_FBC=0
TOTAL_TRAC_BC=0
TOTAL_TRAC_FBC=0

! ADDED THE FOLLOWING ON 3RD MAY
IF (.NOT.ALLOCATED(ZERO_ADD1)) ALLOCATE(ZERO_ADD1(20000),STAT=IERR)
IF (.NOT.ALLOCATED(ZERO_ADD2)) ALLOCATE(ZERO_ADD2(20000),STAT=IERR)
BD_FACE_COUNT=0
! NUMBER OF ELEMENT FACES ON ELASTIC-POROELASTIC BOUNDARY

DO K=KL1+1,KL2-1
DO J=JL1V(K)+1,JL2V(K)-1
DO I=IL1+1,IL2-1
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I-1,J,K).EQ.0) THEN
   ! -X FACE OF THE ELASTIC-POROELASTIC INTERFACE
      BD_FACE_COUNT=BD_FACE_COUNT+1
      ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K)
      ! ELEMENT ID
      ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(1)
      ! ORIENTATION OF THOSE FACES
      BD_FACE_COUNT=BD_FACE_COUNT+1
      ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I-1,J,K)
      ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(2)
   ENDIF 
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I+1,J,K).EQ.0) THEN
   ! +X FACE OF THE ELASTIC-POROELASTIC INTERFACE
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K)
     ! ELEMENT ID
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(2)
     ! ORIENTATION OF THOSE FACES
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I+1,J,K)
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(1)
   ENDIF 
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I,J-1,K).EQ.0) THEN
   ! -Y FACE OF THE ELASTIC-POROELASTIC INTERFACE
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K)
     ! ELEMENT ID
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(3)
     ! ORIENTATION OF THOSE FACES
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J-1,K)
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(4)
   ENDIF 
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I,J+1,K).EQ.0) THEN
   ! +Y FACE OF THE ELASTIC-POROELASTIC INTERFACE
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K)
     ! ELEMENT ID
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(4)
     ! ORIENTATION OF THOSE FACES
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J+1,K)
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(3)
   ENDIF 
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I,J,K-1).EQ.0) THEN
   ! -Z FACE OF THE ELASTIC-POROELASTIC INTERFACE
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=IELEM
     ! ELEMENT ID
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(5)
     ! ORIENTATION OF THOSE FACES
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K-1)
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(6)
   ENDIF 
   IF(BIOTA(I,J,K).GT.0.AND.BIOTA(I,J,K+1).EQ.0) THEN
   ! +Z FACE OF THE ELASTIC-POROELASTIC INTERFACE
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=IELEM
     ! ELEMENT ID
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(6)
     ! ORIENTATION OF THOSE FACES
     BD_FACE_COUNT=BD_FACE_COUNT+1
     ZERO_ADD1(BD_FACE_COUNT)=ELEM_LID(I,J,K+1)
     ZERO_ADD2(BD_FACE_COUNT)=IPARS_FACE_ORDER(5)
   ENDIF 
ENDDO
ENDDO
ENDDO
! ADDED THE ABOVE ON 3RD MAY

! -X FACE
DO K=1,KDIM
   DO J=1,JDIM
      DO DIR = 1,3
         IF (BDTYP1(J,K,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO I = IL1,IL2+1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I-1,J,K).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP1(J,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP1(J,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP1(J,K,DIR).EQ.1) THEN ! NEUMANN B.C.
            DO I = IL1,IL2+1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I-1,J,K).EQ.0) THEN
                  TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC1(J,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC1(J,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            
                   
! +X FACE
DO K = 1,KDIM
   DO J = 1,JDIM
      DO DIR = 1,3
         IF (BDTYP2(J,K,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO I = IL2+1,IL1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I+1,J,K).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP2(J,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP2(J,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP2(J,K,DIR).EQ.1) THEN ! NEUMANN B.C.
            DO I = IL2+1,IL1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I+1,J,K).EQ.0) THEN
                  TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC2(J,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC2(J,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            

! -Y FACE
DO K = 1,KDIM
   DO I = IL1,IL2+1
      DO DIR = 1,3
         IF (BDTYP3(I,K,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO J = 2,JDIM
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J-1,K).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP3(I,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP3(J,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP3(I,K,DIR).EQ.1) THEN ! NEUMANN B.C.
            DO J = 2,JDIM
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J-1,K).EQ.0) THEN
                  TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC3(I,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC3(I,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            

! +Y FACE
DO K = 1,KDIM
   DO I = IL1,IL2+1
      DO DIR = 1,3
         IF (BDTYP4(I,K,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO J = JDIM-1,1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J+1,K).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP4(I,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP4(I,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP4(I,K,DIR).EQ.1) THEN ! NEUMANN B.C.
            TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
            DO J = JDIM-1,1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J+1,K).EQ.0) THEN
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC4(I,K,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC4(I,K,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            

! -Z FACE
DO J = 1,JDIM
   DO I = IL1,IL2+1
      DO DIR = 1,3
         IF (BDTYP5(I,J,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO K = 2,KDIM
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J,K-1).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP5(I,J,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP5(I,J,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP5(I,J,DIR).EQ.1) THEN ! NEUMANN B.C.
            DO K = 2,KDIM
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J,K-1).EQ.0) THEN
                  TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC5(I,J,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC5(I,J,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            

! +Z FACE
DO J = 1,JDIM
   DO I = IL1,IL2+1
      DO DIR = 1,3
         IF (BDTYP6(I,J,DIR).EQ.2) THEN ! DISPLACEMENT B.C.
            DO K = KDIM-1,1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J,K+1).EQ.0) THEN
                  TOTAL_DISP_BC=TOTAL_DISP_BC+1
                  DISP_BC(1,TOTAL_DISP_BC)=NODE_LID(I,J,K)
                  DISP_BC(2,TOTAL_DISP_BC)=DIR
                  DISP_BC(3,TOTAL_DISP_BC)=BDDISP6(I,J,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_DISP_FBC=TOTAL_DISP_FBC+1
                    DISP_FBC(1,TOTAL_DISP_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    DISP_FBC(2,TOTAL_DISP_FBC)=DIR
                    DISP_FBC(3,TOTAL_DISP_FBC)=BDDISP6(I,J,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (BDTYP6(I,J,DIR).EQ.1) THEN ! NEUMANN B.C.
            DO K = KDIM-1,1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND. &
                  KEYOUT_CR(I,J,K+1).EQ.0) THEN
                  TOTAL_TRAC_BC=TOTAL_TRAC_BC+1
                  TRAC_BC(1,TOTAL_TRAC_BC)=NODE_LID(I,J,K)
                  TRAC_BC(2,TOTAL_TRAC_BC)=DIR
                  TRAC_BC(3,TOTAL_TRAC_BC)=BDTRAC6(I,J,DIR)
                  IF(OFNODE_LID(I,J,K).GT.0) THEN
                    TOTAL_TRAC_FBC=TOTAL_TRAC_FBC+1
                    TRAC_FBC(1,TOTAL_TRAC_FBC)=OFNODE_LID(I,J,K)+&
                                               LALLSIZE
                    TRAC_FBC(2,TOTAL_TRAC_FBC)=DIR
                    TRAC_FBC(3,TOTAL_TRAC_FBC)=BDTRAC6(I,J,DIR)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
ENDDO            

! COPY NODAL-BASED DISP AND TRAC BC TO POROHEX ARRAYS AND FREE TEMP MEMORY
TL_0_DCH=TOTAL_DISP_BC+TOTAL_DISP_FBC
! ZERO_VALUE() IS DIRICHLET BC INPUT
! ZERO_DCH() IS THE POSITION OF THE DOF
!CAUTION: ZERO_DCH() POINTS TO GLOBAL DOF, NOT LOCAL!!!
IF(TL_0_DCH>0) THEN
   IF (.NOT.ALLOCATED(ZERO_VALUE)) &
     ALLOCATE(ZERO_VALUE(TL_0_DCH),ZERO_DCH(TL_0_DCH),STAT=IERR)
   IF (IERR.GT.0) THEN
      WRITE(*,*) "5 NOT ENOUGH MEMORY IN PRE_PRCSS"
      STOP 462
   ENDIF
   DO INC=1,TOTAL_DISP_BC
      LOAD_NODE=INT(DISP_BC(1,INC))
      LOAD_DOF=INT(DISP_BC(2,INC))
      ZERO_VALUE(INC)=DISP_BC(3,INC)
      ZERO_DCH(INC)=NODE_ST_DOF(LOAD_NODE)+LOAD_DOF-1
   END DO
   DO INC=1,TOTAL_DISP_FBC
      LOAD_NODE=INT(DISP_FBC(1,INC))
      LOAD_DOF=INT(DISP_FBC(2,INC))
      ZERO_VALUE(INC+TOTAL_DISP_BC)=DISP_FBC(3,INC)
      ZERO_DCH(INC+TOTAL_DISP_BC)=NODE_ST_DOF(LOAD_NODE)+LOAD_DOF-1
   END DO
END IF

!FREE TEMPORARY MEMORY
DEALLOCATE(DISP_BC,DISP_FBC)

!
!SETTING PRESCIBED NODAL FORCE B.C.TO COMMON BLOCK
!
TL_LOAD_POINT=TOTAL_TRAC_BC+TOTAL_TRAC_FBC
IF (.NOT.ALLOCATED(LOAD_CNST)) ALLOCATE(LOAD_CNST(DOF_TL),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "6 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
! ADDED THE FOLLOWING ON 4TH MAY
IF (.NOT.ALLOCATED(LOAD_CNST1)) ALLOCATE(LOAD_CNST1(DOF_TL),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "6 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
! ADDED THE ABOVE ON 4TH MAY

! ALLOCATE TEMPORARY ARRAY FOR FACIAL-BASED TRACTION AND PRESSURE BC
TOTAL_FACE_LOAD_BC=2*(IDIM*JDIM+JDIM*KDIM+IDIM*KDIM)
TOTAL_PRES_LOAD_BC=TOTAL_FACE_LOAD_BC
TOTAL_PRES_LOAD_FBC=MAX(2*TOTAL_CRAC_FACE,1)

IF (.NOT.ALLOCATED(FACE_LOAD_BC)) &
  ALLOCATE(FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "7 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
IF (.NOT.ALLOCATED(PRES_LOAD_BC)) &
  ALLOCATE(PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "8 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
IF (.NOT.ALLOCATED(PRES_LOAD_FBC)) &
  ALLOCATE(PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "9 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF
FACE_LOAD_BC=0.D0
PRES_LOAD_BC=0.D0
PRES_LOAD_FBC=0.D0
 
! CALCULATE TOTAL_FACE_LOAD_BC,LOOP OVER ELEMENTS
TOTAL_FACE_LOAD_BC=0
TOTAL_PRES_LOAD_BC=0
TOTAL_PRES_LOAD_FBC=0

! -X FACE
DO K = 1,KDIM
   DO J = 1,JDIM
      DO I = IL1,IL2
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I-1,J,K).EQ.0) THEN 
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=1
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC1(J,K,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC1(J,K,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC1(J,K,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=1
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE1(J,K)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! +X FACE
DO K = 1,KDIM
   DO J = 1,JDIM
      DO I = IL2,IL1,-1
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I+1,J,K).EQ.0) THEN      
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=2
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC2(J,K,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC2(J,K,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC2(J,K,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=2
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE2(J,K)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! -Y FACE
DO K = 1,KDIM
   DO I = IL1,IL2
      DO J = 2,JDIM
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I,J-1,K).EQ.0) THEN 
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=3
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC3(I,K,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC3(I,K,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC3(I,K,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=3
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE3(I,K)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! +Y FACE
DO K = 1,KDIM
   DO I = IL1,IL2
      DO J = JDIM-1,1,-1
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I,J+1,K).EQ.0) THEN 
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=4
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC4(I,K,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC4(I,K,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC4(I,K,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=4
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE4(I,K)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! -Z FACE
DO J =1,JDIM
   DO I = IL1,IL2
      DO K = 2,KDIM
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I,J,K-1).EQ.0) THEN 
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=5
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC5(I,J,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC5(I,J,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC5(I,J,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=5
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE5(I,J)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! +Z FACE
DO J =1,JDIM
   DO I = IL1,IL2
      DO K = KDIM-1,1,-1
         IF(ELEM_LID(I,J,K).GT.0 .AND. KEYOUT(I,J,K+1).EQ.0) THEN 
           TOTAL_FACE_LOAD_BC=TOTAL_FACE_LOAD_BC+1
           FACE_LOAD_BC(1,TOTAL_FACE_LOAD_BC)=ELEM_LID(I,J,K)
           FACE_LOAD_BC(2,TOTAL_FACE_LOAD_BC)=6
           FACE_LOAD_BC(3,TOTAL_FACE_LOAD_BC)=BDFTRAC6(I,J,1)
           FACE_LOAD_BC(4,TOTAL_FACE_LOAD_BC)=BDFTRAC6(I,J,2)
           FACE_LOAD_BC(5,TOTAL_FACE_LOAD_BC)=BDFTRAC6(I,J,3)
           TOTAL_PRES_LOAD_BC=TOTAL_PRES_LOAD_BC+1
           PRES_LOAD_BC(1,TOTAL_PRES_LOAD_BC)=ELEM_LID(I,J,K)
           PRES_LOAD_BC(2,TOTAL_PRES_LOAD_BC)=6
           PRES_LOAD_BC(3,TOTAL_PRES_LOAD_BC)=PRESFACE6(I,J)
           EXIT
         ENDIF
      ENDDO
   ENDDO 
ENDDO

! FRACTURE FACE INDUCED PRESSURE BOUNDARY CONDITION
DO FRAC = 1, NUMFRAC
   DO FACE = 1, NUMFRACFACE(FRAC)
      I = FRACFACE(1,FACE,FRAC) - IOFF
      J = FRACFACE(2,FACE,FRAC) - JOFF
      K = FRACFACE(3,FACE,FRAC) - KOFF
      IFACE=FRACFACE(4,FACE,FRAC)
      IF (FRACFACEPROC(FACE,FRAC).EQ.0) CYCLE 
      
      IF (ELEM_LID(I,J,K).GT.0) THEN
         TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
         PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I,J,K)
         PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=IFACE
      ENDIF
      IF(IFACE.EQ.1) THEN
        IF(ELEM_LID(I-1,J,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I-1,J,K)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=2
        ENDIF 
      ELSEIF(IFACE.EQ.2) THEN
        IF(ELEM_LID(I+1,J,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I+1,J,K)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=1
        ENDIF 
      ELSEIF(IFACE.EQ.3) THEN
        IF(ELEM_LID(I,J-1,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I,J-1,K)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=4
        ENDIF 
      ELSEIF(IFACE.EQ.4) THEN
        IF(ELEM_LID(I,J+1,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I,J+1,K)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=3
        ENDIF 
      ELSEIF(IFACE.EQ.5) THEN
        IF(ELEM_LID(I,J,K-1).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I,J,K-1)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=6
        ENDIF 
      ELSEIF(IFACE.EQ.6) THEN
        IF(ELEM_LID(I,J,K+1).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(1,TOTAL_PRES_LOAD_FBC)=ELEM_LID(I,J,K+1)
          PRES_LOAD_FBC(2,TOTAL_PRES_LOAD_FBC)=5
        ENDIF 
      ENDIF
   ENDDO
ENDDO

!
!SETUP PRESSURE AND TRACTION SURFACE TO COMMON BLOCK
!
TL_PRESSURED_FACE=TOTAL_PRES_LOAD_BC+TOTAL_PRES_LOAD_FBC
TL_TRACTION_FACE=TOTAL_FACE_LOAD_BC
IF(TL_PRESSURED_FACE+TL_TRACTION_FACE>0) THEN
   IF (.NOT.ALLOCATED(B_FACE)) &
     ALLOCATE(B_FACE(TL_PRESSURED_FACE+TL_TRACTION_FACE),STAT=IERR)
   IF (IERR.GT.0) THEN
      WRITE(*,*) "10 NOT ENOUGH MEMORY IN PRE_PRCSS"
      STOP 462
   ENDIF
   IF(TOTAL_PRES_LOAD_BC>0) THEN
      DO INC=1,TOTAL_PRES_LOAD_BC
         ELEM=INT(PRES_LOAD_BC(1,INC))
         IF (ELEM.EQ.0) THEN
            WRITE(*,*) "WARNING::IN PRE_PRCSS, ELEM ID=0 FOR ", &
            "PRES_LOAD_BC(1,:)=",PRES_LOAD_BC(1,INC)
            STOP 13
         ENDIF
         B_FACE(INC)%ELEMENT=INT(PRES_LOAD_BC(1,INC))
         B_FACE(INC)%ORIEN=IPARS_FACE_ORDER(INT(PRES_LOAD_BC(2,INC)))
         B_FACE(INC)%PRESSURE_VALUE=PRES_LOAD_BC(3,INC)
         B_FACE(INC)%PRESSURE=1
         B_FACE(INC)%TRACTION=0.0D0
      END DO
! FRACTURE INDUCED PRESSURE B.C. IS INPUT AT EACH TIME STEP
   END IF
   IF(TL_TRACTION_FACE>0) THEN
      DO INC=TL_PRESSURED_FACE+1,TL_TRACTION_FACE+TL_PRESSURED_FACE
         III=INC-TL_PRESSURED_FACE
         ELEM=INT(FACE_LOAD_BC(1,III))
         IF (ELEM.EQ.0) THEN
            WRITE(*,*) "WARNING::IN PRE_PRCSS, ELEM ID=0 FOR ", &
            "PRES_LOAD_FBC(1,:)=",FACE_LOAD_BC(1,III)
            STOP 13
         ENDIF
         B_FACE(INC)%ELEMENT=INT(FACE_LOAD_BC(1,III))
         B_FACE(INC)%ORIEN=IPARS_FACE_ORDER(INT(FACE_LOAD_BC(2,III)))
         B_FACE(INC)%TRACTION(1:3)=FACE_LOAD_BC(3:5,III)
         B_FACE(INC)%PRESSURE=0
         B_FACE(INC)%PRESSURE_VALUE=0.0D0
      END DO
   END IF
END IF
!FREE UP TEMPORARY TRACTION AND PRESSURE B.C. ARRAY
DEALLOCATE(FACE_LOAD_BC,PRES_LOAD_BC)

! bag8
IF (NNTIM.EQ.1) GOTO 100

IF (.NOT.ALLOCATED(TL_LOAD)) &
  ALLOCATE(TL_LOAD(DOF_TL),STAT=IERR)
IF (.NOT.ALLOCATED(RESIDUE)) &
  ALLOCATE(RESIDUE(DOF_TL),STAT=IERR) 
! ADDED THE FOLLOWING ON 4TH MAY
IF (.NOT.ALLOCATED(RESIDUE1)) &
  ALLOCATE(RESIDUE1(DOF_TL),STAT=IERR)
! ADDED THE ABOVE ON 4TH MAY
ARRSIZE=MAX(1,GFSIZE)
IF (.NOT.ALLOCATED(OFNODE_DISP_TMP)) &
  ALLOCATE(OFNODE_DISP_TMP(3,ARRSIZE),STAT=IERR)
IF (.NOT.ALLOCATED(OFNODE_GNUM_TMP)) &
  ALLOCATE(OFNODE_GNUM_TMP(ARRSIZE),STAT=IERR)
IF (.NOT.ALLOCATED(HYPRE_ROWS)) &
  ALLOCATE(HYPRE_ROWS(DOF_TL),STAT=IERR)
IF (IERR.GT.0) THEN
   WRITE(*,*) "11 NOT ENOUGH MEMORY IN PRE_PRCSS"
   STOP 462
ENDIF

CALL GET_GSTIFF_INDEX
!
!SETUP CRACK_PROFILE FOR PLOTTING, ONLY WORK FOR SINGLE PROCESSOR
!
IF (NUMPRC.EQ.1 .AND. NUMFRAC .GT. 0) THEN
   TL_CRACKED_FACE=TOTAL_CRAC_FACE
   TOTAL_FRAC_ELEM=TL_CRACKED_FACE
   TOTAL_FRAC_INTERIOR_NODE=LFALLSIZE
   TOTAL_FRAC_NODE=0
   DO K=1,KDIM
      DO J=1,JDIM
         DO I=IL1,IL2+1
            IF(NODE_LID(I,J,K).GT.0 .AND. FNODE_TYPE(I,J,K).GT.0) THEN
              TOTAL_FRAC_NODE=TOTAL_FRAC_NODE+1
            ENDIF
         ENDDO
      ENDDO
   ENDDO 
   IF(TOTAL_FRAC_NODE.GT.0) THEN
      ALLOCATE(FRAC_NODE_GLOBAL_ID(TOTAL_FRAC_NODE),STAT=IERR)
      ALLOCATE(FRAC_NODE_COORD(3,TOTAL_FRAC_NODE),STAT=IERR)
      ALLOCATE(FRAC_NODE_WIDTH(3,TOTAL_FRAC_NODE),STAT=IERR)
      ALLOCATE(FRAC_ELEM_CONNECT(4,TOTAL_FRAC_ELEM),STAT=IERR)
      ALLOCATE(FRAC_INTERIOR_NODE_ID(TOTAL_FRAC_INTERIOR_NODE),STAT=IERR)
      ALLOCATE(FRAC_INTERIOR_NODE_AFFINE(TOTAL_FRAC_INTERIOR_NODE),STAT=IERR)
      IF (IERR.GT.0) THEN
         WRITE(*,*) "12 NOT ENOUGH MEMORY IN PRE_PRCSS"
         STOP 462
      ENDIF
      DO NODE=1,LFALLSIZE
         FRAC_INTERIOR_NODE_ID(NODE)=NODE+LALLSIZE
         FRAC_INTERIOR_NODE_AFFINE(NODE)=OFNODE_AFFINE(4,NODE)
         FRAC_NODE_WIDTH(1:3,1:TOTAL_FRAC_NODE)=0.0D0
      ENDDO
      CTR=0
      DO K=1,KDIM
         DO J=1,JDIM
            DO I=IL1,IL2+1
               IF(NODE_LID(I,J,K).GT.0 .AND. FNODE_TYPE(I,J,K).GT.0) THEN
                 CTR=CTR+1
                 FRAC_NODE_GLOBAL_ID(CTR)=NODE_LID(I,J,K)
                 FRAC_NODE_COORD(1:3,CTR)=COORD(NODE_LID(I,J,K),1:3)
               ENDIF
            ENDDO
         ENDDO
      ENDDO 
      IFACE=0
      DO FRAC = 1, NUMFRAC
         DO FACE = 1, NUMFRACFACE(FRAC)
            I = FRACFACE(1,FACE,FRAC) - IOFF
            J = FRACFACE(2,FACE,FRAC) - JOFF
            K = FRACFACE(3,FACE,FRAC) - KOFF
            IF(I.LT.2 .OR. I.GT.(IDIM-1)) CYCLE
            IF(J.LT.2 .OR. J.GT.(JDIM-1)) CYCLE
            IF(K.LT.2 .OR. K.GT.(KDIM-1)) CYCLE
            ELE_ID=ELEM_LID(I,J,K)
            IF(ELE_ID.LE.0) CYCLE
            IFACE=IFACE+1
            FACE_ID=FRACFACE(4,FACE,FRAC)
            DO JNODE=1,4
               NODE_ID=IPARS_FACE_NODE(JNODE,FACE_ID)
               II=I+OFFSET(1,NODE_ID)
               JJ=J+OFFSET(2,NODE_ID)
               KK=K+OFFSET(3,NODE_ID)
               GNODE=NODE_LID(II,JJ,KK)
               FRAC_ELEM_CONNECT(JNODE,IFACE)=GNODE
            END DO
         ENDDO
      ENDDO
   ENDIF
ENDIF
!
!
!set up parallel data structures
!
!
!if(NUMPRC>1) then
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call fem_parallel(ILOWER,IUPPER)
!end if

100  CONTINUE

!CAUTION: LOAD_CNST IS INDEXED USING LOCAL DOF ID!!!
LOAD_CNST=ZERO_D
IF(TL_LOAD_POINT>ZERO) THEN
   DO ILOAD=1,TOTAL_TRAC_BC
      LOAD_NODE=INT(TRAC_BC(1,ILOAD))
      LOAD_DOF=INT(TRAC_BC(2,ILOAD))
      LOAD_CNST(NODE_DOF*(LOAD_NODE-1)+LOAD_DOF)=TRAC_BC(3,ILOAD)
   END DO
   DO ILOAD=1,TOTAL_TRAC_FBC
      LOAD_NODE=INT(TRAC_FBC(1,ILOAD))
      LOAD_DOF=INT(TRAC_FBC(2,ILOAD))
      LOAD_CNST(NODE_DOF*(LOAD_NODE-1)+LOAD_DOF)=TRAC_FBC(3,ILOAD)
   END DO
END IF

! SETUP TIME-DEPENDENT PROPERTIES: ELEMENT BULK DENSITY, PORE PRESSURE, AND
!       FRACTURE INDUCED PRESSURE B.C.

! SETUP BULK DENSITY AND PORE PRESSURE IN COMMON BLOCK ARRAY EVERY TIME STEP
DO K=KL1-1,KL2+1
   JL1 = MIN(JL1V(K-1),JL1V(K),JL1V(K+1))
   JL2 = MAX(JL2V(K-1),JL2V(K),JL2V(K+1))
   DO J=JL1-1,JL2+1
     DO I=IL1,IL2
        IELEM=ELEM_LID(I,J,K)
        IF(IELEM.GT.0) THEN
           MAT_PRPTY(IELEM)%PRPTY_SLD(8)= BULKDEN(I,J,K)
           HEXA(IELEM)%PORE_PRESSURE=PRESS(I,J,K)
         ENDIF
      ENDDO
   ENDDO
ENDDO

! FRACTURE FACE INDUCED PRESSURE BOUNDARY CONDITION
! UPDATE PRESSURE VALUE AT EACH TIME STEP
TOTAL_PRES_LOAD_FBC=0
DO FRAC = 1, NUMFRAC
   DO FACE = 1, NUMFRACFACE(FRAC)
      I = FRACFACE(1,FACE,FRAC) - IOFF
      J = FRACFACE(2,FACE,FRAC) - JOFF
      K = FRACFACE(3,FACE,FRAC) - KOFF
      IFACE=FRACFACE(4,FACE,FRAC)
      IF (FRACFACEPROC(FACE,FRAC).EQ.0) CYCLE 
  
      IF (ELEM_LID(I,J,K).GT.0) THEN
         TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
         PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
      ENDIF
      IF(IFACE.EQ.1) THEN
        IF(ELEM_LID(I-1,J,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ELSEIF(IFACE.EQ.2) THEN
        IF(ELEM_LID(I+1,J,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ELSEIF(IFACE.EQ.3) THEN
        IF(ELEM_LID(I,J-1,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ELSEIF(IFACE.EQ.4) THEN
        IF(ELEM_LID(I,J+1,K).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ELSEIF(IFACE.EQ.5) THEN
        IF(ELEM_LID(I,J,K-1).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ELSEIF(IFACE.EQ.6) THEN
        IF(ELEM_LID(I,J,K+1).GT.0) THEN
          TOTAL_PRES_LOAD_FBC=TOTAL_PRES_LOAD_FBC+1
          PRES_LOAD_FBC(3,TOTAL_PRES_LOAD_FBC)=-PFN(FACE,FRAC)
        ENDIF 
      ENDIF
   ENDDO
ENDDO
!
!SETUP FRACTURE INDUCED PRESSURE SURFACE TO COMMON BLOCK
!
IF(TOTAL_PRES_LOAD_FBC>0) THEN
   DO INC=1,TOTAL_PRES_LOAD_FBC
      III=(TL_PRESSURED_FACE-TOTAL_PRES_LOAD_FBC)+INC
      ELEM=INT(PRES_LOAD_FBC(1,INC))
      IF (ELEM.EQ.0) THEN
         WRITE(*,*) "WARNING::IN PRE_PRCSS, ELEM ID=0 FOR ", &
         "PRES_LOAD_FBC(1,:)=",PRES_LOAD_FBC(1,INC)
         STOP 13
      ENDIF
      B_FACE(III)%ELEMENT=INT(PRES_LOAD_FBC(1,INC))
      B_FACE(III)%ORIEN=IPARS_FACE_ORDER(INT(PRES_LOAD_FBC(2,INC)))
      B_FACE(III)%PRESSURE_VALUE=PRES_LOAD_FBC(3,INC)
      B_FACE(III)%PRESSURE=1
      B_FACE(III)%TRACTION=0.0D0
   END DO
END IF

! AT END OF SIMULATION, FREE UP TEMPORARY FRACTURE PRESSURE B.C. ARRAY
200 CONTINUE

IF (NNTIM.EQ.2) THEN
   DEALLOCATE(PRES_LOAD_FBC,TRAC_BC,TRAC_FBC)
ENDIF

END SUBROUTINE

!**********************************************************************

SUBROUTINE CHANGE_MECH_BC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
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
IMPLICIT NONE
INCLUDE 'emodel.h'

INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2
INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
INTEGER NBLK
REAL*8  TIME
INTEGER MYPRC,NUMPRC,NNTIM
REAL*8  XC(IDIM+1,JDIM+1,KDIM+1),YC(IDIM+1,JDIM+1,KDIM+1),&
        ZC(IDIM+1,JDIM+1,KDIM+1)
INTEGER KEYOUT_CR(IDIM,JDIM,KDIM),NODE_LID(IDIM,JDIM,KDIM)
INTEGER PASSO(IDIM,JDIM,KDIM),HARDEN_MODEL(IDIM,JDIM,KDIM)
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
REAL*8  PRESFACE1(JDIM,KDIM),PRESFACE2(JDIM,KDIM),&
        PRESFACE3(IDIM,KDIM),PRESFACE4(IDIM,KDIM),&
        PRESFACE5(IDIM,JDIM),PRESFACE6(IDIM,JDIM)
REAL*8  PRESSVAL(IDIM,JDIM,KDIM)

CHARACTER*50 :: CASESTRING = '(unknown)'


IF (MECH_BC_NCASE.EQ.-1) THEN

  !----------------------------------------------------------
  ! Set displacements to zero after initialization
  !----------------------------------------------------------
  IF (TIME.GT.0.D0) THEN
    CASESTRING = '(Nonzero Dirichlet)'
    BDDISP1 = 0.D0
    BDDISP2 = 0.D0
    BDDISP3 = 0.D0
    BDDISP4 = 0.D0
    BDDISP5 = 0.D0
    BDDISP6 = 0.D0
  ENDIF

ELSEIF (MECH_BC_NCASE.EQ.1) THEN

  !----------------------------------------------------------
  ! Bradley problem
  ! (example 2 in RSC 2017 paper)
  !----------------------------------------------------------
  CASESTRING = '(Bradley Example for RSC 2017)'
  IF (TIME.LE.100.D0) THEN
    PRESFACE5 = 4650.D0 + (7000.D0 - 4650.D0)*TIME/100.D0
  ENDIF
!  IF (MYPRC.EQ.0) THEN
!    WRITE(*,'(A,F9.2,A,F9.2)') &
!      'Bradley: TIME=',TIME,' PRESFACE5=',PRESFACE5(3,3)
!  ENDIF

ELSEIF (MECH_BC_NCASE.EQ.100) THEN

  !----------------------------------------------------------
  ! Mandel problem
  !----------------------------------------------------------
  CASESTRING = '(Mandel Problem)'
  CALL MANDEL_BC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,KL2, &
                 KEYOUT,NBLK,BDDISP4,XC,YC,ZC,NODE_LID,KEYOUT_CR, &
                 BDTYP4,PRESSVAL)

ELSE

  STOP 'Unknown NCASE in CHANGE_MECH_BC'

ENDIF

IF (MYPRC.EQ.0) THEN
  WRITE(*,'(1X,A,I5,2A)')'In CHANGE_MECH_BC, NCASE=',MECH_BC_NCASE, &
                     ' ',TRIM(CASESTRING)
ENDIF

END SUBROUTINE

