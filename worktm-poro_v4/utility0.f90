
subroutine point_coord(ielem,r,s,t,x,y,z) 
!bw find global x,y,z by local r,s,t coordinates 
use truth                                      
use model_type
use system                                     
implicit none                                  
integer i,j,orien,igauss,master,face_ID   
integer ielem                                  
real(kind=double),allocatable::node_c(:,:)     
real(kind=double),allocatable::deriv(:,:)      
real(kind=double),allocatable::shap(:)         
real(kind=double) r,s,t,x,y,z                  
ele_node=hexa(ielem)%tl_node                                 
allocate(deriv(ele_node,3))                    
allocate(node_c(ele_node,3))                   
allocate(shap(ele_node))                       
ele_type=hexa(ielem)%ele_type                      
call projection(one,ielem,node_c)                
call shap_g(r,s,t,shap,deriv)             
x=zero_d                                       
y=zero_d                       
z=zero_d                       
do j=1,ele_node                
   x=x+node_c(j,1)*shap(j)     
   y=y+node_c(j,2)*shap(j)     
   z=z+node_c(j,3)*shap(j)     
end do                         
deallocate(shap,node_c,deriv)  
return                         
end                            
 
subroutine projection(mcase,ielem,node_coor)                      
use truth                                                       
use model_type                                             
use system                                                      
implicit none                                                   
integer mcase,inode,node_id,jdof,st_dof,ielem                   
real(kind=double) node_coor(ele_node,3)                         
if(mcase==1) then                                               
   do inode=1,ele_node                                          
         node_id=hexa(ielem)%node(inode)                        
         node_coor(inode,1:3)=coord(node_id,1:3)                
   end do                                                       
end if                                                          
if(mcase==2) then                                               
   do inode=1,ele_node 
     st_dof=hexa(ielem)%node_st_dof(inode) 
     do jdof=1,3 
        node_coor(inode,jdof)=delta_u(st_dof+jdof-1) 
    end do                                                    
   end do                                                       
end if  
if(mcase==3) then                                               
   do inode=1,ele_node 
     st_dof=hexa(ielem)%node_st_dof(inode) 
     do jdof=1,3 
        node_coor(inode,jdof)=u(st_dof+jdof-1) 
    end do                                                    
   end do                                                       
end if                                                  
return 
end  



subroutine update_1
use truth
use model_type
use system
implicit none
integer i
real(kind=double) ur  
!call average_delta_u
!delta_u=delta_u+tl_load
!call average_delta_u
!do i=1,node_tl
!   ur=delta_u(3*(i-1)+1)*delta_u(3*(i-1)+1)
!   ur=ur+delta_u(3*(i-1)+2)*delta_u(3*(i-1)+2)
!   ur=sqrt(ur)
!   write(500,*)'i,ur=====',i,ur     
!end do
return

end


subroutine update_2
use truth
use model_type
use system
implicit none
integer ielem,iface,igauss,j
real(kind=double) stress(6),strain(6),aaa
!BW delta_u=tl_load
u=tl_load
! upt_old=upt_new
do ielem=1,ele_tl
   do igauss=1,8
      call get_strain(ielem,igauss,strain)
      call get_stress(ielem,igauss,stress)
      hexa(ielem)%gpt_strss(igauss,1:6)=stress
      hexa(ielem)%gpt_strss_t(igauss,1:6)=stress
      hexa(ielem)%gpt_strn(igauss,1:6)=strain
      hexa(ielem)%gpt_strn_t(igauss,1:6)=strain
   end do
   aaa=0.0d0
   do igauss=1,8
      do j=1,3
         aaa=aaa+hexa(ielem)%gpt_strn(igauss,j)
      end do
   end do
   aaa=aaa/8.0d0
   hexa(ielem)%vstrain=aaa
end do
return
end


subroutine node_share_coord  !bw Find nodes sharing the same coordiates                                
use truth                    !   geometrically the same node
use model_type                                       
use system                                           
implicit none                                        
!bw integer ielem,node_id,inode,jnode,mask(node_tl)
integer ielem,node_id,inode,jnode
!bw dynamically allocate mask()
integer,allocatable :: mask(:)
integer ierr
integer add_node
allocate (mask(node_tl),STAT=ierr)
if (ierr.ne.0) stop 462
mask=zero                         
cg_tl_node=zero       
do inode=1,node_tl
   if(mask(inode)==one) cycle
   mask(inode)=one 
   add_node=one    
   cg_tl_node=cg_tl_node+one
   cg_node(cg_tl_node,1)=inode              
   do jnode=inode+1,node_tl
      if(mask(jnode)==one) cycle
      if(abs(coord(inode,1)-coord(jnode,1))>1.0e-3) cycle
      if(abs(coord(inode,2)-coord(jnode,2))>1.0e-3) cycle
      if(abs(coord(inode,3)-coord(jnode,3))>1.0e-3) cycle
      mask(jnode)=one
      add_node=add_node+one
      cg_node(cg_tl_node,add_node)=jnode
   end do
   cg_node(cg_tl_node,9)=add_node
end do
!bw
deallocate(mask)
!bw
return                                               
end  


subroutine stress_invariants(stress,I1,J2)                 
use truth                                                                                         
implicit none 
integer i   
real(kind=double) I1,J2,mean,stress(6)                                              
I1=stress(1)+stress(2)+stress(3)
mean=I1/3.0d0 
J2=0.0d0
do i=1,3
   J2=J2+(stress(i)-mean)*(stress(i)-mean)
end do
do i=4,6
    J2=J2+2.0d0*stress(i)*stress(i)
end do
J2=J2/2.0d0
return
end 


subroutine J2_deriv(stress,dJ2dsig,ddJ2ddsig)                 
use truth                                                                                         
implicit none 
integer i,j   
real(kind=double) tmp,stress(6),dev_stress(6),&
dJ2dsig(6),ddJ2ddsig(6,6),dsdsig(6,6),dJ2ds(6)                                            
tmp=stress(1)+stress(2)+stress(3)
tmp=tmp/3.0d0 
dev_stress=0.0d0
dev_stress=stress
dev_stress(1:3)=dev_stress(1:3)-tmp
dJ2dsig=dev_stress
dJ2dsig(4:6)=2.0d0*dJ2dsig(4:6)
ddJ2ddsig=0.0d0
ddJ2ddsig(1:3,1:3)=-1.0d0/3.0d0
ddJ2ddsig(1,1)=2.0d0/3.0d0
ddJ2ddsig(2,2)=2.0d0/3.0d0
ddJ2ddsig(3,3)=2.0d0/3.0d0
ddJ2ddsig(4,4)=2.0d0
ddJ2ddsig(5,5)=2.0d0
ddJ2ddsig(6,6)=2.0d0
return
end

      SUBROUTINE MATINV (A,LDA,N,IFLAG) 
!
!-----------------------------------------------------------------------
!   MATINV   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
!            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
!            MARYLAND  20899
!
!   FOR: COMPUTING THE INVERSE OF A GENERAL N BY N MATRIX IN PLACE,
!        I.E., THE INVERSE OVERWRITES THE ORIGINAL MATRIX.  THE STEPS 
!        OF THE ALGORITHM ARE DESCRIBED BELOW AS THEY OCCUR.  ROW
!        INTERCHANGES ARE DONE AS NEEDED IN ORDER TO INCREASE THE
!        ACCURACY OF THE INVERSE MATRIX.  WITHOUT INTERCHANGES THIS
!        ALGORITHM WILL FAIL WHEN ANY OF THE LEADING PRINCIPAL
!        SUBMATRICES ARE SINGULAR OR WHEN THE MATRIX ITSELF IS
!        SINGULAR.  WITH INTERCHANGES THIS ALGORITHM WILL FAIL ONLY
!        WHEN THE MATRIX ITSELF IS SINGULAR.  THE LEADING PRINCIPAL
!
!                                   [A B C]
!        SUBMATRICES OF THE MATRIX  [D E F]  ARE  [A]  AND  [A B] .
!                                   [G H I]                 [D E]
!
!   SUBPROGRAMS CALLED: -NONE-
!
!   CURRENT VERSION COMPLETED JANUARY 15, 1987
!
!   REFERENCE: STEWART, G.W., 'INTRODUCTION TO MATRIX COMPUTATIONS',
!@              ACADEMIC PRESS, INC., 1973
!----------------------------------------------------------------------
!   DEFINITION OF PASSED PARAMETERS
!
!     * A = MATRIX (SIZE NXN) TO BE INVERTED (REAL)
!
!   * LDA = LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
!
!     * N = NUMBER OF ROWS AND COLUMNS OF MATRIX A (INTEGER)
!
!   IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
!           -2 -> TOO MANY ROW INTERCHANGES NEEDED - INCREASE MX
!           -1 -> N>LDA
!            0 -> NO ERRORS DETECTED
!            K -> MATRIX A FOUND TO BE SINGULAR AT THE KTH STEP OF
!                 THE CROUT REDUCTION (1<=K<=N)
!
!   * INDICATES PARAMETERS REQUIRING INPUT VALUES 
!-----------------------------------------------------------------------
!
use truth                                                                                         
implicit none 
integer IFLAG,N,NEX,K,I,L,J,MX,LDA
real(kind=double) A(LDA,LDA),Q,S,R   
      PARAMETER (MX=100)
  integer IEX(MX,2)
      IFLAG = 0
!
!--- CHECK CONSISTENCY OF PASSED PARAMETERS
!
      IF (N.GT.LDA) THEN
         IFLAG = -1 
         RETURN
      ENDIF
!
!--- COMPUTE A = LU BY THE CROUT REDUCTION WHERE L IS LOWER TRIANGULAR
!--- AND U IS UNIT UPPER TRIANGULAR (ALGORITHM 3.4, P. 138 OF THE
!--- REFERENCE)
!
      NEX = 0
      DO K = 1, N
         DO I = K, N
            S = A(I,K)
            DO L = 1, K-1
               S = S-A(I,L)*A(L,K)
            end do
            A(I,K) = S
         end do
!
!--- INTERCHANGE ROWS IF NECESSARY
!
         Q = 0.0
         L = 0
         DO I = K, N
            R = ABS(A(I,K))
            IF (R.GT.Q) THEN
               Q = R
               L = I
            ENDIF
        end do
         IF (L.EQ.0) THEN
            IFLAG = K
            RETURN
         ENDIF
         IF (L.NE.K) THEN
            NEX = NEX+1
            IF (NEX.GT.MX) THEN
               IFLAG = -2
               RETURN
            ENDIF
            IEX(NEX,1) = K
            IEX(NEX,2) = L
            DO J = 1, N
               Q = A(K,J)
               A(K,J) = A(L,J)
               A(L,J) = Q
            end do
         ENDIF
!
!--- END ROW INTERCHANGE SECTION
!
         DO J = K+1, N
            S = A(K,J)
            DO L = 1, K-1
               S = S-A(K,L)*A(L,J)
            end do
            A(K,J) = S/A(K,K) 
        end do
   end do
!
!--- INVERT THE LOWER TRIANGLE L IN PLACE (SIMILAR TO ALGORITHM 1.5,
!--- P. 110 OF THE REFERENCE) 
!
      DO K = N, 1, -1
         A(K,K) = 1.0/A(K,K)
         DO  I = K-1, 1, -1 
            S = 0.0 
            DO J = I+1, K
               S = S+A(J,I)*A(K,J)
           end do
            A(K,I) = -S/A(I,I)
           end do
      end do
!
!--- INVERT THE UPPER TRIANGLE U IN PLACE (ALGORITHM 1.5, P. 110 OF
!--- THE REFERENCE) 
!
      DO K = N, 1, -1
         DO I = K-1, 1, -1
            S = A(I,K)
            DO J = I+1, K-1
               S = S+A(I,J)*A(J,K)
            end do
            A(I,K) = -S
        end do
     end do
!
!--- COMPUTE INV(A) = INV(U)*INV(L)
!
      DO I = 1, N
         DO J = 1, N
            IF (J.GT.I) THEN
               S = 0.0
               L = J
            ELSE
               S = A(I,J)
               L = I+1
            ENDIF
            DO K = L, N
               S = S+A(I,K)*A(K,J)
            end do
            A(I,J) = S
         end do
      end do
!
!--- INTERCHANGE COLUMNS OF INV(A) TO REVERSE EFFECT OF ROW 
!--- INTERCHANGES OF A
!
      DO I = NEX, 1, -1
         K = IEX(I,1)
         L = IEX(I,2)
         DO J = 1, N
            Q = A(J,K)
            A(J,K) = A(J,L)
            A(J,L) = Q
          end do
       end do
      RETURN
      END 

 subroutine inverse(amatx,pivin,iposn,nsize)                 
use truth                                                                                         
implicit none 
integer i,nsize,ik,it,ix,ix1,ix2,j,jx,k,l,lpivt,m,iposn(nsize)   
real(kind=double) tmp,amatx(nsize*nsize),pivin(nsize),pivot                                        
do i=1,nsize
   iposn(i)=i
end do
do k=1,nsize
   m=nsize+1-k
   pivot=0.0d0
   do i=1,m
      if(abs(pivot).gt.abs(amatx(i))) cycle
      pivot=amatx(i)
      lpivt=i
   end do
   ik=lpivt+k-1
   it=iposn(k)
   iposn(k)=iposn(ik)
   iposn(ik)=it
   do j=2,nsize
      ix=nsize*(j-1)+lpivt
      pivin(j-1)=amatx(ix)/pivot
   end do
   pivin(nsize)=1.0d0/pivot
   do j=1,nsize
      ix=nsize*(j-1)+lpivt
      jx=nsize*(j-1)+1
      amatx(ix)=amatx(jx)
   end do
   do i=2,nsize
      do j=2,nsize
         ix1=nsize*(j-2)+i-1
         ix2=nsize*(j-1)+i
         amatx(ix1)=amatx(ix2)-amatx(i)*pivin(j-1)
      end do
      ix=nsize*(nsize-1)+i-1
      amatx(ix)=-amatx(i)/pivot
   enddo
   do j=1,nsize
      ix=nsize*(j-1)+nsize
      amatx(ix)=pivin(j)
   enddo
 end do
   do l=1,nsize
      do j=l,nsize
         if(iposn(j).ne.l) cycle
            iposn(j)=iposn(l)
         do i=1,nsize
            ix1=nsize*(l-1)+i
            ix2=nsize*(j-1)+i
            pivin(i)=amatx(ix1)
            amatx(ix1)=amatx(ix2)
            amatx(ix2)=pivin(i)
         end do
         exit
      end do
   end do
   return
   end
  

subroutine L2_norm(mresidue,n_residue,L2)
use truth
integer n_residue
real(kind=double) mresidue(n_residue),L2
L2=0.0d0
do i=1,n_residue
   L2=L2+mresidue(i)*mresidue(i)
end do
L2=sqrt(L2) 
return
end

subroutine eqv_strain(delta_strain_ep,delta_eqv)
use truth
integer i
real(kind=double) delta_strain_ep(6),delta_eqv
delta_eqv=0.0d0
do i=1,3
   delta_eqv=delta_eqv+delta_strain_ep(i)*delta_strain_ep(i)
end do
do i=4,6
   delta_eqv=delta_eqv+delta_strain_ep(i)*delta_strain_ep(i)/2.0d0
end do
delta_eqv=sqrt(2.0d0*delta_eqv/3.0d0)
return
end

subroutine eqv_svect(delta_u_jump,delta_equ,n_u)
use truth
integer i
real(kind=double) delta_u_jump(3),delta_equ,n_u(3)
delta_equ=0.0d0
n_u=0.0d0
do i=1,3
   delta_equ=delta_equ+delta_u_jump(i)*delta_u_jump(i)
end do
delta_equ=sqrt(2.0d0*delta_equ/3.0d0)
n_u=2.0d0/3.0d0*delta_u_jump/delta_equ
return
end

subroutine L2_norm_leve_u(L2)
use truth
use system
real(kind=double) L2,a
L2=0.0d0
do i=1,dof_tl
   L2=L2+tl_load(i)*tl_load(i)
end do
a=0.0d0
do i=1,dof_tl
   a=a+delta_u(i)*delta_u(i)
end do
if(a.lt.1.0e-20) a=1
L2=sqrt(L2/a) 
return
end

subroutine L2_norm_leve_1(L2)
use truth
use system
real(kind=double) L2
L2=0.0d0
do i=1,dof_tl
   L2=L2+tl_load(i)*tl_load(i)
end do
L2=sqrt(L2) 
return
end

subroutine  master_node
use truth
use model_type
use system
implicit none
!bw integer ielem,inode,i,j,loct,st_dof,ncase,pnode_c_id(node_tl,8) 
integer ielem,inode,i,j,loct,st_dof,ncase, ierr
integer, allocatable :: pnode_c_id(:,:), node_cover(:)
integer type,na,ncount
integer n1,n2,n3,n4,n5,n6,n7,n8,master,iface !bw node_cover(node_tl)
!bw real(kind=double) distpnode(node_tl,3)
real(kind=double),allocatable ::distpnode(:,:),pnode_dis(:,:)
!bw real(kind=double) pnode_dis(node_tl,3),x,y,z
real(kind=double) x,y,z
allocate(pnode_c_id(node_tl,8),distpnode(node_tl,3),node_cover(node_tl),&
         pnode_dis(node_tl,3),STAT=ierr)
if (ierr .ne. 0) then
   write(*,*) "ERROR: in MASTER_NODE(), insufficient memory"
   stop 462
endif
pnode=one
pnode_cn(1)=one
pnode_c_id(pnode,1)=one  
cnode_p(1)=pnode
distpnode(pnode,1:3)=coord(1,1:3) 
do inode=2, node_tl
   ncount=zero
   do i=1,pnode
      x=abs(coord(inode,1)-distpnode(i,1))
      y=abs(coord(inode,2)-distpnode(i,2))
      z=abs(coord(inode,3)-distpnode(i,3))
      if(x<1.0e-3.and.y<1.0e-3.and.z<1.0e-3) then
         cnode_p(inode)=i
         pnode_cn(i)=pnode_cn(i)+one
         pnode_c_id(i,pnode_cn(i))=inode 
         cycle
      else
         ncount=ncount+1  
      end if
   end do
      if(ncount==pnode) then
        pnode=pnode+one  !bw pnode, total number of nodes with different coord
        pnode_cn(pnode)=one   !bw  pnode_cn, number of nodes sharing the coord
        pnode_c_id(pnode,pnode_cn(pnode))=inode 
        distpnode(pnode,1:3)=coord(inode,1:3) 
        cnode_p(inode)=pnode !bw cnode_p, pointer, node -> condensed node number
      end if
end do
deallocate(pnode_c_id,distpnode,pnode_dis,node_cover)
return
end 


subroutine  average_delta_u
use truth
use system
use model_type
implicit none
integer ielem,inode,i,j,loct,st_dof,ncase
integer type,na,ncount
integer n1,n2,n3,n4,n5,n6,n7,n8,master,iface
real(kind=double) pnode_dis(node_tl,3)
pnode_dis=zero_d
do ielem=1,ele_tl
    do inode=1,8
       n1=hexa(ielem)%node(inode)
       n2=cnode_p(n1)
pnode_dis(n2,:)=pnode_dis(n2,:)+tl_load(3*(n1-1)+1:3*n1)
    end do
end do
do inode=1,pnode
   pnode_dis(inode,:)=pnode_dis(inode,:)/pnode_cn(inode)
end do
do n1=1,node_tl
tl_load(3*(n1-1)+1:3*n1)=pnode_dis(cnode_p(n1),:)
end do
return
end

subroutine  average_residue
use truth
use system
use model_type
implicit none
integer ielem,inode,i,j,loct,st_dof,ncase
integer type,na,ncount,jnode
integer n1,n2,n3,n4,n5,n6,n7,n8,master,iface
real(kind=double) pnode_dis(node_tl,3)
pnode_dis=zero_d
do inode=1,cg_tl_node
   j=cg_node(inode,9)
   do i=1,j
   jnode=cg_node(inode,i)
   pnode_dis(inode,:)=pnode_dis(inode,:)+tl_load(3*(jnode-1)+1:3*jnode)
   end do
   pnode_dis(inode,:)=pnode_dis(inode,:)/real(j)
end do
do inode=1,cg_tl_node
   j=cg_node(inode,9)
   do i=1,j
   jnode=cg_node(inode,i)
   tl_load(3*(jnode-1)+1:3*jnode)=pnode_dis(inode,:)
   end do
end do
return
end

subroutine  well_geometry(RI,RO,H,R1,R2,n1,n2,n3,n_theta,n_z,p0,p1,p2)
use truth
implicit none
integer i,j,k,kkk,ielem,inode,n1,n2,n3,n_theta,n_z,&
        n_h_tl,n_line_r_1,n_line_r_2,n_line_r_3,n_line_theta,n_line_z,&
        n_sheet_v,n_sheet_nelem,node_tl_n,node_start,node_end,nelem,&
        node_n_d,node_n_f,node_n_force
real(kind=double) RI,RO,H,R1,R2,p0,p1,p2,x,y,r,r11,r22,A,p,theta
integer, allocatable:: node_id(:),node_bc_id(:),node_bc_dof(:),node_f_id(:),&
                       node_force_id(:),node_force_dof(:),elem(:,:)
real(kind=double), allocatable:: node_coord(:,:),node_bc_dis(:),&
                                node_f_force(:),node_force_force(:)
allocate(node_bc_id((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_bc_dof((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_f_id((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_force_id((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_force_dof((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(elem((n1+n2+n3)*(n_theta)*(n_z),8))
allocate(node_bc_dis((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_f_force((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_coord((n1+n2+n3+1)*(n_theta+1)*(n_z+1),3))
allocate(node_id((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
allocate(node_force_force((n1+n2+n3+1)*(n_theta+1)*(n_z+1)))
n_h_tl=n1+n2+n3
n_line_r_1=n1+1
n_line_r_2=n1+n2+1
n_line_r_3=n1+n2+n3+1
n_line_theta=n_theta+1
n_line_z=n_z+1
n_sheet_v=n_line_theta*n_line_z
n_sheet_nelem=n_theta*n_z
node_tl_n=0
do i=1,n_line_r_3
   do j=1,n_line_z
      do k=1,n_line_theta
         node_tl_n=node_tl_n+1
         node_coord(node_tl_n,3)=(j-1)*H/n_z
         if(i<=n_line_r_2) then
            theta=(k-1)*3.1415926/2.0d0/n_theta
            if(i<=n_line_r_1) then
               x=(RI+(i-1)*(R1-RI)/n1)*cos(theta)
               y=(RI+(i-1)*(R1-RI)/n1)*sin(theta)
            else
               x=(R1+(i-n_line_r_1)*(R2-R1)/n2)*cos(theta)
               y=(R1+(i-n_line_r_1)*(R2-R1)/n2)*sin(theta)
            end if
        else
            node_start=(n_line_r_2-1)*n_sheet_v+k
            node_end=(n_line_r_3-1)*n_sheet_v+k
            if(k<=n_theta/2+1) then
               x=node_coord(node_start,1)+(RO-node_coord(node_start,1))*(i-n_line_r_2)/n3
               y=node_coord(node_start,2)+(RO*(k-1)/(n_theta/2)-node_coord(node_start,2))*(i-n_line_r_2)/n3
            else
               x=node_coord(node_start,1)+(RO*(n_theta-k+1)/(n_theta/2)-node_coord(node_start,1))*(i-n_line_r_2)/n3
               y=node_coord(node_start,2)+(RO-node_coord(node_start,2))*(i-n_line_r_2)/n3
            end if
        end if
        node_coord(node_tl_n,1)=x
        node_coord(node_tl_n,2)=y
     end do
  end do
end do
nelem=1
elem(1,1)=1
elem(1,2)=n_sheet_v+1 
elem(1,3)=elem(1,2)+1
elem(1,4)=elem(1,1)+1
do kkk=1,4
   elem(1,kkk+4)=elem(1,kkk)+n_line_theta
end do
do i=2,n_theta
   nelem= nelem+1
   elem(i,1:8)=elem(i-1,1:8)+1
end do
do j=2,n_z
   do i=1,n_theta
      nelem=nelem+1
      elem(nelem,1:8)=elem(nelem-n_theta,1:8)+n_line_theta
   end do
end do        
do i=2,n_h_tl
   do j=1,n_z
      do k=1,n_theta
         nelem=nelem+1
         elem(nelem,1:8)=elem(nelem-n_sheet_nelem,1:8)+n_sheet_v
      end do
   end do
end do 
!
!B.C.condition
!
node_n_d=0
do i=1,node_tl_n
   if(abs(node_coord(i,1))<1.0e-5) then
      node_n_d=node_n_d+1
      node_bc_id(node_n_d)=i
      node_bc_dof(node_n_d)=1
      node_bc_dis(node_n_d)=0.0d0
   end if
end do
do i=1,node_tl_n
   if(abs(node_coord(i,2))<1.0e-5) then
      node_n_d=node_n_d+1
      node_bc_id(node_n_d)=i
      node_bc_dof(node_n_d)=2
      node_bc_dis(node_n_d)=0.0d0
   end if
end do
do i=1,node_tl_n
   if(abs(node_coord(i,3))<1.0e-5) then
      node_n_d=node_n_d+1
      node_bc_id(node_n_d)=i
      node_bc_dof(node_n_d)=3
      node_bc_dis(node_n_d)=0.0d0
   end if
end do
A=3.1415926*RI*H/n_sheet_nelem/2.0d0
p=p0*A/4.0
node_n_f=0
do i=1,node_tl_n
   r=node_coord(i,1)*node_coord(i,1)
   r=r+node_coord(i,2)*node_coord(i,2)
   r=sqrt(r)
   if(abs(r-RI)<1.0e-5) then
      !inner well wall surface
      node_n_f=node_n_f+1
      node_f_id(node_n_f)=i
      node_f_force(node_n_f)=4.0d0*p
   end if
end do
do i=1,node_n_f
   if(abs(node_coord(node_f_id(i),1))<1.0e-5.or.& 
      abs(node_coord(node_f_id(i),2))<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3))<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=2.0d0*p
  if(abs(node_coord(node_f_id(i),1))<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),1))<1.0e-5.and.& 
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),2))<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),2))<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
end do
node_n_force=0
do i=1,node_n_f
   r11=node_coord(i,1)/RI
   r22=node_coord(i,2)/RI
   node_n_force=node_n_force+1
   node_force_id(node_n_force)=node_f_id(i)
   node_force_dof(node_n_force)=1
   node_force_force(node_n_force)=r11*node_f_force(i)
   node_n_force=node_n_force+1
   node_force_id(node_n_force)=node_f_id(i)
   node_force_dof(node_n_force)=2
   node_force_force(node_n_force)=r22*node_f_force(i)
end do
A=RO*H/n_sheet_nelem*2.0d0
p=p1*A/4.0
node_n_f=0
do i=1,node_tl_n
   if(abs(node_coord(i,1)-RO)<1.0e-5) then
      !outer surface in x-direction
      node_n_f=node_n_f+1
      node_f_id(node_n_f)=i
      node_f_force(node_n_f)=4.0d0*p
   end if
end do
do i=1,node_n_f
   if(abs(node_coord(node_f_id(i),2))<1.0e-5.or.& 
      abs(node_coord(node_f_id(i),2)-RO)<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3))<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=2.0d0*p
  if(abs(node_coord(node_f_id(i),2))<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),2))<1.0e-5.and.& 
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),2)-RO)<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),2)-RO)<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
end do
do i=1,node_n_f
   node_n_force=node_n_force+1
   node_force_id(node_n_force)=node_f_id(i)
   node_force_dof(node_n_force)=1
   node_force_force(node_n_force)=-node_f_force(i)
end do
A=RO*H/n_sheet_nelem*2.0d0
p=p2*A/4.0
node_n_f=0
do i=1,node_tl_n
   if(abs(node_coord(i,2)-RO)<1.0e-5) then
      !outer surface in y-direction
      node_n_f=node_n_f+1
      node_f_id(node_n_f)=i
      node_f_force(node_n_f)=4.0d0*p
   end if
end do
do i=1,node_n_f
   if(abs(node_coord(node_f_id(i),1))<1.0e-5.or.& 
      abs(node_coord(node_f_id(i),1)-RO)<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3))<1.0e-5.or.&
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=2.0d0*p
  if(abs(node_coord(node_f_id(i),1))<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),1))<1.0e-5.and.& 
      abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),1)-RO)<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3))<1.0e-5) node_f_force(i)=p
  if(abs(node_coord(node_f_id(i),1)-RO)<1.0e-5.and.& 
     abs(node_coord(node_f_id(i),3)-H)<1.0e-5) node_f_force(i)=p
end do
do i=1,node_n_f
   node_n_force=node_n_force+1
   node_force_id(node_n_force)=node_f_id(i)
   node_force_dof(node_n_force)=2
   node_force_force(node_n_force)=-node_f_force(i)
end do
write(900,30) node_tl_n
do i=1,node_tl_n
   write(900,30) i,node_coord(i,1),node_coord(i,2),node_coord(i,3)
30 format(i10,5x,F15.8,5x,F15.8,5x,F15.8)
end do
write(900,30) nelem
do i=1,nelem
   n1=1
   n2=1
   write(900,40) i,n1,n2
40 format(i10,5x,i5,5x,i5)
end do
do i=1,nelem
   write(900,50) i,elem(i,1),elem(i,2),elem(i,3),elem(i,4),&
                   elem(i,5),elem(i,6),elem(i,7),elem(i,8)
50 format(i8,5x,8i8)
end do
write(900,*) node_n_d
do i=1,node_n_d
   write(900,60) node_bc_id(i),node_bc_dof(i),node_bc_dis(i)
60 format(i8,5x,i8,5x,F15.8)
end do
p=1.0
write(900,*) node_n_force,p
do i=1,node_n_force
   write(900,70) node_force_id(i),node_force_dof(i),node_force_force(i)
70 format(i8,5x,i8,5x,F15.8)
end do
deallocate(node_force_force,node_id,node_coord,node_f_force,node_bc_dis,elem,&
           node_force_dof,node_force_id,node_f_id,&
           node_bc_dof,node_bc_id)
return
end

subroutine  get_strain(ielem,igauss,strain)
use truth
use model_type
use system
implicit none
integer ielem,inode,idof,i,j,igauss,node_id
real(kind=double) node_c(ele_node,3),r,s,t
real(kind=double) cartd(ele_node,3)
real(kind=double) bmatx(b_dof,ele_dof),N_matx(node_dof,ele_dof)
real(kind=double) dvolu,node_dis(ele_dof),strain(b_dof)
ele_type=hexa(ielem)%ele_type
if(ele_type==1) then
   r=gauss_coord_8(igauss,1)
   s=gauss_coord_8(igauss,2)
   t=gauss_coord_8(igauss,3)
else
   r=gauss_coord_27(igauss,1)
   s=gauss_coord_27(igauss,2)
   t=gauss_coord_27(igauss,3)
end if
call hexanb0(ielem,r,s,t,bmatx,dvolu,N_matx)
do inode=1,8
   node_id=hexa(ielem)%node(inode)
   node_dis(3*(inode-1)+1:3*inode)=u(3*(node_id-1)+1:3*node_id)
end do
strain=0.0d0
do i=1,b_dof
    do j=1,ele_dof
       strain(i)=strain(i)+bmatx(i,j)*node_dis(j)
    end do
end do
return
end


subroutine  get_vstrain_at_element_centor
use truth
use model_type
use system
implicit none
integer ielem,inode
real(kind=double) r,s,t
real(kind=double) shap(8),deriv(8,3),node_vstrain(8)
do ielem=1,ele_tl
   r=0.0d0
   s=0.0d0
   t=0.0d0
   call shap_g(r,s,t,shap,deriv)
   do inode=1,8
      node_vstrain(inode)=hexa(ielem)%gpt_strn(inode,1)+&
                          hexa(ielem)%gpt_strn(inode,2)+&
                          hexa(ielem)%gpt_strn(inode,3)
   end do
   hexa(ielem)%vstrain=0.0d0
   do inode=1,8
      hexa(ielem)%vstrain=hexa(ielem)%vstrain+shap(inode)*node_vstrain(inode)
   end do
end do
return
end


subroutine  get_stress(ielem,igauss,stress)
use truth
use model_type
use system
implicit none
integer ielem,inode,idof,i,j,mat_id,igauss
real(kind=double) node_c(ele_node,3),r,s,t
real(kind=double) cartd(ele_node,3)
real(kind=double) bmatx(b_dof,ele_dof),N_matx(node_dof,ele_dof)
real(kind=double) dvolu,node_dis(ele_dof),stress(b_dof),strain(b_dof),D_ep(b_dof,b_dof)
call get_strain(ielem,igauss,strain)
call mat_solver(ielem,igauss,D_ep) 
stress=0.0d0 
do i=1,b_dof
    do j=1,b_dof
       stress(i)=stress(i)+D_ep(i,j)*strain(j)
    end do
end do
return
end

!subroutine L2_norm_leve_1(L2)
!use truth
!use system
!implicit none 
!integer i,j
!real(kind=double) L2
!L2=0.0d0
!do i=1,dof_tl
!   L2=L2+tl_load(i)*tl_load(i)
!end do
!!write(700,*) 'tl_load=',tl_load
!L2=sqrt(L2) 
!return
!end

subroutine updating_after_coupled_field_converge
!
!This is done after the convergency of Netwon-Raphson iterations,
!which means this time step is finished
!(a)------updating total displacement u for this time step: u=u_n+delta_u 
!(b)------updating u_n by u: u_n=u
!(c)------updating stress at t by current stress
!(d)------updating other state varaibles at t by current values
!
use truth
use model_type
use system
implicit none
integer ielem,inode,NNTIM
u_n=u
! bag8 - do not perturb coordinates
!do inode=1,node_tl
!   coord_n(inode,1:3)=coord_n(inode,1:3)+&
!   delta_u(3*(inode-1)+1:3*inode)
!end do
delta_u=0.0d0
do ielem=1,ele_tl
   HEXA(ielem)%gpt_strss_t=hexa(ielem)%gpt_strss
   hexa(ielem)%gpt_strn_t=hexa(ielem)%gpt_strn
   hexa(ielem)%gpt_estrn_t=hexa(ielem)%gpt_estrn
   hexa(ielem)%gpt_pstrn_t=hexa(ielem)%gpt_pstrn
   hexa(ielem)%gpt_vstate_t=hexa(ielem)%gpt_vstate
end do
return
end





