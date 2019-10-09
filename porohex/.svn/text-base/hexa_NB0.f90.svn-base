subroutine  hexanb0(ielem,r,s,t,bmatx,dvolu,N_matx)
use truth
use model_type
use system
implicit none
integer ielem,inode,idof
real(kind=double) node_c(ele_node,3),r,s,t
real(kind=double) cartd(ele_node,3)
real(kind=double) bmatx(b_dof,ele_dof),N_matx(node_dof,ele_dof)
real(kind=double), allocatable:: shap(:),deriv(:,:)
real(kind=double) dvolu
allocate(shap(ele_node))
allocate(deriv(ele_node,3))
call projection(one,ielem,node_c)
call shap_g(r,s,t,shap,deriv) 
call jacob3d(node_c,deriv,dvolu,cartd)
bmatx=0.0d0
call hexa_bmat(cartd,bmatx)
N_matx=0.0d0
do idof=1,node_dof
   do inode=1,ele_node
      N_matx(idof,(inode-1)*node_dof+idof)=shap(inode)
   end do
end do
deallocate(deriv,shap)
return
end

subroutine hexa_bmat(cartd,bmatx)
use truth
use model_type
use system
implicit none
integer inode,mgash
real(kind=double) bmatx(b_dof,ele_dof),cartd(ele_node,3)
bmatx=zero_d
mgash=one
if(b_dof==6) then
do inode=1,ele_node
   bmatx(1,mgash)=cartd(inode,1)
   bmatx(2,mgash+1)=cartd(inode,2)
   bmatx(3,mgash+2)=cartd(inode,3)
   bmatx(4,mgash)=cartd(inode,2)
   bmatx(4,mgash+1)=cartd(inode,1)
   bmatx(5,mgash+1)=cartd(inode,3)
   bmatx(5,mgash+2)=cartd(inode,2)
   bmatx(6,mgash)=cartd(inode,3)
   bmatx(6,mgash+2)=cartd(inode,1)
   mgash=mgash+3
end do
else
do inode=1,ele_node
   bmatx(1,inode)=cartd(inode,1)
   bmatx(2,inode)=cartd(inode,2)
   bmatx(3,inode)=cartd(inode,3)
end do
end if
return
end

subroutine shap_g(r,s,t,shap,deriv)
use truth
use model_type
use system
implicit none
integer rule(20,3),i,j
real(kind=double) r,s,t 
real(kind=double) shap(ele_node),deriv(ele_node,3)
real(kind=double) gbase(ele_node),dgbase(ele_node,3)
shap=zero_d
deriv=zero_d
gbase=zero_d
dgbase=zero_d   !bw derivative of shape functions
gbase(1)=(one_d+r)*(one_d+s)*(one_d-t)/8.0_double
gbase(2)=(one_d-r)*(one_d+s)*(one_d-t)/8.0_double
gbase(3)=(one_d-r)*(one_d-s)*(one_d-t)/8.0_double
gbase(4)=(one_d+r)*(one_d-s)*(one_d-t)/8.0_double
gbase(5)=(one_d+r)*(one_d+s)*(one_d+t)/8.0_double
gbase(6)=(one_d-r)*(one_d+s)*(one_d+t)/8.0_double
gbase(7)=(one_d-r)*(one_d-s)*(one_d+t)/8.0_double
gbase(8)=(one_d+r)*(one_d-s)*(one_d+t)/8.0_double
dgbase(1,1)=(one_d+s)*(one_d-t)/8.0_double
dgbase(1,2)=(one_d+r)*(one_d-t)/8.0_double
dgbase(1,3)=-(one_d+r)*(one_d+s)/8.0_double
dgbase(2,1)=-(one_d+s)*(one_d-t)/8.0_double
dgbase(2,2)=(one_d-r)*(one_d-t)/8.0_double
dgbase(2,3)=-(one_d-r)*(one_d+s)/8.0_double
dgbase(3,1)=-(one_d-s)*(one_d-t)/8.0_double
dgbase(3,2)=-(one_d-r)*(one_d-t)/8.0_double
dgbase(3,3)=-(one_d-r)*(one_d-s)/8.0_double
dgbase(4,1)=(one_d-s)*(one_d-t)/8.0_double
dgbase(4,2)=-(one_d+r)*(one_d-t)/8.0_double
dgbase(4,3)=-(one_d+r)*(one_d-s)/8.0_double
dgbase(5,1)=(one_d+s)*(one_d+t)/8.0_double
dgbase(5,2)=(one_d+r)*(one_d+t)/8.0_double
dgbase(5,3)=(one_d+r)*(one_d+s)/8.0_double
dgbase(6,1)=-(one_d+s)*(one_d+t)/8.0_double
dgbase(6,2)=(one_d-r)*(one_d+t)/8.0_double
dgbase(6,3)=(one_d-r)*(one_d+s)/8.0_double
dgbase(7,1)=-(one_d-s)*(one_d+t)/8.0_double
dgbase(7,2)=-(one_d-r)*(one_d+t)/8.0_double
dgbase(7,3)=(one_d-r)*(one_d-s)/8.0_double
dgbase(8,1)=(one_d-s)*(one_d+t)/8.0_double
dgbase(8,2)=-(one_d+r)*(one_d+t)/8.0_double
dgbase(8,3)=(one_d+r)*(one_d-s)/8.0_double
if(ele_node.gt.8) then
gbase(9)=(one_d-r*r)*(one_d+s)*(one_d-t)/4.0_double
gbase(10)=(one_d-s*s)*(one_d-r)*(one_d-t)/4.0_double
gbase(11)=(one_d-r*r)*(one_d-s)*(one_d-t)/4.0_double
gbase(12)=(one_d-s*s)*(one_d+r)*(one_d-t)/4.0_double
gbase(13)=(one_d-r*r)*(one_d+s)*(one_d+t)/4.0_double
gbase(14)=(one_d-s*s)*(one_d-r)*(one_d+t)/4.0_double
gbase(15)=(one_d-r*r)*(one_d-s)*(one_d+t)/4.0_double
gbase(16)=(one_d-s*s)*(one_d+r)*(one_d+t)/4.0_double
gbase(17)=(one_d-t*t)*(one_d+r)*(one_d+s)/4.0_double
gbase(18)=(one_d-t*t)*(one_d-r)*(one_d+s)/4.0_double
gbase(19)=(one_d-t*t)*(one_d-r)*(one_d-s)/4.0_double
gbase(20)=(one_d-t*t)*(one_d+r)*(one_d-s)/4.0_double
dgbase(9,1)=-r*(one_d+s)*(one_d-t)/2.0_double
dgbase(9,2)=(one_d-r*r)*(one_d-t)/4.0_double
dgbase(9,3)=-(one_d-r*r)*(one_d+s)/4.0_double
dgbase(10,1)=-(one_d-s*s)*(one_d-t)/4.0_double
dgbase(10,2)=-s*(one_d-r)*(one_d-t)/2.0_double
dgbase(10,3)=-(one_d-s*s)*(one_d-r)/4.0_double
dgbase(11,1)=-r*(one_d-s)*(one_d-t)/2.0_double
dgbase(11,2)=-(one_d-r*r)*(one_d-t)/4.0_double
dgbase(11,3)=-(one_d-r*r)*(one_d-s)/4.0_double
dgbase(12,1)=(one_d-s*s)*(one_d-t)/4.0_double
dgbase(12,2)=-s*(one_d+r)*(one_d-t)/2.0_double
dgbase(12,3)=-(one_d-s*s)*(one_d+r)/4.0_double
dgbase(13,1)=-r*(one_d+s)*(one_d+t)/2.0_double
dgbase(13,2)=(one_d-r*r)*(one_d+t)/4.0_double
dgbase(13,3)=(one_d-r*r)*(one_d+s)/4.0_double
dgbase(14,1)=-(one_d-s*s)*(one_d+t)/4.0_double
dgbase(14,2)=-s*(one_d-r)*(one_d+t)/2.0_double
dgbase(14,3)=(one_d-s*s)*(one_d-r)/4.0_double
dgbase(15,1)=-r*(one_d-s)*(one_d+t)/2.0_double
dgbase(15,2)=-(one_d-r*r)*(one_d+t)/4.0_double
dgbase(15,3)=(one_d-r*r)*(one_d-s)/4.0_double
dgbase(16,1)=(one_d-s*s)*(one_d+t)/4.0_double
dgbase(16,2)=-s*(one_d+r)*(one_d+t)/2.0_double
dgbase(16,3)=(one_d-s*s)*(one_d+r)/4.0_double
dgbase(17,1)=(one_d-t*t)*(one_d+s)/4.0_double
dgbase(17,2)=(one_d-t*t)*(one_d+r)/4.0_double
dgbase(17,3)=-t*(one_d+r)*(one_d+s)/2.0_double
dgbase(18,1)=-(one_d-t*t)*(one_d+s)/4.0_double
dgbase(18,2)=(one_d-t*t)*(one_d-r)/4.0_double
dgbase(18,3)=-t*(one_d-r)*(one_d+s)/2.0_double
dgbase(19,1)=-(one_d-t*t)*(one_d-s)/4.0_double
dgbase(19,2)=-(one_d-t*t)*(one_d-r)/4.0_double
dgbase(19,3)=-t*(one_d-r)*(one_d-s)/2.0_double
dgbase(20,1)=(one_d-t*t)*(one_d-s)/4.0_double
dgbase(20,2)=-(one_d-t*t)*(one_d+r)/4.0_double
dgbase(20,3)=-t*(one_d+r)*(one_d-s)/2.0_double
end if
if(ele_node.gt.20) then
gbase(21)=(one_d-s*s)*(one_d-t*t)*(one_d+r)/2.0_double
gbase(22)=(one_d-r*r)*(one_d-t*t)*(one_d+s)/2.0_double
gbase(23)=(one_d-s*s)*(one_d-t*t)*(one_d-r)/2.0_double
gbase(24)=(one_d-r*r)*(one_d-t*t)*(one_d-s)/2.0_double
gbase(25)=(one_d-r*r)*(one_d-s*s)*(one_d-t)/2.0_double
gbase(26)=(one_d-r*r)*(one_d-s*s)*(one_d+t)/2.0_double
gbase(27)=(one_d-r*r)*(one_d-s*s)*(one_d-t*t)
dgbase(21,1)=(one_d-s*s)*(one_d-t*t)/2.0_double
dgbase(21,2)=-s*(one_d-t*t)*(one_d+r)
dgbase(21,3)=-t*(one_d-s*s)*(one_d+r)
dgbase(22,1)=-r*(one_d-t*t)*(one_d+s)
dgbase(22,2)=(one_d-r*r)*(one_d-t*t)/2.0_double
dgbase(22,3)=-t*(one_d-r*r)*(one_d+s)
dgbase(23,1)=-(one_d-s*s)*(one_d-t*t)/2.0_double
dgbase(23,2)=-s*(one_d-t*t)*(one_d-r)
dgbase(23,3)=-t*(one_d-s*s)*(one_d-r)
dgbase(24,1)=-r*(one_d-t*t)*(one_d-s)
dgbase(24,2)=-(one_d-r*r)*(one_d-t*t)/2.0_double
dgbase(24,3)=-t*(one_d-r*r)*(one_d-s)
dgbase(25,1)=-r*(one_d-s*s)*(one_d-t)
dgbase(25,2)=-s*(one_d-r*r)*(one_d-t)
dgbase(25,3)=-(one_d-r*r)*(one_d-s*s)/2.0_double
dgbase(26,1)=-r*(one_d-s*s)*(one_d+t)
dgbase(26,2)=-s*(one_d-r*r)*(one_d+t)
dgbase(26,3)=(one_d-r*r)*(one_d-s*s)/2.0_double
dgbase(27,1)=-2.0*r*(one_d-s*s)*(one_d-t*t)
dgbase(27,2)=-2.0*s*(one_d-r*r)*(one_d-t*t)
dgbase(27,3)=-2.0*t*(one_d-r*r)*(one_d-s*s)
end if
do i=1,ele_node
   shap(i)=gbase(i)
   deriv(i,1:3)=dgbase(i,1:3)
end do
if(ele_node>8)then
rule(1,1)=9
rule(1,2)=12
rule(1,3)=17
rule(2,1)=9
rule(2,2)=10
rule(2,3)=18
rule(3,1)=10
rule(3,2)=11
rule(3,3)=19
rule(4,1)=11
rule(4,2)=12
rule(4,3)=20
rule(5,1)=13
rule(5,2)=16
rule(5,3)=17
rule(6,1)=13
rule(6,2)=14
rule(6,3)=18
rule(7,1)=14
rule(7,2)=15
rule(7,3)=19
rule(8,1)=15
rule(8,2)=16
rule(8,3)=20
do i=1,8
   do j=1,3
   shap(i)=shap(i)-gbase(rule(i,j))/2.0_double
   deriv(i,1:3)=deriv(i,1:3)-dgbase(rule(i,j),1:3)/2.0_double
   end do
end do
end if
if(ele_node > 20) then
rule(1,1)=21
rule(1,2)=22
rule(1,3)=25
rule(2,1)=22
rule(2,2)=23
rule(2,3)=25
rule(3,1)=23
rule(3,2)=24
rule(3,3)=25
rule(4,1)=21
rule(4,2)=24
rule(4,3)=25
rule(5,1)=21
rule(5,2)=22
rule(5,3)=26
rule(6,1)=22
rule(6,2)=23
rule(6,3)=26
rule(7,1)=23
rule(7,2)=24
rule(7,3)=26
rule(8,1)=21
rule(8,2)=24
rule(8,3)=26
do i=1,8
   do j=1,3
   shap(i)=shap(i)+gbase(rule(i,j))/4.0_double
   deriv(i,1:3)=deriv(i,1:3)+dgbase(rule(i,j),1:3)/4.0_double
   end do
end do
rule(9,1)=22
rule(9,2)=25
rule(10,1)=23
rule(10,2)=25
rule(11,1)=24
rule(11,2)=25
rule(12,1)=21
rule(12,2)=25
rule(13,1)=22
rule(13,2)=26
rule(14,1)=23
rule(14,2)=26
rule(15,1)=24
rule(15,2)=26
rule(16,1)=21
rule(16,2)=26
rule(17,1)=21
rule(17,2)=22
rule(18,1)=22
rule(18,2)=23
rule(19,1)=23
rule(19,2)=24
rule(20,1)=24
rule(20,2)=21
do i=9,20
   do j=1,2
   shap(i)=shap(i)-gbase(rule(i,j))/2.0_double
   deriv(i,1:3)=deriv(i,1:3)-dgbase(rule(i,j),1:3)/2.0_double
   end do
end do
do i=1,8
   shap(i)=shap(i)-1.0_double*gbase(27)/8.0_double
   deriv(i,1:3)=deriv(i,1:3)-1.0_double*dgbase(27,1:3)/8.0_double
end do
do i=9,20
   shap(i)=shap(i)+gbase(27)/4.0_double
   deriv(i,1:3)=deriv(i,1:3)+dgbase(27,1:3)/4.0_double
end do 
do i=21,26
   shap(i)=shap(i)-gbase(27)/2.0_double 
   deriv(i,1:3)=deriv(i,1:3)-dgbase(27,1:3)/2.0_double 
end do  
end if
return
end

subroutine jacob3d(node_c,deriv,dvolu,cartd)
use truth
use model_type
use system
implicit none
integer i,j,m
real(kind=double) xjacm(3,3),cartd(ele_node,3)
real(kind=double) xjacmi(3,3),node_c(ele_node,3)
real(kind=double) djacb,dvolu,deriv(ele_node,3)
xjacm=zero_d   !bw xjacm(i,j)  = dxj/dsi
do i=1,3
   do j=1,3
      do m=1,ele_node
         xjacm(i,j)=xjacm(i,j)+deriv(m,i)*node_c(m,j)
      end do
   end do
end do
        djacb=xjacm(1,1)*xjacm(2,2)*xjacm(3,3)+xjacm(1,2)*xjacm(2,3)*xjacm(3,1)+ &
             xjacm(1,3)*xjacm(2,1)*xjacm(3,2)-xjacm(1,3)*xjacm(2,2)*xjacm(3,1)- &
             xjacm(1,2)*xjacm(2,1)*xjacm(3,3)-xjacm(1,1)*xjacm(2,3)*xjacm(3,2)        
!
!     Determines the inverse of xjacm
!
         xjacmi(1,1)=(xjacm(2,2)*xjacm(3,3)-xjacm(2,3)*xjacm(3,2))
         xjacmi(1,2)=(xjacm(1,3)*xjacm(3,2)-xjacm(1,2)*xjacm(3,3))
         xjacmi(1,3)=(xjacm(1,2)*xjacm(2,3)-xjacm(1,3)*xjacm(2,2))
         xjacmi(2,1)=(xjacm(2,3)*xjacm(3,1)-xjacm(2,1)*xjacm(3,3))
         xjacmi(2,2)=(xjacm(1,1)*xjacm(3,3)-xjacm(1,3)*xjacm(3,1))
         xjacmi(2,3)=(xjacm(1,3)*xjacm(2,1)-xjacm(1,1)*xjacm(2,3))
         xjacmi(3,1)=(xjacm(2,1)*xjacm(3,2)-xjacm(2,2)*xjacm(3,1))
         xjacmi(3,2)=(xjacm(1,2)*xjacm(3,1)-xjacm(1,1)*xjacm(3,2))
         xjacmi(3,3)=(xjacm(1,1)*xjacm(2,2)-xjacm(1,2)*xjacm(2,1))
!xjacmi(1,1)=xjacm(2,2)*xjacm(3,3)-xjacm(3,2)*xjacm(2,3) 
!xjacmi(1,2)=-(xjacm(1,2)*xjacm(3,3)-xjacm(3,2)*xjacm(1,3))
!xjacmi(1,3)=xjacm(1,2)*xjacm(2,3)-xjacm(2,2)*xjacm(1,3)
!xjacmi(2,1)=-(xjacm(2,1)*xjacm(3,3)-xjacm(3,1)*xjacm(2,3))
!xjacmi(2,2)=xjacm(1,1)*xjacm(3,3)-xjacm(3,1)*xjacm(1,3)
!xjacmi(2,3)=-(xjacm(1,1)*xjacm(2,3)-xjacm(2,1)*xjacm(1,3))
!xjacmi(3,1)=xjacm(2,1)*xjacm(3,2)-xjacm(3,1)*xjacm(2,2)
!xjacmi(3,2)=-(xjacm(1,1)*xjacm(3,2)-xjacm(3,1)*xjacm(1,2))
!xjacmi(3,3)=xjacm(1,1)*xjacm(2,2)-xjacm(2,1)*xjacm(1,2)
!djacb=xjacm(1,1)*xjacmi(1,1)+xjacm(2,1)*xjacmi(1,2)+&
!      xjacm(3,1)*xjacmi(1,3)
xjacmi=xjacmi/djacb
dvolu=djacb
do i=1,ele_node   !bw card() is d/dxi
   cartd(i,1:3)=matmul(xjacmi(1:3,1:3),deriv(i,1:3)) 
end do
return
end


subroutine face_normal(ielem,orien,c)                 
use truth                                              
use model_type                                 
use system                                             
implicit none                                          
integer node_1,node_2,node_3,orien,node_all(8)
integer ielem                                          
real(kind=double) a(3),b(3),c(3)                    
node_all(1:8)=hexa(ielem)%node(1:8)                    
if(orien==1) then                                      
   node_1=node_all(1)                                  
   node_2=node_all(5)                                  
   node_3=node_all(4)                                  
endif                                                                                                         
if(orien==2) then                                      
node_1=node_all(2)                                     
node_2=node_all(3)                                     
node_3=node_all(6)                                     
endif                                                  
if(orien==3) then      
node_1=node_all(1)     
node_2=node_all(2)     
node_3=node_all(5)     
endif                  
                       
if(orien==4) then      
node_1=node_all(3)     
node_2=node_all(4)     
node_3=node_all(7)     
endif                  
                       
if(orien==5) then      
node_1=node_all(5)     
node_2=node_all(6)     
node_3=node_all(8)     
endif                  
                       
if(orien==6) then      
node_1=node_all(1)     
node_2=node_all(4)     
node_3=node_all(2)     
endif                  
a(1)=coord(node_2,1)-coord(node_1,1)      
a(2)=coord(node_2,2)-coord(node_1,2)      
a(3)=coord(node_2,3)-coord(node_1,3)      
b(1)=coord(node_3,1)-coord(node_1,1)      
b(2)=coord(node_3,2)-coord(node_1,2)      
b(3)=coord(node_3,3)-coord(node_1,3)      
c(1)=a(2)*b(3)-a(3)*b(2)                  
c(2)=-a(1)*b(3)+a(3)*b(1)                 
c(3)=a(1)*b(2)-a(2)*b(1)                  
a(1)=c(1)*c(1)+c(2)*c(2)+c(3)*c(3)        
a(1)=sqrt(a(1))                           
c(1)=c(1)/a(1)                            
c(2)=c(2)/a(1)                            
c(3)=c(3)/a(1)                            
return               
end 

subroutine face_NB_mat(ielem,&
           orien,igauss,N_mat,B_mat,area)
use truth  
use model_type
use system
implicit none
integer igauss,inode,icolumn,orien,face_id,master
integer temp,j,ielem
real(kind=double) N_mat(node_dof,ele_dof)
real(kind=double) area,dvolu,r,s,t
real(kind=double) B_mat(b_dof,ele_dof)
real(kind=double) shap(ele_node)
real(kind=double) deriv(ele_node,3)
real(kind=double) node_c(ele_node,3)
real(kind=double) cartd(ele_node,3)
real(kind=double) gpcoor(3)              
N_mat=zero_d
B_mat=zero_d
temp=one
call projection(temp,ielem,node_c)
call face_gauss(ielem,orien,igauss,r,s,t)
call shap_g(r,s,t,shap,deriv)
icolumn=one
N_mat=0.0d0
do inode=1,ele_node
   do j=1,node_dof
      N_mat(j,icolumn)=shap(inode)
      icolumn=icolumn+one 
   end do
end do
call jacob_face(orien,node_c,deriv,dvolu,cartd)
area=dvolu
B_mat=0.0d0
call hexa_bmat(cartd,B_mat)
return
end
!
!         Author: Ruijie Liu
!
!
subroutine face_gauss(ielem,orien,igauss,r,s,t)
use truth
use model_type
use system
implicit none
integer i,igauss,face_id,ielem,orien 
real(kind=double) :: pt1=0.577350269189626
real(kind=double) :: pt2=0.774596669241483
real(kind=double) r,s,t,b,c,shrink 
!
!Setting up Gauss Integration Point Coordinates for interface
!
ngauss_face=4
if(ngauss_face==4) then
   if(orien==1.or.orien==2) then
       if(igauss==1) then
          s=pt1
          t=-pt1
       end if
       if(igauss==2) then
          s=pt1
          t=pt1
       end if
       if(igauss==3) then
          s=-pt1
          t=pt1
       end if
       if(igauss==4) then 
          s=-pt1
          t=-pt1
       end if
       if(orien==1) r=one_d
       if(orien==2) r=-one_d
   end if
   if(orien==3.or.orien==4) then
       if(igauss==1) then
          t=pt1
          r=-pt1
       end if
       if(igauss==2) then
          t=pt1
          r=pt1
       end if
       if(igauss==3) then
          t=-pt1
          r=pt1
       end if
       if(igauss==4) then
          t=-pt1
          r=-pt1 
       end if
       if(orien==3) s=one_d
       if(orien==4) s=-one_d
   end if

   if(orien==5.or.orien==6) then
      if(igauss==1) then
          r=pt1
          s=-pt1
       end if
       if(igauss==2) then
          r=pt1
          s=pt1
       end if
       if(igauss==3) then
          r=-pt1
          s=pt1
       end if
       if(igauss==4) then
          r=-pt1
          s=-pt1
       end if 
       if(orien==5) t=one_d
       if(orien==6) t=-one_d
   end if
end if
if(ngauss_face==9) then
   if(orien==1.or.orien==2) then
       if(igauss==1) then
          s=pt2
          t=-pt2
       end if
       if(igauss==2) then
          s=pt2
          t=zero_d
       end if
       if(igauss==3) then
          s=pt2
          t=pt2
       end if
       if(igauss==4) then 
          s=zero_d
          t=-pt2
       end if
       if(igauss==5) then
          s=zero_d
          t=-zero_d
       end if
       if(igauss==6) then
          s=zero_d
          t=pt2
       end if
       if(igauss==7) then
          s=-pt2
          t=-pt2
       end if
       if(igauss==8) then 
          s=-pt2
          t=zero_d
       end if
       if(igauss==9) then 
          s=-pt2
          t=pt2
       end if
       if(orien==1) r=one_d
       if(orien==2) r=-one_d
   end if

   if(orien==3.or.orien==4) then
       if(igauss==1) then
          t=pt2
          r=-pt2
       end if
       if(igauss==2) then
          t=pt2
          r=zero_d
       end if
       if(igauss==3) then
          t=pt2
          r=pt2
       end if
       if(igauss==4) then 
          t=zero_d
          r=-pt2
       end if
       if(igauss==5) then
          t=zero_d
          r=-zero_d
       end if
       if(igauss==6) then
          t=zero_d
          r=pt2
       end if
       if(igauss==7) then
          t=-pt2
          r=-pt2
       end if
       if(igauss==8) then 
          t=-pt2
          r=zero_d
       end if
       if(igauss==9) then 
          t=-pt2
          r=pt2
       end if
       if(orien==3) s=one_d
       if(orien==4) s=-one_d
   end if

   if(orien==5.or.orien==6) then
       if(igauss==1) then
          r=pt2
          s=-pt2
       end if
       if(igauss==2) then
          r=pt2
          s=zero_d
       end if
       if(igauss==3) then
          r=pt2
          s=pt2
       end if
       if(igauss==4) then 
          r=zero_d
          s=-pt2
       end if
       if(igauss==5) then
          r=zero_d
          s=-zero_d
       end if
       if(igauss==6) then
          r=zero_d
          s=pt2
       end if
       if(igauss==7) then
          r=-pt2
          s=-pt2
       end if
       if(igauss==8) then 
          r=-pt2
          s=zero_d
       end if
       if(igauss==9) then 
          r=-pt2
          s=pt2
       end if
       if(orien==5) t=one_d
       if(orien==6) t=-one_d
   end if
end if
return
end

subroutine jacob_face(orien,node_c,deriv,darea,cartdc)
use truth
use model_type
use system
implicit none
integer i,j,kkk,orien
real(kind=double) xjacm(3,3),cartdc(ele_node,3)
real(kind=double) xjacmi(3,3),node_c(ele_node,3)
real(kind=double) darea,deriv(ele_node,3),temp(3,1)
real(kind=double) p1,p2,p3,djacb,shap(ele_node)
xjacm=zero_d
cartdc(1:ele_node,1:3)=zero_d
do i=1,3
   do j=1,3
      do kkk=1,ele_node
         xjacm(i,j)=xjacm(i,j)+deriv(kkk,i)*node_c(kkk,j)
      end do
   end do
end do
djacb=xjacm(1,1)*xjacm(2,2)*xjacm(3,3)+xjacm(1,2)* &
      xjacm(2,3)*xjacm(3,1)+xjacm(1,3)*xjacm(2,1)* &
      xjacm(3,2)-xjacm(1,3)*xjacm(2,2)*xjacm(3,1)- &
      xjacm(1,2)*xjacm(2,1)*xjacm(3,3)-xjacm(1,1)* &
      xjacm(2,3)*xjacm(3,2)  
if(orien==5.or.orien==6) then
      p1=xjacm(1,1)*xjacm(1,1)+xjacm(1,2)*xjacm(1,2)+ &
         xjacm(1,3)*xjacm(1,3)
      p2=xjacm(1,1)*xjacm(2,1)+xjacm(1,2)*xjacm(2,2)+ &
         xjacm(1,3)*xjacm(2,3)
      p3=xjacm(2,1)*xjacm(2,1)+xjacm(2,2)*xjacm(2,2)+ &
         xjacm(2,3)*xjacm(2,3)    
elseif(orien==1.or.orien==2) then
	  p1=xjacm(2,1)*xjacm(2,1)+xjacm(2,2)*xjacm(2,2)+ &
           xjacm(2,3)*xjacm(2,3)
	  p2=xjacm(2,1)*xjacm(3,1)+xjacm(2,2)*xjacm(3,2)+ &
           xjacm(2,3)*xjacm(3,3)
      p3=xjacm(3,1)*xjacm(3,1)+xjacm(3,2)*xjacm(3,2)+ &
           xjacm(3,3)*xjacm(3,3) 
else
      p1=xjacm(1,1)*xjacm(1,1)+xjacm(1,2)*xjacm(1,2)+ &
           xjacm(1,3)*xjacm(1,3)
	  p2=xjacm(1,1)*xjacm(3,1)+xjacm(1,2)*xjacm(3,2)+ &
           xjacm(1,3)*xjacm(3,3)
      p3=xjacm(3,1)*xjacm(3,1)+xjacm(3,2)*xjacm(3,2)+ &
           xjacm(3,3)*xjacm(3,3) 
end if
darea=sqrt(abs(p1*p3-p2*p2))
xjacmi(1,1)=(xjacm(2,2)*xjacm(3,3)-xjacm(2,3)*xjacm(3,2))
xjacmi(1,2)=(xjacm(1,3)*xjacm(3,2)-xjacm(1,2)*xjacm(3,3))
xjacmi(1,3)=(xjacm(1,2)*xjacm(2,3)-xjacm(1,3)*xjacm(2,2))
xjacmi(2,1)=(xjacm(2,3)*xjacm(3,1)-xjacm(2,1)*xjacm(3,3))
xjacmi(2,2)=(xjacm(1,1)*xjacm(3,3)-xjacm(1,3)*xjacm(3,1))
xjacmi(2,3)=(xjacm(1,3)*xjacm(2,1)-xjacm(1,1)*xjacm(2,3))
xjacmi(3,1)=(xjacm(2,1)*xjacm(3,2)-xjacm(2,2)*xjacm(3,1))
xjacmi(3,2)=(xjacm(1,2)*xjacm(3,1)-xjacm(1,1)*xjacm(3,2))
xjacmi(3,3)=(xjacm(1,1)*xjacm(2,2)-xjacm(1,2)*xjacm(2,1))
xjacmi=xjacmi/djacb
do i=1,ele_node
   cartdc(i,1:3)=matmul(xjacmi(1:3,1:3),deriv(i,1:3))
end do
return
end

subroutine b_face_load(iface,mele_load)
use truth
use model_type
use system
implicit none
integer igauss,ele_ID,local_face,iface
real(kind=double) area
real(kind=double) N_mat(3,24),B_mat(6,24)
real(kind=double) normal(3),mele_load1(24,1),mele_load(24),ppp
real(kind=double) wght(9),p_pressure,n_vector0(3,1),traction(3)
N_mat=zero_d
B_mat=zero_d
mele_load=0.0d0
mele_load1=0.0d0
ngauss_face=4
wght=0.0d0
wght(1:4)=face_wght_4(1:4)
ele_ID=b_face(iface)%element
local_face=b_face(iface)%orien
traction=0.0d0
if(b_face(iface)%pressure>0) then
   call face_normal(ele_ID,local_face,normal) 
   traction=b_face(iface)%pressure_value*normal
else
   traction=b_face(iface)%traction
end if
do igauss=1,ngauss_face
   call face_NB_mat(ele_ID,local_face,igauss,N_mat,B_mat,area)
   n_vector0(1:3,1)=traction
   mele_load1=mele_load1+matmul(transpose(N_mat),n_vector0)&
              *area*wght(igauss)
end do 
mele_load(:)=mele_load1(:,1)
return
end

