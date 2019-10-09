!         Author: Ruijie Liu
!         
!      output data for IPARS:
!

!subroutine post_prcss(node_u,node_strss,node_strn,elm_vstrain)
!use truth
!use control
!use model_type
!use system
!implicit none
!integer igauss,inode,i,j,jnode,node_shadow,kkk,node_id_affine
!integer ielem,loct,ncase,node_id,node_flag,jj1,jj2,jj3,dg_flag 
!!bw integer n1,n2,n3,n4,n5,n6,n7,n8,node_count(node_tl),b,c,d,a
!integer n1,n2,n3,n4,n5,n6,n7,n8,b,c,d,a
!integer,allocatable::node_count(:)
!integer node_on_face(4),iface,orien,mat_id
!real(kind=double), allocatable::deriv1(:,:),shap1(:)
!real(kind=double) dvolu,r,s,t,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,tao
!real(kind=double) x11,x12,x13,a1,a2,a3,a4,a5,a6,stress(6),co,so,ro 
!real(kind=double) pt_8,node_strss(6,*),strss_node(6,8),strn_node(6,8),pt_27
!!bw real(kind=double) node_yield(node_tl),node_yield_e(8),yield_node(8)
!real(kind=double) node_yield_e(8),yield_node(8)
!real(kind=double) avg_y,average(13),dx,dy,dz,dx1,dx2,dx3,aaa,mean
!real(kind=double) node_strn(6,*),elm_vstrain(*),node_u(3,*),alpha
!real(kind=double) pore_pressure,initial_stress(6)
!integer ierr
!! produce output data accoring to visual config. 
!pt_8=0.577350269189626
!pt_27=0.774596669241483
!b_dof=6
!node_strss(1:6,1:node_tl)=zero_d
!node_strn(1:6,1:node_tl)=zero_d
!!bw node_yield=zero_d
!ALLOCATE(node_count(node_TL),STAT=IERR)
!IF (IERR.GT.0) THEN
!   WRITE(*,*) "NOT ENOUGH MEMORY IN POST_PRCSS"
!   STOP 462
!ENDIF
!node_count=zero
!yield_node=0.0d0
!do i=1,node_tl
!   node_u(1:3,i)=u(3*(i-1)+1:3*i)
!end do
!do ielem=1,ele_tl
!   elm_vstrain(ielem)=hexa(ielem)%vstrain
!   pore_pressure=hexa(ielem)%pore_pressure
!   initial_stress=hexa(ielem)%initial_stress
!   mat_id=hexa(ielem)%mat_no
!   alpha=mat_prpty(mat_id)%prpty_sld(6)
!   ngauss=hexa(ielem)%ngauss
!   ele_type=hexa(ielem)%ele_type
!   node_dof=3
!   b_dof=6
!   ele_node=hexa(ielem)%tl_node
!   ele_dof=ele_node*node_dof
!   allocate(deriv1(ele_node,3))
!   allocate(shap1(ngauss))
!   do inode=1,8
!      if(ele_type==1) then
!         r=gauss_coord_8(inode,1)/pt_8
!         s=gauss_coord_8(inode,2)/pt_8
!         t=gauss_coord_8(inode,3)/pt_8
!      else
!         r=gauss_coord_27(inode,1)/pt_27
!         s=gauss_coord_27(inode,2)/pt_27
!         t=gauss_coord_27(inode,3)/pt_27
!      end if
!      call shap_g(r,s,t,shap1,deriv1)
!      strss_node(:,inode)=zero_d
!      strn_node(:,inode)=zero_d
!      do igauss=1,8
!         strss_node(1:6,inode)=strss_node(1:6,inode)+&
!         shap1(igauss)*hexa(ielem)%gpt_strss(igauss,1:6)
!         strn_node(1:6,inode)=strn_node(1:6,inode)+&
!         shap1(igauss)*hexa(ielem)%gpt_strn(igauss,1:6)       
!      end do
!       strss_node(1:6,inode)=strss_node(1:6,inode)+initial_stress
!       strss_node(1:3,inode)=strss_node(1:3,inode)-alpha*pore_pressure
!       node_id=hexa(ielem)%node(inode)
!       node_strss(1:6,node_id)=node_strss(1:6,node_id)+strss_node(1:6,inode)
!       node_strn(1:6,node_id)=node_strn(1:6,node_id)+strn_node(1:6,inode)
!       node_count(node_id)=node_count(node_id)+one 
!   end do  
!deallocate(shap1,deriv1)
!end do
!do inode=1,node_tl
!   node_strss(:,inode)=node_strss(:,inode)/node_count(inode)
!   node_strn(:,inode)=node_strn(:,inode)/node_count(inode)      
!end do                                                                       
!return
!end
!
!subroutine transfer_to_gmsh
!use truth
!use system
!implicit none
!gmsh_order(1)=3
!gmsh_order(2)=4
!gmsh_order(3)=1
!gmsh_order(4)=2
!gmsh_order(5)=7
!gmsh_order(6)=8
!gmsh_order(7)=5
!gmsh_order(8)=6
!return
!end

subroutine IPARS_transfer_to_elastic_solver
use truth
use system
implicit none   
IPARS_order(7)=1 !bw maps from internal node order to 
IPARS_order(3)=2 !bw IPARS node order
IPARS_order(6)=3
IPARS_order(2)=4
IPARS_order(8)=5
IPARS_order(4)=6
IPARS_order(5)=7
IPARS_order(1)=8
return
end

subroutine IPARS_face_transfer_to_elastic_solver
use truth   !bw IPARS_face_order maps IPARS face to 
use system  !bw internal face
implicit none
IPARS_face_order(1)=5  !bw this is suspicious, shouldn't it be
IPARS_face_order(2)=6  !bw IPARS_face_order(1)=6, and 
IPARS_face_order(3)=4  !bw IPARS_face_order(2)=5   ?
IPARS_face_order(4)=3
IPARS_face_order(5)=2  !bw This is confirmed, because hexa_face_node is 
IPARS_face_order(6)=1  !bw werid, which made IPARS_face_order weird....
return
end

subroutine IPARS_node_on_face
use truth   !bw IPARS_face_node(i,j) for i=1,4 is IPARS node number
use system  !bw on IPARS face j
implicit none
IPARS_face_node(1,1)=7
IPARS_face_node(2,1)=3
IPARS_face_node(3,1)=1
IPARS_face_node(4,1)=5
IPARS_face_node(1,2)=8
IPARS_face_node(2,2)=4
IPARS_face_node(3,2)=2
IPARS_face_node(4,2)=6
IPARS_face_node(1,3)=6
IPARS_face_node(2,3)=2
IPARS_face_node(3,3)=1
IPARS_face_node(4,3)=5
IPARS_face_node(1,4)=8
IPARS_face_node(2,4)=4
IPARS_face_node(3,4)=3
IPARS_face_node(4,4)=7
!bw dbg The following definition is WRONG! 
!bw IPARS_face_node(1,5)=8
!bw IPARS_face_node(2,5)=7
!bw IPARS_face_node(3,5)=5
!bw IPARS_face_node(4,5)=6
!bw IPARS_face_node(1,6)=4
!bw IPARS_face_node(2,6)=3
!bw IPARS_face_node(3,6)=1
!bw IPARS_face_node(4,6)=2

!bw Corrected Face-Node definition
IPARS_face_node(1,5)=4
IPARS_face_node(2,5)=3
IPARS_face_node(3,5)=1
IPARS_face_node(4,5)=2
IPARS_face_node(1,6)=8
IPARS_face_node(2,6)=7
IPARS_face_node(3,6)=5
IPARS_face_node(4,6)=6
return
end

subroutine hexa_node_on_face
use truth
use system
implicit none
hexa_face_node(1,1)=1
hexa_face_node(2,1)=5
hexa_face_node(3,1)=8
hexa_face_node(4,1)=4
hexa_face_node(1,2)=2
hexa_face_node(2,2)=6
hexa_face_node(3,2)=7
hexa_face_node(4,2)=3
hexa_face_node(1,3)=1
hexa_face_node(2,3)=2
hexa_face_node(3,3)=6
hexa_face_node(4,3)=5
hexa_face_node(1,4)=3
hexa_face_node(2,4)=7
hexa_face_node(3,4)=8
hexa_face_node(4,4)=4
hexa_face_node(1,5)=5
hexa_face_node(2,5)=6
hexa_face_node(3,5)=7
hexa_face_node(4,5)=8
hexa_face_node(1,6)=1
hexa_face_node(2,6)=2
hexa_face_node(3,6)=3
hexa_face_node(4,6)=4
return
end


!subroutine for_gmsh(node_u,node_strss,node_strn)
!use truth
!use control
!use model_type
!use system
!implicit none
!integer igauss,inode,i,j,jnode,node_shadow
!integer ielem,loct,ncase,node_id,node_flag,jj1,jj2,jj3,dg_flag 
!integer n1,n2,n3,n4,n5,n6,n7,n8,node_count(node_tl),b,c,d,a
!integer node_on_face(4),iface,orien
!real(kind=double), allocatable::deriv1(:,:),shap1(:)
!real(kind=double) dvolu,r,s,t,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,tao
!real(kind=double) x11,x12,x13,a1,a2,a3,a4,a5,a6,stress(6),co,so,ro 
!real(kind=double) pt_8,node_strss(6,*),strss_node(6,8),pt_27
!real(kind=double) node_yield(node_tl),node_yield_e(8),yield_node(8)
!real(kind=double) node_u(3,*),node_strn(6,*),dx,dy,dz,dx1,dx2,dx3,aaa,mean
!node_count=zero         
!write(200,101)'$MeshFormat'
!write(300,101)'$MeshFormat'
!write(400,101)'$MeshFormat'
!write(500,101)'$MeshFormat'
!write(600,101)'$MeshFormat'
!write(700,101)'$MeshFormat'
!a=2.0
!b=0
!c=8
!write(200,*)a,b,c
!write(200,101)'$EndMeshFormat'
!write(200,101)'$Nodes'
!write(200,*)node_tl
!write(300,*)a,b,c
!write(300,101)'$EndMeshFormat'
!write(300,101)'$Nodes'
!write(300,*)node_tl
!write(400,*)a,b,c
!write(400,101)'$EndMeshFormat'
!write(400,101)'$Nodes'
!write(400,*)node_tl
!write(500,*)a,b,c
!write(500,101)'$EndMeshFormat'
!write(500,101)'$Nodes'
!write(500,*)node_tl
!write(600,*)a,b,c
!write(600,101)'$EndMeshFormat'
!write(600,101)'$Nodes'
!write(600,*)node_tl
!write(700,*)a,b,c
!write(700,101)'$EndMeshFormat'
!write(700,101)'$Nodes'
!write(700,*)node_tl
!do inode=1,node_tl 
!   x1=coord(inode,1) 
!   x2=coord(inode,2)
!   x3=coord(inode,3) 
!   dx1=node_u(1,inode)  
!   dx2=node_u(2,inode)
!   dx3=node_u(3,inode)  
!    x1=x1+dx1
!    x2=x2+dx2
!    x3=x3+dx3
!   write(200,700) inode,x1,x2,x3     
!700    format(i5,3e14.4)  
!   write(300,701) inode,x1,x2,x3      
!701    format(i5,3e14.4)  
!      write(400,702) inode,x1,x2,x3      
!702    format(i5,3e14.4) 
!      write(500,708) inode,x1,x2,x3      
!708    format(i5,3e14.4)
!      write(600,704) inode,x1,x2,x3      
!704    format(i5,3e14.4)
!      write(700,705) inode,x1,x2,x3      
!705    format(i5,3e14.4)
!end do
!write(200,101)'$EndNodes'
!write(200,101)'$Elements'
!write(200,*) ele_tl  
!write(300,101)'$EndNodes'
!write(300,101)'$Elements'
!write(300,*) ele_tl  
!write(400,101)'$EndNodes'
!write(400,101)'$Elements'
!write(400,*) ele_tl  
!write(500,101)'$EndNodes'
!write(500,101)'$Elements'
!write(500,*) ele_tl   
!write(600,101)'$EndNodes'
!write(600,101)'$Elements'
!write(600,*) ele_tl  
!write(700,101)'$EndNodes'
!write(700,101)'$Elements'
!write(700,*) ele_tl  
!call transfer_to_gmsh  
!b=5
!c=2
!d=99
!ele_node=8
!do ielem=1,ele_tl   
!   write(200,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node) 
!   write(300,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node)
!   write(400,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node)
!   write(500,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node)
!   write(600,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node) 
!   write(700,*) ielem,b,c,d,c,(hexa(ielem)%node(gmsh_order(i)),i=1,ele_node)
!enddo  
!write(200,101)'$EndElements'
!write(200,101)'$NodeData'               
!write(300,101)'$EndElements'
!write(300,101)'$NodeData'
!write(400,101)'$EndElements'
!write(400,101)'$NodeData'
!write(500,101)'$EndElements'
!write(500,101)'$NodeData'
!write(600,101)'$EndElements'
!write(600,101)'$NodeData'
!write(700,101)'$EndElements'
!write(700,101)'$NodeData'
!a=0
!b=1
!c=1
!d=3
!write(200,*) b
!write(200,*)"Field Valuable Contour"
!write(200,*) b
!write(200,*) time_current
!write(200,*) d
!write(200,*) a
!write(200,*) c
!write(200,*) node_tl
!write(300,*) b
!write(300,*)"Field Valuable Contour"
!write(300,*) b
!write(300,*) time_current
!write(300,*) d
!write(300,*) a
!write(300,*) c
!write(300,*) node_tl
!write(400,*) b
!write(400,*)"Field Valuable Contour"
!write(400,*) b
!write(400,*) time_current
!write(400,*) d
!write(400,*) a
!write(400,*) c
!write(400,*) node_tl
!write(500,*) b
!write(500,*)"Field Valuable Contour"
!write(500,*) b
!write(500,*) time_current
!write(500,*) d
!write(500,*) a
!write(500,*) c
!write(500,*) node_tl
!write(600,*) b
!write(600,*)"Field Valuable Contour"
!write(600,*) b
!write(600,*) time_current
!write(600,*) d
!write(600,*) a
!write(600,*) c
!write(600,*) node_tl
!write(700,*) b
!write(700,*)"Field Valuable Contour"
!write(700,*) b
!write(700,*) time_current
!write(700,*) d
!write(700,*) a
!write(700,*) c
!write(700,*) node_tl
!do inode=1,node_tl                        
!   write(200,703) inode,node_u(1,inode)   
!703    format(i5,e14.4)      
!   write(300,604) inode,node_u(2,inode)  
!604    format(i5,e14.4)    
!   write(400,605) inode,node_u(3,inode)  
!605    format(i5,e14.4)   
!   write(500,606) inode,node_strss(1,inode)  
!606    format(i5,e14.4)   
!   write(600,607) inode,node_strss(2,inode)  
!607    format(i5,e14.4)  
!    write(700,608) inode,node_strss(3,inode)  
!608    format(i5,e14.4)   
!end do 
!write(200,101)'$EndNodeData'   
!write(300,101)'$EndNodeData' 
!write(400,101)'$EndNodeData'   
!write(500,101)'$EndNodeData' 
!write(600,101)'$EndNodeData'   
!write(700,101)'$EndNodeData'                          
!101 format(a)
!return
!end

subroutine frac_tecplot(time,node_coord_ipars,elm_gnode_id,frac_width, &
   crack_ibc_face,elm_pore_pressure,poro_neighbor)
! Plots a surface in 3D with fracture width contour in tecplot format.
! Ben Ganis
! bganis@ices.utexas.edu
! 7/31/12
use truth
use control
use model_type
use system
implicit none
double precision time
double precision node_coord_ipars(3,*)
double precision width_all(node_tl),u_a(3)
double precision u_b(3)
real(kind=double) frac_width(*)
real(kind=double) crack_ibc_face(3,*)
real(kind=double) elm_pore_pressure(*)      
integer k,gnode,ii,node_id,elm_gnode_id(8,*)
integer elem_face_connect(4,6,ele_tl),total_elem_face
integer poro_neighbor(6,*)
integer :: i,j, &
  tecunit = 10
character :: &
  title*(80) = 'frac', &
  tecname*(80)
call hexa_node_on_face
do i=1,ele_tl
   do j=1,6
      do k=1,4
         elem_face_connect(k,j,i)=&
         hexa(i)%node(hexa_face_node(k,j))
      end do
   end do
end do
tecname = trim(title)//'.plt'//char(0)

write(*,*)'In frac_tecplot, writing: ',trim(tecname),'...'
if (tecexists.eqv..false.) then
  open(tecunit,file=trim(tecname),status='unknown')
  tecexists = .true.

  write(tecunit,10)'TITLE = "',trim(title),'"'
  write(tecunit,11)'VARIABLES = "X", "Y", "Z", "W", "UX", "UY", "UZ", "P", "Trac", "qL"'
  write(tecunit,11)
else
  open(tecunit,file=trim(tecname),status='old',access='append')
endif

total_elem_face=6*ele_tl
write(tecunit,11)'ZONE T="domain"'
write(tecunit,11)'ZONETYPE=FEPOLYHEDRON'
write(tecunit,11)'DATAPACKING=BLOCK'
write(tecunit,12)'NODES=',node_tl
write(tecunit,12)'ELEMENTS=',ele_tl
write(tecunit,12)'FACES=',total_elem_face
write(tecunit,12)'TOTALNUMFACENODES=',4*total_elem_face
write(tecunit,11)'NUMCONNECTEDBOUNDARYFACES=0'
write(tecunit,11)'TOTALNUMBOUNDARYCONNECTIONS=0'
write(tecunit,11)'PASSIVEVARLIST=[4,9-10]'
write(tecunit,11)'VARLOCATION=([8]=CELLCENTERED)'
write(tecunit,12)'STRANDID=1'
write(tecunit,15)'SOLUTIONTIME=',time
write(tecunit,11)'# X'
write(tecunit,13)(coord(i,1),i=1,node_tl)
write(tecunit,11)'# Y'
write(tecunit,13)(coord(i,2),i=1,node_tl)
write(tecunit,11)'# Z'
write(tecunit,13)(coord(i,3),i=1,node_tl)
write(tecunit,11)'# UX'
write(tecunit,13)(u(3*(i-1)+1),i=1,node_tl)
write(tecunit,11)'# UY'
write(tecunit,13)(u(3*(i-1)+2),i=1,node_tl)
write(tecunit,11)'# UZ'
write(tecunit,13)(u(3*(i-1)+3),i=1,node_tl)
write(tecunit,11)'# P'
write(tecunit,13)(elm_pore_pressure(i),i=1,ele_tl)
write(tecunit,11)'# node count per face'
write(tecunit,16)(4,i=1,total_elem_face)
write(tecunit,11)'# face nodes'
write(tecunit,14)(((elem_face_connect(i,j,k),i=1,4), &
                     j=1,6),k=1,ele_tl)
write(tecunit,11)'# left elements'
do j=1,ele_tl
write(tecunit,16)j,poro_neighbor(5,j),j,poro_neighbor(3,j),j,poro_neighbor(2,j)
if (j.eq.1) then
write(*,*)'ele=',j,',left=',j,poro_neighbor(5,j),j,poro_neighbor(3,j),j,poro_neighbor(2,j)
endif
enddo
write(tecunit,11)'# right elements'
do j=1,ele_tl
write(tecunit,16)poro_neighbor(6,j),j,poro_neighbor(4,j),j,poro_neighbor(1,j),j
if (j.eq.1) then
write(*,*)'ele=',j,',right=',poro_neighbor(6,j),j,poro_neighbor(4,j),j,poro_neighbor(1,j),j
endif
enddo
write(tecunit,11)

if (total_frac_elem.eq.0) goto 101
write(tecunit,11)'ZONE T="frac1"'
write(tecunit,11)'ZONETYPE=FEQUADRILATERAL'
write(tecunit,11)'DATAPACKING=BLOCK'
write(tecunit,12)'NODES=',node_tl
write(tecunit,12)'ELEMENTS=',total_frac_elem
write(tecunit,11)'PASSIVEVARLIST=[5-8,10]'
write(tecunit,11)'VARLOCATION=([9]=CELLCENTERED)'
write(tecunit,12)'STRANDID=2'
write(tecunit,15)'SOLUTIONTIME=',time
write(tecunit,11)'# X'
write(tecunit,13)(coord(i,1),i=1,node_tl)
write(tecunit,11)'# Y'
write(tecunit,13)(coord(i,2),i=1,node_tl)
write(tecunit,11)'# Z'
write(tecunit,13)(coord(i,3),i=1,node_tl)
write(tecunit,11)'# W'

frac_node_width=0.0d0
do i=1,total_frac_interior_node
   j=frac_interior_node_id(i)
   k=frac_interior_node_affine(i)
   u_a=u(3*(j-1)+1:3*(j-1)+3)
   u_b=u(3*(k-1)+1:3*(k-1)+3)
   do ii=1,total_frac_node
      gnode=frac_node_global_id(ii)
      if(gnode==k) then
         frac_node_width(:,ii)=u_a-u_b
         exit
      end if
   end do
end do

do i=1,node_tl
   width_all(i)=0.0d0
   do j=1,total_frac_node
      gnode=frac_node_global_id(j)
      if(i==gnode) then
         do k=1,3
            width_all(i)=width_all(i)+&
            frac_node_width(k,j)*frac_node_width(k,j)
         end do
         width_all(i)=sqrt(width_all(i))
         exit
      end if
   end do
end do
write(tecunit,13)(width_all(i),i=1,node_tl)

write(tecunit,11)'# Trac'
write(tecunit,13)(crack_ibc_face(3,i),i=1,total_frac_elem)
!write(*,*)'crack_ibc_face=',(crack_ibc_face(3,i),i=1,total_frac_elem)

write(tecunit,11)'# Connectivity'
write(tecunit,14)((frac_elem_connect(i,j),i=1,4), &
                     j=1,total_frac_elem)
write(tecunit,11)

! bag8 - Take average of 4 fracture node widths to compute 
!        fracture element width
do j=1,total_frac_elem
frac_width(j)=0.D0
do i=1,4
frac_width(j) = frac_width(j) + &
  0.25D0*width_all(frac_elem_connect(i,j))
enddo
enddo

deallocate(frac_interior_node_affine,&
               frac_interior_node_id,&
                   frac_elem_connect,&
                     frac_node_width,&
                     frac_node_coord,&
                   frac_node_global_id)

101 continue
close(tecunit)

10 format(3a)
11 format(a)
12 format(a,i)
13 format(1p,4e15.6)
14 format(4i)
15 format(1p,a,e15.6)
16 format(6i)

end subroutine frac_tecplot

subroutine post_prcss2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,&
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
        node_u,&
        node_strss,node_strn,elem_vstrain,node_pstrain,&
        node_pstate,node_width,&
        frac_width,frac_width_tmp,fracfaceproc,&
        model_ep)
use truth
use control
use model_type
use system
implicit none

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
INTEGER FRAC,FACE,IOFF,JOFF,KOFF,IERR
INTEGER TOTAL_CRACKED_FACE
INTEGER OFNODE_GNUM(GFSIZE)
REAL*8  OFNODE_DISP(3,GFSIZE)
integer igauss,inode,i,j,jnode,node_shadow,kkk,node_id_affine
integer ielem,loct,ncase,node_id,node_flag,jj1,jj2,jj3,dg_flag 
integer n1,n2,n3,n4,n5,n6,n7,n8,b,c,d,a
integer node_on_face(4),iface,orien,mat_id
real(kind=double) deriv1(8,3),shap1(8)
real(kind=double) deriv2(8,3),shap2(8) ! dana...!
real(kind=double) dvolu,r,s,t,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,tao
real(kind=double) x11,x12,x13,a1,a2,a3,a4,a5,a6,stress(6),co,so,ro 
real(kind=double) pt_8,node_strss(idim,jdim,kdim,6),&
                  strss_node(6,8),strn_node(6,8),pstrn_node(6,8),&
                  pstate_node(3,8),gpt_pstate(8,3),pt_27,&
                  vstrain_node(8),gpt_vstrain(8)
real(kind=double) node_yield_e(8),yield_node(8)
real(kind=double) avg_y,average(13),dx,dy,dz,dx1,dx2,dx3,aaa,mean
real(kind=double) node_strn(idim,jdim,kdim,6),&
                  elem_vstrain(idim,jdim,kdim),&
                  node_pstrain(idim,jdim,kdim,6),&
                  node_pstate(idim,jdim,kdim,3),&
                  node_width(idim,jdim,kdim),&
                  node_u(idim,jdim,kdim,3),&
                  frac_width(mxfracface,mxfrac),&
                  frac_width_tmp(mxfracface,mxfrac),&
                  alpha,disp(3),width
integer  gid,gid2,k,jl1,jl2,ctr,node,ii,jj,kk
INTEGER OFFSET(3,8),I1,J1,K1,IFACE2
INTEGER FRACFACEPROC(MXFRACFACE,MXFRAC)
DATA    OFFSET /0,0,0, 1,0,0, 0,1,0, 1,1,0,&
                0,0,1, 1,0,1, 0,1,1, 1,1,1/
!real(kind=double) node_vstrain(idim,jdim,kdim) ! dana...!

pt_8=1.D0/0.577350269189626D0
pt_27=1.D0/0.774596669241483D0
node_dof=3
b_dof=6

! elasticity variable postprocessing
node_strss=zero_d
node_strn=zero_d
elem_vstrain=zero_d ! dana...!
!node_vstrain=zero_d
do ielem=1,ele_tl
   ngauss=hexa(ielem)%ngauss
   ele_type=hexa(ielem)%ele_type
   ele_node=hexa(ielem)%tl_node
   ele_dof=ele_node*node_dof
   do inode=1,8
      if(ele_type==1) then
         r=gauss_coord_8(inode,1)*pt_8
         s=gauss_coord_8(inode,2)*pt_8
         t=gauss_coord_8(inode,3)*pt_8
      else
         r=gauss_coord_27(inode,1)*pt_27
         s=gauss_coord_27(inode,2)*pt_27
         t=gauss_coord_27(inode,3)*pt_27
      end if
      call shap_g(r,s,t,shap1,deriv1)
      strss_node(:,inode)=zero_d
      strn_node(:,inode)=zero_d
      vstrain_node(inode)=zero_d
      do igauss=1,8
         !
         !Volume Strain
         !
         gpt_vstrain(igauss)=hexa(ielem)%gpt_strn(igauss,1)+&
                              hexa(ielem)%gpt_strn(igauss,2)+&
                              hexa(ielem)%gpt_strn(igauss,3)
      end do
      do igauss=1,8
         strss_node(1:6,inode)=strss_node(1:6,inode)+&
         shap1(igauss)*hexa(ielem)%gpt_strss(igauss,1:6)
         strn_node(1:6,inode)=strn_node(1:6,inode)+&
         shap1(igauss)*hexa(ielem)%gpt_strn(igauss,1:6) 
         vstrain_node(inode)=vstrain_node(inode)+&
         shap1(igauss)*gpt_vstrain(igauss)
      end do
      node_id=hexa(ielem)%node(inode)
      i=lid_2ijk(1,node_id)
      j=lid_2ijk(2,node_id)
      k=lid_2ijk(3,node_id)
      if (i .gt. 0 .and. j .gt. 0 .and. k .gt. 0) then
         node_strss(i,j,k,1:6)=node_strss(i,j,k,1:6)+strss_node(1:6,inode)
         node_strn(i,j,k,1:6)=node_strn(i,j,k,1:6)+strn_node(1:6,inode)
!         node_vstrain(i,j,k)=node_vstrain(i,j,k)+vstrain_node(inode)
      endif
   end do  
   ! volumetric strain computation at element center...!
   r=0.d0
   s=0.d0
   t=0.d0
   call shap_g(r,s,t,shap2,deriv2)
   i=lid_2ijk(1,hexa(ielem)%node(7))
   j=lid_2ijk(2,hexa(ielem)%node(7))
   k=lid_2ijk(3,hexa(ielem)%node(7))
   ! extrapolating from gauss points to element center...!
   do igauss=1,8
      elem_vstrain(i,j,k)=elem_vstrain(i,j,k)+&
                         shap2(igauss)*gpt_vstrain(igauss)
   end do
end do
do inode=1,lallsize
   i=lid_2ijk(1,inode)
   j=lid_2ijk(2,inode)
   k=lid_2ijk(3,inode)
   node_strss(i,j,k,1:6)=node_strss(i,j,k,1:6)/node_nb(inode)%tl_nb_elem
   node_strn(i,j,k,1:6)=node_strn(i,j,k,1:6)/node_nb(inode)%tl_nb_elem
!   node_vstrain(i,j,k)=node_vstrain(i,j,k)/node_nb(inode)%tl_nb_elem
end do                                                                       

! plasticity variable postprocessing
if (MODEL_EP>0) then
node_pstrain=zero_d
node_pstate=zero_d
do ielem=1,ele_tl
   ngauss=hexa(ielem)%ngauss
   ele_type=hexa(ielem)%ele_type
   ele_node=hexa(ielem)%tl_node
   ele_dof=ele_node*node_dof
   do inode=1,8
      if(ele_type==1) then
         r=gauss_coord_8(inode,1)*pt_8
         s=gauss_coord_8(inode,2)*pt_8
         t=gauss_coord_8(inode,3)*pt_8
      else
         r=gauss_coord_27(inode,1)*pt_27
         s=gauss_coord_27(inode,2)*pt_27
         t=gauss_coord_27(inode,3)*pt_27
      end if
      call shap_g(r,s,t,shap1,deriv1)
      pstrn_node(:,inode)=zero_d
      pstate_node(:,inode)=zero_d
      
      do igauss=1,8
         !
         !Volume Plastic Strain
         !
         gpt_pstate(igauss,1)=hexa(ielem)%gpt_pstrn(igauss,1)+&
                              hexa(ielem)%gpt_pstrn(igauss,2)+&
                              hexa(ielem)%gpt_pstrn(igauss,3)
         !
         !EQPL
         ! 
         gpt_pstate(igauss,2)=hexa(ielem)%gpt_vstate(igauss,2)
         !
         !Yield Stress (Hardened)
         !
         gpt_pstate(igauss,3)=hexa(ielem)%gpt_vstate(igauss,1)
      end do
      do igauss=1,8
         pstrn_node(1:6,inode)=pstrn_node(1:6,inode)+&
         shap1(igauss)*hexa(ielem)%gpt_pstrn(igauss,1:6)
         pstate_node(1:3,inode)=pstate_node(1:3,inode)+&
         shap1(igauss)*gpt_pstate(igauss,1:3)
      end do
      node_id=hexa(ielem)%node(inode)
      i=lid_2ijk(1,node_id)
      j=lid_2ijk(2,node_id)
      k=lid_2ijk(3,node_id)
      if (i .gt. 0 .and. j .gt. 0 .and. k .gt. 0) then
         node_pstrain(i,j,k,1:6)=node_pstrain(i,j,k,1:6)+pstrn_node(1:6,inode)
         node_pstate(i,j,k,1:3)=node_pstate(i,j,k,1:3)+pstate_node(1:3,inode)
      endif
   end do  
end do
do inode=1,lallsize
   i=lid_2ijk(1,inode)
   j=lid_2ijk(2,inode)
   k=lid_2ijk(3,inode)
   node_pstrain(i,j,k,1:6)=node_pstrain(i,j,k,1:6)/node_nb(inode)%tl_nb_elem
   node_pstate(i,j,k,1:3)=node_pstate(i,j,k,1:3)/node_nb(inode)%tl_nb_elem
end do
endif                                                                       

! calculate nodal width for open fracture node (active on current processor)
node_width=0.d0
do inode=1,lfallsize
   i=ofnode_affine(1,inode) 
   j=ofnode_affine(2,inode) 
   k=ofnode_affine(3,inode)
   gid=ofnode_l2gid(inode)   
   do ctr=1,gfsize
      gid2=ofnode_gnum(ctr)
      if (gid.eq.gid2) then         
         disp(1:3)=node_u(i,j,k,1:3)-ofnode_disp(1:3,ctr)
         width=disp(1)*disp(1)+disp(2)*disp(2)+disp(3)*disp(3)
         width=sqrt(width)
         node_width(i,j,k)=width
         exit
      endif
   enddo
enddo    

! calculate fracture face width to be sent to mimetic fracture code
CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)

IF (NUMFRAC.GT.0) THEN
   FRAC_WIDTH_TMP=0.D0
   DO FRAC = 1, NUMFRAC
      DO FACE = 1, NUMFRACFACE(FRAC)
         IF (FRACFACEPROC(FACE,FRAC).NE.1) CYCLE
         I = FRACFACE(1,FACE,FRAC) - IOFF
         J = FRACFACE(2,FACE,FRAC) - JOFF
         K = FRACFACE(3,FACE,FRAC) - KOFF
         IFACE=FRACFACE(4,FACE,FRAC)
         I1=I
         J1=J
         K1=K
         IFACE2=IFACE
         IF (IFACE.EQ.2) THEN
            I1=I+1
            IFACE2=1
         ELSEIF (IFACE.EQ.4) THEN
            J1=J+1
            IFACE2=3
         ELSEIF (IFACE.EQ.6) THEN
            K1=K+1
            IFACE2=5
         ENDIF
         WIDTH=0.D0
         IF (KEYOUT(I,J,K).EQ.1) THEN
            DO NODE=1,4
               INODE=IPARS_FACE_NODE(NODE,IFACE)
               II=I+OFFSET(1,INODE)
               JJ=J+OFFSET(2,INODE)
               KK=K+OFFSET(3,INODE)
               WIDTH=WIDTH+0.25D0*NODE_WIDTH(II,JJ,KK)
            ENDDO
         ELSEIF (KEYOUT(I1,J1,K1).EQ.1) THEN
            DO NODE=1,4
               INODE=IPARS_FACE_NODE(NODE,IFACE2)
               II=I1+OFFSET(1,INODE)
               JJ=J1+OFFSET(2,INODE)
               KK=K1+OFFSET(3,INODE)
               WIDTH=WIDTH+0.25D0*NODE_WIDTH(II,JJ,KK)
            ENDDO
         ENDIF
         FRAC_WIDTH_TMP(FACE,FRAC)=WIDTH
      ENDDO
   ENDDO
ENDIF

IF (NUMPRC.EQ.1 .AND. NUMFRAC.GT.0) THEN
   FRAC_WIDTH=FRAC_WIDTH_TMP
ELSEIF (NUMPRC.GT.1 .AND. NUMFRAC.GT.0) THEN
   IFACE=MXFRACFACE*MXFRAC
   CALL UPDATE_WIDTH(FRAC_WIDTH,FRAC_WIDTH_TMP,IFACE)
ENDIF

RETURN

END



