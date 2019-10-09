subroutine crack_profile_setup(    node_total,& 
                                    ele_total,&
                               crack_ibc_face,&
                                   node_coord,&   
                                 elm_gnode_id,&
                                  dis_bc_node,&             
                                force_bc_node,& 
                              pressure_bc_face)
!
!Author: Ruijie Liu
!07/25/2012 
!
!adding new nodes due to cracks;
!considering crack on boundary surfaces;
!updating element nodes;
!updating force, surface,displacement boundary
!conditions due to the cracks.
!
use truth
use system
implicit none
integer elm_gnode_id(8,*)
real(kind=double) node_coord(3,*)             
real(kind=double) dis_bc_node(3,*)   
real(kind=double) force_bc_node(3,*)
real(kind=double) pressure_bc_face(3,*)
real(kind=double) crack_ibc_face(3,*)                   
integer tl_node_on_crack,face_id,elm_id,inode,iface
integer gnode,jnode,flag0,tl_new_node,ele_total,node_total
integer element_id,nb_elm_id,nb_face_id,new_bc_number,load_node
integer new_node_id,node_affine_id,nelem,ielem,inc,i,j,jjj,kkk
integer node_tl_new,gnode_on_crack,flag_on_face,node_id
integer node_on_crack(4*tl_cracked_face)
integer node_shared_by_elements(4,4*tl_cracked_face)
integer node_on_crack_sitting(4*tl_cracked_face)
integer new_interior_node_id(4*tl_cracked_face)
integer new_interior_node_affine(4*tl_cracked_face)
integer new_interior_node_elmtl(4*tl_cracked_face)
integer node_tl_elm_number(4*tl_cracked_face)
integer new_interior_node_elm(4,4*tl_cracked_face)
integer new_interior_node_face(4,4*tl_cracked_face)
integer local_face_node(4)
total_node_ipars=node_total
!
!restating ipars face node notations:
!
call IPARS_node_on_face
!
!loop on each cracked face for classfying nodes on crack surface
!These nodes form the topology of crack surface
!Corner nodes;crack front nodes;nodes sitting on interior of the crack surface 
!
tl_node_on_crack=0
!
!starting the first cracked face
!
!write(40,*) 'tl_cracked_face==',tl_cracked_face
!do i=1,tl_cracked_face
!write(40,*) 'crack ele face',crack_ibc_face(1:3,i)
!end do
elm_id=int(crack_ibc_face(1,1))
face_id=int(crack_ibc_face(2,1))
do inode=1,4
   node_id=IPARS_face_node(inode,face_id)
   tl_node_on_crack=tl_node_on_crack+1
   node_on_crack(tl_node_on_crack)=elm_gnode_id(node_id,&
   elm_id)
end do
!
!loop over all cracked surface
!    
do iface=2,tl_cracked_face
   elm_id=int(crack_ibc_face(1,iface))
   face_id=int(crack_ibc_face(2,iface))
   do jnode=1,4
      node_id=IPARS_face_node(jnode,face_id)
      gnode=elm_gnode_id(node_id,elm_id)
      !
      !checking if this node already in the list
      !
      flag0=0
      do inode=1,tl_node_on_crack
         if(gnode==node_on_crack(inode)) then
            flag0=1
            exit
         end if
      end do
      !
      !not in the list and count this
      !
      if(flag0==0) then
         tl_node_on_crack=tl_node_on_crack+1
         node_on_crack(tl_node_on_crack)=gnode      
      end if
   end do
end do
!write(40,*) 'tl_node_on_crack=',tl_node_on_crack
!do i=1,tl_node_on_crack
!   write(40,*) 'node_on_crack',node_on_crack(i)
!end do

!
!nodes on crack surfaces are now sorted out.
!Next, classify these nodes for siting on corner, side,interior...
!

!bw The algorithm below is very inefficient, need to rewrite the
!   sorting part  
!   construct node_tl_elm_number(inode) when construct inode list

do inode=1,tl_node_on_crack
   flag0=0
   gnode_on_crack=node_on_crack(inode)
   do iface=1,tl_cracked_face
      flag_on_face=0
      elm_id=int(crack_ibc_face(1,iface))
      face_id=int(crack_ibc_face(2,iface))
      do jnode=1,4
         node_id=IPARS_face_node(jnode,face_id)
         gnode=elm_gnode_id(node_id,elm_id)
         !
         !checking if gnode==gnode_on_crack
         !
         if(gnode_on_crack==gnode) then
            flag_on_face=1
            flag0=flag0+1
         end if
      end do
      !
      !figuring out how many elements sharing this node
      !
      if(flag_on_face>0) then
         if(flag0>4) then
           !
           !issuee error for crack input data
           !
           write(*,*)
           write(*,*) '*********WARNINGS or ERRORS***********'
           write(*,*) 'Crack surfaces are not smooth and please check crack input data'
           write(*,*)
         end if
         node_shared_by_elements(flag0,inode)=elm_id
      end if
   end do
   node_tl_elm_number(inode)=flag0   
   if(flag0==1) node_on_crack_sitting(inode)=1 !corner
   if(flag0==2) node_on_crack_sitting(inode)=2 !side !bw  edge
   if(flag0==4) node_on_crack_sitting(inode)=3 !interior   
end do
!do i=1,tl_node_on_crack
!   write(40,*) 'node_on_crack_sitting',node_on_crack_sitting(i)
!end do

!
!handling crack on boundaries:  
!check node connecting elements
!
do inode=1,tl_node_on_crack
   if(node_on_crack_sitting(inode).ne.2) cycle
   !
   !this is a node on side to be checked
   !
   gnode_on_crack=node_on_crack(inode)

!bw semi-open fracture on surface? the logic used to identify
!   boundary node needs to be changed for parallel
   call node_on_boundary_surface(gnode_on_crack,&
                    ele_total,elm_gnode_id,flag0)
   if(flag0>0) node_on_crack_sitting(inode)=3
end do
!do i=1,tl_node_on_crack
!   write(40,*) 'secondsiit',node_on_crack_sitting(i)
!end do
!
!Creating new nodes and associating them with elements and 
!with old nodes sharing the same coordinates.
!
tl_new_node=0
do inode=1,tl_node_on_crack
   !
   !Checking only interior nodes
   !
   if(node_on_crack_sitting(inode).ne.3) cycle
   tl_new_node=tl_new_node+1
   node_tl_new=node_tl+tl_new_node
   new_interior_node_id(tl_new_node)=node_tl_new
   new_interior_node_affine(tl_new_node)=node_on_crack(inode)
   !
   !Get elements associated with this new node
   !
   nelem=node_tl_elm_number(inode)
   new_interior_node_elmtl(tl_new_node)=nelem
   do i=1,nelem
      elm_id=node_shared_by_elements(i,inode)
      do iface=1,tl_cracked_face
         element_id=int(crack_ibc_face(1,iface))
         if(elm_id.ne.element_id) cycle
         !
         !getting its neighbor shared the face
         !
         face_id=int(crack_ibc_face(2,iface))
         call elem_shared_face(elm_id,&
                              face_id,&
                            ele_total,&   
                         elm_gnode_id,&
                            nb_elm_id,&             
                            nb_face_id)
         new_interior_node_elm(i,tl_new_node)=nb_elm_id
         new_interior_node_face(i,tl_new_node)=nb_face_id
      end do
  end do
end do  
!write(40,*) 'tl_new_node=',tl_new_node 
!do i=1,tl_new_node
!   write(40,*) 'new_interior_node_id=',new_interior_node_id(i)
!   write(40,*) 'new_interior_node_affine=',new_interior_node_affine(i)
!   write(40,*) 'new_interior_node_elmtl=',new_interior_node_elmtl(i)
!   nelem=new_interior_node_elmtl(i)
!   write(40,*) '1--nelem elm',new_interior_node_elm(1:nelem,i)
!   write(40,*) '1--nelem face',new_interior_node_face(1:nelem,i)
!end do
!
!setup crack_profile for plotting
!
if(tl_cracked_face>0) then
  total_frac_node=tl_node_on_crack
  total_frac_elem=tl_cracked_face
  total_frac_interior_node=tl_new_node
  allocate(frac_node_global_id(total_frac_node))
  allocate(frac_node_coord(3,total_frac_node))
  allocate(frac_node_width(3,total_frac_node))
  allocate(frac_elem_connect(4,total_frac_elem))
  allocate(frac_interior_node_id(total_frac_interior_node))
  allocate(frac_interior_node_affine(total_frac_interior_node))
  frac_interior_node_id(1:total_frac_interior_node)=&
  new_interior_node_id(1:total_frac_interior_node)
  frac_interior_node_affine(1:total_frac_interior_node)=&
  new_interior_node_affine(1:total_frac_interior_node)
  frac_node_width(1:3,1:total_frac_node)=0.0d0
  frac_node_global_id(1:total_frac_node)=&
  node_on_crack(1:total_frac_node)
  do inode=1,total_frac_node
     frac_node_coord(1:3,inode)=&
     node_coord(1:3,frac_node_global_id(inode))
  end do
  do iface=1,tl_cracked_face
     elm_id=int(crack_ibc_face(1,iface))
     face_id=int(crack_ibc_face(2,iface))
     do jnode=1,4
        node_id=IPARS_face_node(jnode,face_id)
        gnode=elm_gnode_id(node_id,elm_id)
        frac_elem_connect(jnode,iface)=gnode
     end do
  end do   
end if

!
!updating total nummber of new nodes
!
node_tl=node_total+tl_new_node
!
!Adding new pressured boundary surfaces due to crack
!

! debug
!write(40,*) 'tl_pressured_face1==',tl_pressured_face
!do iface=1,tl_pressured_face
!   write(40,*) 'iface elmid faceID value',&
!   iface, pressure_bc_face(1:3,iface)
!end do

new_bc_number=0
do iface=1,tl_cracked_face
   elm_id=int(crack_ibc_face(1,iface))
   face_id=int(crack_ibc_face(2,iface))
   call elem_shared_face(elm_id,&
                        face_id,&
                      ele_total,&   
                   elm_gnode_id,&
                      nb_elm_id,&             
                      nb_face_id)
!   write(40,*) 'elm_id face_id',elm_id,face_id
!   write(40,*) 'nbelm_id nbface_id',nb_elm_id,nb_face_id
   new_bc_number=new_bc_number+1
   pressure_bc_face(1,new_bc_number+tl_pressured_face)=elm_id
   pressure_bc_face(2,new_bc_number+tl_pressured_face)=face_id

! bag8 -- replaced hard coded pressure with CRAC_IBC from IPARS
   pressure_bc_face(3,new_bc_number+tl_pressured_face)= -crack_ibc_face(3,iface)

!   !hard code pressure
!   pressure_bc_face(3,new_bc_number+tl_pressured_face)=-2000.0
!   !crack_ibc_face(3,iface)+&
!   !10*pressure_bc_face(3,1)

!   write(40,*) 'iface elmid faceID value',&
!   iface, pressure_bc_face(1:3,new_bc_number+tl_pressured_face)
   new_bc_number=new_bc_number+1
   pressure_bc_face(1,new_bc_number+tl_pressured_face)=nb_elm_id
   pressure_bc_face(2,new_bc_number+tl_pressured_face)=nb_face_id

! bag8 -- replaced hard coded pressure with CRAC_IBC from IPARS
   pressure_bc_face(3,new_bc_number+tl_pressured_face)= -crack_ibc_face(3,iface)

!   !hard code pressure
!   pressure_bc_face(3,new_bc_number+tl_pressured_face)=-2000.0
!   !crack_ibc_face(3,iface)+&
!   !10*pressure_bc_face(3,1)

!   write(40,*) 'iface elmid faceID value',&
!   iface, pressure_bc_face(1:3,new_bc_number+tl_pressured_face)
end do
!
!updating total # of pressure surface (due double cracked surface)
!
tl_pressured_face=tl_pressured_face+new_bc_number
!
!using the new node associated affine node, element, face
!to updating element global node id, new internal 
!boundary surfaces
!
!
do inode=1,tl_new_node
   new_node_id=new_interior_node_id(inode)
   node_affine_id=new_interior_node_affine(inode)
   !
   !computing coordinates of new nodes
   !
   node_coord(1:3,new_node_id)=node_coord(1:3,node_affine_id)
   !
   !updating element global node ID
   !
   nelem=new_interior_node_elmtl(inode)
   do ielem=1,nelem
      elm_id=new_interior_node_elm(ielem,inode)
      do jnode=1,8
         if(node_affine_id==elm_gnode_id(jnode,elm_id)) then
            !
            !updating this node id for this element with
            !new cracked surface 
            !
            elm_gnode_id(jnode,elm_id)=new_node_id
!            write(40,*) 'elmid==',elm_id
!            write(40,*) 'node',elm_gnode_id(1:8,elm_id)
            exit
          end if
      end do
   end do
end do

!
!updating displacement boundary conditions
!
new_bc_number=0
do inc=1,tl_0_dch
      load_node=int(dis_bc_node(1,inc))
      !
      !check if node "load_node" is on affine list of the new nodes
      !
      do inode=1,tl_new_node
         node_affine_id=new_interior_node_affine(inode)
         if(load_node==node_affine_id) then
           !
           !this is a boundary node and the splitted node 
           !"new_node_id" must be the same as load_node  
           new_node_id=new_interior_node_id(inode)
           new_bc_number=new_bc_number+1
           dis_bc_node(1,tl_0_dch+new_bc_number)=new_node_id
           dis_bc_node(2,tl_0_dch+new_bc_number)=&
           dis_bc_node(2,inc)
           dis_bc_node(3,tl_0_dch+new_bc_number)=&
           dis_bc_node(3,inc)
           exit
         end if
      end do
end do
!
!updating total # of displacement bc
!
tl_0_dch=tl_0_dch+new_bc_number
!
!updating nodal force boundary conditions
!
new_bc_number=0
do inc=1,tl_load_point
      load_node=int(force_bc_node(1,inc))
      !
      !check if node "load_node" is on affine list of the new nodes
      !
      do inode=1,tl_new_node
         node_affine_id=new_interior_node_affine(inode)
         if(load_node==node_affine_id) then
           !
           !this is a boundary node and the splitted node 
           !"new_node_id" must be the same as load_node  
           new_node_id=new_interior_node_id(inode)
           new_bc_number=new_bc_number+1
           force_bc_node(1,tl_load_point+new_bc_number)=new_node_id
           force_bc_node(2,tl_load_point+new_bc_number)=&
           force_bc_node(2,inc)
         force_bc_node(3,tl_load_point+new_bc_number)=&
        force_bc_node(3,inc)
           exit
         end if
      end do
end do
!
!updating total # of nodal force B.C.
!
tl_load_point=tl_load_point+new_bc_number
return
end

subroutine elem_shared_face(elm_id,&
                           face_id,&
                         ele_total,&   
                      elm_gnode_id,&
                         nb_elm_id,&             
                         nb_face_id)
!
!Author: Ruijie Liu
! 
!utility routine: find the neighbor element
!sharing the face defined by local face of this 
!element
!
use truth
use system
implicit none
integer ele_total
integer elm_gnode_id(8,*)
integer elm_id
integer face_id
integer nb_elm_id
integer nb_face_id
integer j,jj,jjj,kkk,flag0
integer local_face_node(4),face_node_a(4),face_node_b(4)

!bw IPARS_node_on_face has been called already!
!   call IPARS_node_on_face
!   need to simplify this subroutine, using low/high
!   relationship, only check only one face of a potential
!   neighboring element
local_face_node(1:4)=IPARS_face_node(1:4,face_id)          
do j=1,4
   face_node_a(j)=elm_gnode_id(local_face_node(j),elm_id)
end do
do j=1,ele_total
!bw skip element ele_id
   if (j==elm_id) cycle
   do jj=1,6           
      local_face_node(1:4)=IPARS_face_node(1:4,jj)
      do jjj=1,4
         face_node_b(jjj)=elm_gnode_id(&
                  local_face_node(jjj),j)
      end do
      !
      !comparing nodes
      !
      flag0=0
      do jjj=1,4
         do kkk=1,4
            if(face_node_a(jjj)==face_node_b(kkk)) then
               flag0=flag0+1
               cycle
            end if
         end do
      end do
      if(flag0==4) then
         nb_elm_id=j
         nb_face_id=jj
! bag8 - bug fix for fracture on high/low face
!bw         if (elm_id.ne.nb_elm_id) exit
         exit
      end if
    end do
!bw    if(flag0==4) exit
end do
return
end



subroutine node_on_boundary_surface(node_id,nelem,&
                                elm_gnode_id,flag)
use truth
!
!utility routine: given a node, check if it is on
!the boundary surfaces.  flag0: 1 on boundary
!
integer flag0,node_id,i,j
integer elm_gnode_id(8,*)
flag=0
do i=1,nelem
   do j=1,8
      if(node_id.ne.elm_gnode_id(j,i)) cycle
      flag=flag+1
   end do
end do
if(flag==4) then
   flag=1
else
   flag=0
end if
return
end
