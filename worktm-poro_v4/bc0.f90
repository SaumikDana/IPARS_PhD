subroutine bc(ncase,gciter)
!
!Author: Ruijie Liu, Bin Wang
!       
use truth
use model_type
use system
implicit none
integer ncase,gciter
integer i,j,row_index,col_index,cindex,address1,address2,row,col
real(kind=double) value0
INTEGER NODE,DIR,INODE,TOTAL_NB_NODE,JNODE

! bag8 - Meaning of ncase:
!  1 = Newton iteration 0
!  2 = elasticity, iter > 0
!  3 = plasticity, iter > 0

if(ncase==1.or.ncase==3) then
   do i=1,tl_0_dch
      row_index=zero_dch(i)
      if(ncase==1.and.gciter==1) then    ! saumik - Added gciter
      value0=load_scaling*zero_value(i)    ! bag8 - Dirichlet BC
      else
      value0=0.0d0
      endif
      address1=gstiff_row_index(row_index)
      address2=gstiff_row_index(row_index+1)-1
      !
      !off diagonal
      !
      col=row_index
      node=row_index/3+1
      dir=mod(row_index,3)
      if (dir.eq.0) then
         node=node-1
         dir=3
      endif
      TOTAL_NB_NODE=NODE_NB(NODE)%TL_NB_NODE
      do inode=1,TOTAL_NB_NODE
         jnode=NODE_NB(NODE)%NB_NODE_ID(INODE)
         DO DIR=1,3
            ROW=NODE_ST_DOF(JNODE)+DIR-1
            call mapping_gij_to_compressed_index(row,col,cindex)
            if(cindex.gt.0) then
               tl_load(row)=tl_load(row)-gstiff(cindex)*value0
               gstiff(cindex)=0.0d0
         end if         
         ENDDO
      ENDDO 
      do j=address1,address2 
         gstiff(j)=0.0d0 
      end do
      !
      !get diagonal address
      !
      call mapping_gij_to_compressed_index(row_index,row_index,cindex) 
      gstiff(cindex)=1.0d0
      tl_load(row_index)=value0
   end do
end if
if(ncase==2) then
   do i=1,tl_0_dch  
      row_index=zero_dch(i)        
      tl_load(row_index)=zero_d     
   end do
end if
residue=tl_load
return
end

