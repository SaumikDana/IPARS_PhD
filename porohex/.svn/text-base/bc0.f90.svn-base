subroutine bc(ncase,gciter)
!
!Author: Ruijie Liu, Bin Wang
!
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

!bw
!disp_bc_residue=0.d0
!bw
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
      !off diaognal
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
!bw      do row=1,dof_tl                 
            call mapping_gij_to_compressed_index(row,col,cindex)
            if(cindex.gt.0) then
               tl_load(row)=tl_load(row)-gstiff(cindex)*value0
!bw during initialization, store the RHS induced by non-zero Dirichlet BC
!               disp_bc_residue(row)=disp_bc_residue(row)-gstiff(cindex)*value0
!bw
               gstiff(cindex)=0.0d0
         end if         
         ENDDO
      ENDDO 
!bw      end do
      do j=address1,address2 
         gstiff(j)=0.0d0 
      end do
      !
      !get diaognal address
      !
      call mapping_gij_to_compressed_index(row_index,row_index,cindex) 
      gstiff(cindex)=1.0d0
      tl_load(row_index)=value0
   end do
end if
if(ncase==2) then
!   tl_load=tl_load+disp_bc_residue
   do i=1,tl_0_dch  
      row_index=zero_dch(i)        
      tl_load(row_index)=zero_d     
   end do
end if
residue=tl_load
return
end


SUBROUTINE BC_STEP(NCASE)
!
! Bin Wang
!
!       
USE TRUTH
USE MODEL_TYPE
USE SYSTEM
IMPLICIT NONE
INTEGER I,J,NCASE,ROW_INDEX,COL_INDEX,CINDEX,ADDRESS1,ADDRESS2,ROW,COL
REAL(KIND=DOUBLE) VALUE0

RESIDUE=TL_LOAD+DISP_BC_RESIDUE
IF(NCASE==1.OR.NCASE==3) THEN
   DO I=1,TL_0_DCH
      ROW_INDEX=ZERO_DCH(I)
      IF(NCASE==1) THEN
      VALUE0=ZERO_VALUE(I)
      ELSE
      VALUE0=0.0D0
      ENDIF
      !
      !GET DIAOGNAL ADDRESS
      !
!      CALL MAPPING_GIJ_TO_COMPRESSED_INDEX(ROW_INDEX,ROW_INDEX,CINDEX) 
      RESIDUE(ROW_INDEX)=VALUE0
   END DO
END IF
IF(NCASE==2) THEN
   DO I=1,TL_0_DCH  
      ROW_INDEX=ZERO_DCH(I)        
      RESIDUE(ROW_INDEX)=ZERO_D     
   END DO
END IF
RETURN
END
