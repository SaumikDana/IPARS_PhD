!======================================================================
      SUBROUTINE SUPERLU_POROHEX(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V, &
          KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,GELEINDEX,GSTIFF,               &
          GSTIFF_ROW_INDEX,GSTIFF_COL_INDEX,ACTIVE_NODE,RESIDUE,        &
          TL_LOAD,EDISP,NODE_ST_DOF,NODE_ST_GDOF,ROWS,OFNODE_DISP,      &
          OFNODE_DISP_TMP,OFNODE_KEYOUT,NNTIM,TL_NONZERO,NODE_TL,       &
          OFNODE_GNUM,OFNODE_GNUM_TMP,OFNODE_L2GID)
!======================================================================
      USE superlu_mod
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'
      INCLUDE 'emodel.h'
      INCLUDE 'hypre.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V(KDIM),JL2V(KDIM),&
              KL1,KL2,KEYOUT(IDIM,JDIM,KDIM),NBLK
      INTEGER KEYOUT_CR(IDIM,JDIM,KDIM),GELEINDEX(IDIM,JDIM,KDIM)
      INTEGER NNTIM,TL_NONZERO,NODE_TL
      REAL*8  GSTIFF(TL_NONZERO),EDISP(IDIM,JDIM,KDIM,3),&
              OFNODE_DISP(3,POROHEX_GFSIZE),RESIDUE(NODE_TL*3),&
              TL_LOAD(NODE_TL*3),OFNODE_DISP_TMP(3,POROHEX_GFSIZE)
      INTEGER GSTIFF_ROW_INDEX(NODE_TL*3+1),GSTIFF_COL_INDEX(TL_NONZERO)
      INTEGER ACTIVE_NODE(NODE_TL),NODE_ST_DOF(NODE_TL),&
              NODE_ST_GDOF(NODE_TL),ROWS(NODE_TL*3),&
              OFNODE_KEYOUT(POROHEX_LFALLSIZE),&
              OFNODE_GNUM(POROHEX_GFSIZE),&
              OFNODE_GNUM_TMP(POROHEX_GFSIZE),&
              OFNODE_L2GID(POROHEX_LFALLSIZE)

      INTEGER IERR,IROW
      
      INTEGER I,J,K,LROW,LCOL,COL1,COL2,COL3,DIR,NODE
      INTEGER HILOWER,HIUPPER,HLSIZE,BUFSIZE

      integer nprow, npcol, n, m, ldb, nrhs, ICTR, JCTR
      integer*4 iam, ldb4, info

      integer nnz_loc,m_loc,fst_row,berr,init
      real*8, allocatable :: nzval(:)
      integer, allocatable :: colind(:),rowptr(:)

      integer(superlu_ptr) :: grid
      integer(superlu_ptr) :: options
      integer(superlu_ptr) :: ScalePermstruct
      integer(superlu_ptr) :: LUstruct
      integer(superlu_ptr) :: SOLVEstruct
      integer(superlu_ptr) :: A
      integer(superlu_ptr) :: stat

      IF (MYPRC.EQ.0) write(*,*)'In SUPERLU_POROHEX'

! Create Fortran handles for the C structures used in SuperLU_DIST
      call f_create_gridinfo_handle(grid)
      call f_create_options_handle(options)
      call f_create_ScalePerm_handle(ScalePermstruct)
      call f_create_LUstruct_handle(LUstruct)
      call f_create_SOLVEstruct_handle(SOLVEstruct)
      call f_create_SuperMatrix_handle(A)
      call f_create_SuperLUStat_handle(stat)

! Initialize the SuperLU_DIST process grid
      nprow = NUMPRC
      npcol = 1
      call f_superlu_gridinit(MPI_COMM_WORLD, nprow, npcol, grid)

      HILOWER=(POROHEX_ILOWER-1)*3+1
      HIUPPER=POROHEX_IUPPER*3
      HLSIZE=3*(POROHEX_LSIZE+POROHEX_LFSIZE)

      nrhs=1
      m = 3*POROHEX_GSIZE
      n = m
      fst_row = HILOWER-1
      m_loc = HLSIZE

      allocate(colind(TL_NONZERO),stat=ierr)
      if (ierr.ne.0) stop 1
      allocate(rowptr(m_loc+1),stat=ierr)
      if (ierr.ne.0) stop 1
      allocate(nzval(TL_NONZERO),stat=ierr)
      if (ierr.ne.0) stop 1

! Bail out if I do not belong in the grid. 
      call get_GridInfo(grid, iam=iam)
      if (iam >= nprow * npcol) then
        write(*,*)'Error: proc',myprc,'does not belong to superlu'
        stop 13
      endif

! SETUP GLOBAL ID ARRAY ASSOCIATED WITH OFNODE_DISP

!      OFNODE_GNUM=0
!      CTR=0
!      DO I=1,POROHEX_LFALLSIZE
!         IF (OFNODE_KEYOUT(I).EQ.1) THEN
!            CTR=CTR+1 
!            NODE=POROHEX_IFLOWER+CTR-1 
!            OFNODE_GNUM(NODE)=OFNODE_L2GID(I)
!         ENDIF
!      ENDDO 
!      IF (CTR.NE.POROHEX_LFSIZE) THEN
!         WRITE(*,*) "ERROR IN HYPRE_POROHEX"
!         STOP 13
!      ENDIF

! UPDATE OFNODE_GNUM ACROSS ALL PROCESSORS

!      IF (NUMPRC.GT.1 .AND. POROHEX_GFSIZE .GT. 0) THEN
!         OFNODE_GNUM_TMP=0
!         CALL MPI_ALLREDUCE(OFNODE_GNUM,OFNODE_GNUM_TMP,POROHEX_GFSIZE,
!     &                      MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
!   
!         OFNODE_GNUM=OFNODE_GNUM_TMP
!      ENDIF


! SET MATRIX VALUES FROM GSTIFF
      ICTR=1
      JCTR=1
      DO I = 1,NODE_TL
         IF (ACTIVE_NODE(I).EQ.1) THEN
            DO J = 1,3
               LROW=NODE_ST_DOF(I)+J-1
               IROW=NODE_ST_GDOF(I)+J-1
               rowptr(ICTR) = JCTR-1
               ICTR=ICTR+1
               COL1=GSTIFF_ROW_INDEX(LROW)
               COL2=GSTIFF_ROW_INDEX(LROW+1)-1
               DO K=COL1,COL2
                  NODE=GSTIFF_COL_INDEX(K)/3+1
                  DIR=MOD(GSTIFF_COL_INDEX(K),3)
                  IF (DIR.EQ.0) THEN
                     NODE=NODE-1
                     DIR = 3
                  ENDIF
                  COL3=NODE_ST_GDOF(NODE)+DIR-1
                  colind(JCTR)=COL3-1
                  nzval(JCTR)=GSTIFF(K)
                  JCTR=JCTR+1
               ENDDO
            ENDDO
         ENDIF
      ENDDO
      nnz_loc = JCTR-1
      rowptr(ICTR) = nnz_loc

! Create the distributed compressed row matrix pointed to by the F90
! handle A
      call f_dCreate_CompRowLoc_Mat_dist(A, m, n, nnz_loc, m_loc, &
        fst_row, nzval, colind, rowptr, SLU_NR_loc, SLU_D, SLU_GE)

! SET RHS VALUES FROM RESIDUE
      call get_CompRowLoc_Matrix(A, nrow_loc=ldb)
      TL_LOAD=0.D0    ! USED AS TEMP SPACE FOR RHS IN HYPRE
      ICTR=0
      DO I = 1,NODE_TL
         IF (ACTIVE_NODE(I).EQ.1) THEN
            ICTR=ICTR+1
            TL_LOAD(((ICTR-1)*3+1):(ICTR*3)) = &
               RESIDUE(NODE_ST_DOF(I):(NODE_ST_DOF(I)+2))
         ENDIF
      ENDDO
      ldb4 = ldb

! Set the default input options
      call f_set_default_options(options)

! Modify one or more options
      call set_superlu_options(options,ColPerm=NATURAL)
      call set_superlu_options(options,RowPerm=NOROWPERM)
      call set_superlu_options(options,PrintStat=NO)

! Initialize ScalePermstruct and LUstruct
      call get_SuperMatrix(A,nrow=m,ncol=n)
      call f_ScalePermstructInit(m, n, ScalePermstruct)
      call f_LUstructInit(m, n, LUstruct)

! Initialize the statistics variables
      call f_PStatInit(stat)

! Call the linear equation solver
      call f_pdgssvx(options, A, ScalePermstruct, TL_LOAD, ldb4, nrhs, &
                     grid, LUstruct, SOLVEstruct, berr, stat, info)

!      if (myprc.eq.0) then
!      if (info == 0 .and. iam == 1) then
!         write (*,*) 'Backward error: ', berr
!      else
!         write(*,*) 'INFO from f_pdgssvx = ', info
!      endif
!      endif

! STORE SOLUTION TO RESIDUE, THEN COPY BACK TO N_EDISP AND N_OFNODE_DISP in 
! IPARS
      RESIDUE = TL_LOAD

!      OFNODE_DISP=0.D0
!      IF (POROHEX_LFALLSIZE.GT.0) THEN
!         DO I=1,POROHEX_LFALLSIZE
!            IF (OFNODE_KEYOUT(I).EQ.1) THEN
!               CTR=CTR+1
!               NODE=POROHEX_IFLOWER+CTR-POROHEX_LSIZE-1 
!               OFNODE_DISP(1,NODE)=RESIDUE((CTR-1)*3+1) 
!               OFNODE_DISP(2,NODE)=RESIDUE((CTR-1)*3+2)                 
!               OFNODE_DISP(3,NODE)=RESIDUE((CTR-1)*3+3)
!            ENDIF
!         ENDDO 
!      ENDIF
!      IF (CTR.NE.(POROHEX_LSIZE+POROHEX_LFSIZE)) THEN
!         WRITE(*,*) "ERROR IN HYPRE_POROHEX"
!         STOP 13
!      ENDIF
!
!! UPDATE OFNODE_DISP ACROSS ALL PROCESSORS
!      IF (NUMPRC.GT.1 .AND. POROHEX_GFSIZE .GT. 0) THEN
!         OFNODE_DISP_TMP = 0.D0
!         BUFSIZE=3*POROHEX_GFSIZE
!         CALL MPI_ALLREDUCE(OFNODE_DISP,OFNODE_DISP_TMP,BUFSIZE,
!     &                      MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)
!   
!         OFNODE_DISP=OFNODE_DISP_TMP
!      ENDIF

! Deallocate the storage allocated by SuperLU_DIST
      call f_PStatFree(stat)
      call f_Destroy_SuperMat_Store_dist(A)
      call f_ScalePermstructFree(ScalePermstruct)
      call f_Destroy_LU(n, grid, LUstruct)
      call f_LUstructFree(LUstruct)
      call get_superlu_options(options, SolveInitialized=init)
      if (init == YES) then
         call f_dSolveFinalize(options, SOLVEstruct)
      endif

! Release the SuperLU process grid
100   call f_superlu_gridexit(grid)

! Deallocate the C structures pointed to by the Fortran handles
      call f_destroy_gridinfo_handle(grid)
      call f_destroy_options_handle(options)
      call f_destroy_ScalePerm_handle(ScalePermstruct)
      call f_destroy_LUstruct_handle(LUstruct)
      call f_destroy_SOLVEstruct_handle(SOLVEstruct)
      call f_destroy_SuperMatrix_handle(A)
      call f_destroy_SuperLUStat_handle(stat)

      deallocate(colind,rowptr,nzval)

      IF (MYPRC.EQ.0) write(*,*)'Leaving SUPERLU_POROHEX'

      END
