! Utility program to convert ascii to binary and binary to ascii
! created by Ben Ganis 5/24/17.

      PROGRAM binary_converter
      IMPLICIT NONE
      INTEGER :: I,J,K,L,NX,NY,NZ,N4,N,IERR
      DOUBLE PRECISION, ALLOCATABLE :: BUF(:,:,:,:)
      CHARACTER*80 :: INFILE, OUTFILE, INPUTNAME
      INTEGER MODE

      WRITE(*,*)'-------------------------------------'
      WRITE(*,*)'Binary input conversion utility      '
      WRITE(*,*)'-------------------------------------'
      WRITE(*,*)'Type 1 to convert ascii to binary'
      WRITE(*,*)'Type 2 to convert binary to ascii'
      WRITE(*,'(a,$)')'Enter mode: '
      READ(*,*) MODE
      IF ((MODE.NE.1).AND.(MODE.NE.2)) STOP 'Error'
      WRITE(*,'(a,$)')'Enter input filename without suffix: '
      READ(*,*) INPUTNAME
      WRITE(*,'(a,$)')'Enter I,J,K,L dimensions: '
      READ(*,*) NX,NY,NZ,N4
      N = NX*NY*NZ*N4

      ALLOCATE(BUF(NX,NY,NZ,N4),STAT=IERR)
      IF (IERR.NE.0) STOP 'Could not allocate BUF'

      IF (MODE.EQ.1) THEN

        INFILE=TRIM(INPUTNAME)//'.dat'
        WRITE(*,*)'Reading: ',TRIM(INFILE)
        OPEN(unit=10,file=TRIM(INFILE),status='old')
        READ(10,*) ! comment
        READ(10,*) ((((BUF(I,J,K,L),I=1,NX),J=1,NY),K=1,NZ),L=1,N4)
        CLOSE(10)      

        OUTFILE=TRIM(INPUTNAME)//'.bin'
        WRITE(*,*)'Writing: ',TRIM(OUTFILE)
        OPEN(unit=10,file=TRIM(OUTFILE),form='binary',status='unknown')
        WRITE(10) BUF
        CLOSE(10)

      ELSEIF (MODE.EQ.2) THEN

        INFILE=TRIM(INPUTNAME)//'.bin'
        WRITE(*,*)'Reading: ',TRIM(INFILE)
        OPEN(unit=10,file=TRIM(INFILE),form='binary', &
          access='sequential',status='old')
        READ(10) BUF
        CLOSE(10)      

        OUTFILE=TRIM(INPUTNAME)//'.dat'
        WRITE(*,*)'Writing: ',TRIM(OUTFILE)
        OPEN(unit=10,file=TRIM(OUTFILE), &
             status='unknown')
        WRITE(10,'(2A)')TRIM(INPUTNAME),'1()='
        WRITE(10,'(1P,6E16.9)') BUF
        CLOSE(10)

      ENDIF

      WRITE(*,*)'Done!'
      DEALLOCATE(BUF)

      END PROGRAM binary_converter
