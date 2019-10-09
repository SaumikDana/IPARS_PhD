
      PROGRAM GENHETEROGENEOUS
      IMPLICIT NONE

      INTEGER NX,NY,NZ,I,J,K
      REAL*8 FLCOMP,MAXV,TEMP,MODULV,BIOTV,BULKV,NUM,DEN,INITPOR
      REAL*8 POISS,MINV
      PARAMETER (NX=20,NY=100,NZ=100)
      PARAMETER (FLCOMP = 2.089E-06)
      PARAMETER (INITPOR = 0.2D0)
      PARAMETER (POISS = 0.2D0)
      REAL*8 MODUL(NX,NY,NZ)
      REAL*8 BIOTA(NX,NY,NZ)
      REAL*8 RANDOM

      OPEN(UNIT=1, FILE='heterogeneous.dat',STATUS='UNKNOWN')

      MAXV = 0.D0
      MINV = 1.D0
      DO 100 K = 1, NZ
         DO 100 J = 1, NY
            DO 100 I = 1, NX
! SAUMIK - YOUNG'S MODULUS VARYING FROM 0.009 GPA TO 9 GPA
!          BULK MODULUS OF SOLID GRAINS BEING 15 GPA
!          ALL IN PSI      
               MODUL(I,J,K) = 1305+1304034*RANDOM()
               MODULV = MODUL(I,J,K)
               BULKV = MODULV/(3.D0*(1.D0-2.D0*POISS))
               BIOTA(I,J,K) = 1.D0 - BULKV/2175566
               BIOTV = BIOTA(I,J,K)
               NUM = BIOTV*BIOTV               
               DEN = BULKV*INITPOR*FLCOMP+BIOTV-INITPOR+INITPOR*BIOTV
               TEMP = NUM/DEN
               IF(TEMP.GT.MAXV) MAXV = TEMP
               IF(TEMP.LT.MINV) MINV = TEMP 
 100  CONTINUE

      WRITE(*,*)"THEORETICAL CONSTANT IS", MAXV
      WRITE(*,*)"MIN IS", MINV

      WRITE(1,15) 'MODULUSFLOW2()='
      WRITE(1,20) ((((MODUL(I,J,K)),I=1,NX),J=1,NY),K=1,NZ)
      WRITE(1,*)
      WRITE(1,*) 'BIOTAFLOW2()='
      WRITE(1,21) ((((BIOTA(I,J,K)),I=1,NX),J=1,NY),K=1,NZ)

 15   FORMAT(A15)
 20   FORMAT(6(F10.2,3X))
 21   FORMAT(6(F10.4,3X))
      CLOSE(1)

      END

C=======================================================================
      REAL*8 FUNCTION RANDOM()
C=======================================================================
      IMPLICIT NONE
C      
C REAL*8 RANDOM NUMBER GENERATOR (RESULT UNIFORMLY BETWEEN 0 AND 1)
C      
      INTEGER SEED
      DATA SEED / 100 /
C      
      SEED = 2045*SEED + 1
      SEED = SEED - (SEED/1048576)*1048576
      RANDOM = REAL(SEED + 1) / 1048577.D0
C
      RETURN
      END



      
