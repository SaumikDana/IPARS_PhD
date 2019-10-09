      PROGRAM PERMREAD
      IMPLICIT NONE
      CHARACTER*20 FNAME
      INTEGER I,J,K,NX,NY,NZ,NF1,NF2
      PARAMETER (NF1=1,NF2=2,NX=20, NY=17, NZ=6, FNAME='Yperm.dat')
      REAL*8 YP(NX,NY,NZ)

      OPEN(UNIT=NF1,FILE=FNAME,STATUS='old')
      READ(NF1,*) (((YP(I,J,K),K=1,NZ),J=1,NY),I=1,NX)
      CLOSE(NF1)

      DO I=1,NX
         DO J=1,NY
            DO K=1,NZ
               YP(I,J,K)=0.1D0*YP(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      OPEN(UNIT=NF2,FILE='Xperm.dat',STATUS='new')
      WRITE(NF2,*) (((YP(I,J,K),K=1,NZ),J=1,NY),I=1,NX)
      CLOSE(NF2)
     
      STOP
      END 
