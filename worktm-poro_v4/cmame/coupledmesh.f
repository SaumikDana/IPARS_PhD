
      PROGRAM GENMESH
      IMPLICIT NONE

      INTEGER NX,NY,NZ,I,J,K
      REAL*8 HX,HY,HZ,X,Y,Z,LX,LY,LZ

      PARAMETER (NX=5,NY=25,NZ=25)

      REAL*8 XC(NX+1,NY+1,NZ+1)
      REAL*8 YC(NX+1,NY+1,NZ+1)
      REAL*8 ZC(NX+1,NY+1,NZ+1)

      OPEN(UNIT=1, FILE='mesh1.dat',STATUS='UNKNOWN')

      LX = 100.
      LY = 5000.
      LZ = 5000.

      HX = LX/NX
      HY = LY/NY
      HZ = LZ/NZ

      DO 100 K = 1, NZ+1
      Z = (K-1)*HZ
      DO 100 J = 1, NY+1
      Y = (J-1)*HY
      DO 100 I = 1, NX+1
      X = (I-1)*HX

      XC(I,J,K) = X
      YC(I,J,K) = Y
      ZC(I,J,K) = Z
      
 100  CONTINUE

      WRITE(1,10)'NX(1)=',NX,'','NY(1)=',NY,'','NZ(1)=',NZ
 10   FORMAT(A,I4,A5,A,I4,A5,A,I4)

      WRITE(1,15) 'XC1()='
      WRITE(1,20) ((((XC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,*) 'YC1()='
      WRITE(1,20) ((((YC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,*) 'ZC1()='
      WRITE(1,20) ((((ZC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)

 15   FORMAT(A9)
 20   FORMAT(6(F10.4,3X))
      CLOSE(1)

      END

     
