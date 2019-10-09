
      PROGRAM GENMESH
      IMPLICIT NONE

      INTEGER NX,NY,NZ,I,J,K
      REAL*8 X,Y,Z,LX,LY,LZ,HX,HY,HZ

      PARAMETER (NX=5,NY=60,NZ=60)

      REAL*8 XC(NX+1,NY+1,NZ+1)
      REAL*8 YC(NX+1,NY+1,NZ+1)
      REAL*8 ZC(NX+1,NY+1,NZ+1)

      OPEN(UNIT=1, FILE='mesh60.dat',STATUS='UNKNOWN')

      LX = 200.D0
      LY = 3000.D0
      LZ = 3000.D0

      HX = LX/NX
!      HX = LX/(NX-2)
      HY = LY/NY
      HZ = LZ/NZ

      DO K = 1, NZ+1
         Z = (K-1)*HZ
         DO J = 1, NY+1
            Y = (J-1)*HY
            DO I = 1, NX+1
!               IF(I.LE.2) THEN
!                 X = (I-1)*800.D0
!               ELSEIF(I.GT.2.AND.I.LE.NX) THEN
!                 X = 800.D0 + (I-2)*HX
!               ELSE
!                 X = 1000.D0 + (I-NX)*200.D0
!               ENDIF
               X = 800.D0 + (I-1)*HX
               XC(I,J,K) = X
               YC(I,J,K) = Y
               ZC(I,J,K) = Z
            ENDDO
         ENDDO
      ENDDO

      WRITE(1,10)'NX(2)=',NX,'','NY(2)=',NY,'','NZ(2)=',NZ
 10   FORMAT(A,I4,A5,A,I4,A5,A,I4)

      WRITE(1,15) 'XC2()='
      WRITE(1,20) ((((XC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,*) 'YC2()='
      WRITE(1,20) ((((YC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,*) 'ZC2()='
      WRITE(1,20) ((((ZC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)

 15   FORMAT(A9)
 20   FORMAT(4(E15.7,3X))
      CLOSE(1)

      END

     
