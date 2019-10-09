
      PROGRAM GENMESH
      IMPLICIT NONE

      INTEGER NX,NY,NZ,I,J,K
      REAL*8 X,Y,Z,LX,LY,LZ,HX,HY,HZ1,HZ2

! saumik - gravity is +z direction
      PARAMETER (NX=18,NY=6,NZ=11)
      ! 500 ft * 1500 ft * 500 ft for overburden 
      ! overburden --> z=0 ft to 3500 ft

      ! 500 ft * 1500 ft * 500 ft for near-reservoir
      ! near-reservoir --> z=3500 ft to 5500 ft

      REAL*8 XC(NX+1,NY+1,NZ+1)
      REAL*8 YC(NX+1,NY+1,NZ+1)
      REAL*8 ZC(NX+1,NY+1,NZ+1)

      OPEN(UNIT=1, FILE='bigmesh.dat',STATUS='UNKNOWN')

      LX = 9000.0D0
      LY = 9000.0D0

      HX = LX/NX
      HY = LY/NY
      HZ1 = 500.0D0
      HZ2 = 500.0D0

      DO K = 1, NZ+1
! overburden
         IF(K.LE.8) THEN
           Z = (K-1)*HZ1
! near reservoir
         ELSE
           Z = 3500.D0 + (K-8)*HZ2
         ENDIF
         DO J = 1, NY+1
            Y = (J-1)*HY
            DO I = 1, NX+1
               X = (I-1)*HX
               XC(I,J,K) = X
               YC(I,J,K) = Y
               ZC(I,J,K) = Z
            ENDDO
         ENDDO
      ENDDO

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
 20   FORMAT(4(E15.7,3X))
      CLOSE(1)

      END

     
