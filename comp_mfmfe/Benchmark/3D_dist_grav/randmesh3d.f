
      program genmesh
      IMPLICIT NONE

      INTEGER NX,NY,NZ,MAX,I,J,K
      REAL*8 HX,HY,HZ,X,Y,Z ,XX,YY,ZZ,LX,LY,LZ

      PARAMETER (MAX=300)

      REAL*8 XC(MAX+1,MAX+1,MAX+1),YC(MAX+1,MAX+1,MAX+1),
     &     ZC(MAX+1,MAX+1,MAX+1)
      REAL*8 RANDOM
      REAL*8 FAC
c
cgxue In h perturbed mesh:  
c     FAC = 0.6D0 convergence failed. may generate non-convex elements
c     FAC = 0.5D0 Picture is used in non-symmetric MFMFE paper
c                 AMG fails on 1/h=128, the matrix may be close to indefinite
c                 GMRES with AMG converges
c     FAC = 0.4D0 1/h=128 

      FAC = 0.2D0


      OPEN(UNIT=1, FILE='mesh.dat',STATUS='UNKNOWN')
      
      LX = 1000.d0
      LY = 1000.d0
      LZ = 200.d0
     
      
      WRITE(*,*)'INPUT MESH SIZE NX='
      READ (*,*) NX
      
      WRITE(*,*)'INPUT MESH SIZE NY='
      READ (*,*) NY
      
      WRITE(*,*)'INPUT MESH SIZE NZ='
      READ (*,*) NZ

      IF (NX*NY*NZ.GT.MAX**3)THEN
         WRITE(*,*) 'PROBLEM SIZE IS TOO BIG'
         GOTO 9999
      ENDIF

      HX = LX/NX
      HY = LY/NY
      HZ = LZ/NZ

      DO 100 K = 1, NZ+1
      Z=(K-1)*HZ
      DO 100 J = 1, NY+1
      Y = (J-1)*HY
      DO 100 I = 1, NX+1
      X = (I-1)*HX
      
      XX = X
      YY = Y
      ZZ = Z


      IF ((I.NE.1).AND.(I.NE.NX+1)) XX = X + (RANDOM()-0.5D0)*FAC*HX
      IF ((J.NE.1).AND.(J.NE.NY+1)) YY = Y + (RANDOM()-0.5D0)*FAC*HY
      IF ((K.NE.1).AND.(K.NE.NZ+1)) ZZ = Z + (RANDOM()-0.5D0)*FAC*HZ

      XC(I,J,K) = XX
      YC(I,J,K) = YY
      ZC(I,J,K) = ZZ
            
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
c 20   FORMAT(6(F15.10,3X))
 20   FORMAT(6(F19.14,3X))



      CLOSE(1)

 9999 STOP

      END


c=======================================================================
      real*8 function random()
c=======================================================================
      implicit none
c
c real*8 random number generator (result uniformly between 0 and 1)
c
      integer seed
      data seed / 100 /
c
      seed = 2045*seed + 1
      seed = seed - (seed/1048576)*1048576
      random = real(seed + 1) / 1048577.d0
c
      return
      end

