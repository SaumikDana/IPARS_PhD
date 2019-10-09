
      program genmesh
      IMPLICIT NONE

      INTEGER NX,NX1,NX2,NY,NZ,MAX,I,J,K
      REAL*8 HX,HX1,HX2,HY,HZ,X,Y,Z,XX,YY,ZZ,LX,LX1,LX2,LY,LZ,
     &       hatY,hatZ,pi
      data pi/3.14159265358979323846d0/

      PARAMETER (MAX=300)
      PARAMETER (NX=40,NY=40,NZ=1)

      REAL*8 XC(NX+1,NY+1,NZ+1)
      REAL*8 YC(NX+1,NY+1,NZ+1)
      REAL*8 ZC(NX+1,NY+1,NZ+1)
      INTEGER QUADTYPE(NX,NY,NZ)
      INTEGER ELEMCOUNT,ELEMCOUNTER
      REAL*8, ALLOCATABLE::MIDPOINT(:,:)

      REAL*8 FAC
      REAL*8 RANDOM
      INTEGER QTYPE

      FAC = 0.35d0     

!      OPEN(UNIT=1, FILE='mesh1.dat',STATUS='UNKNOWN')
!      OPEN(UNIT=2, FILE='quadtype.dat',STATUS='UNKNOWN')
      OPEN(UNIT=3, FILE='midpoint.dat',STATUS='UNKNOWN')

      LX = 328.084
      LY = 32.8084
      LZ = 3.28084

      ELEMCOUNT = NY*NZ
      ELEMCOUNTER = 0

      ALLOCATE(MIDPOINT(ELEMCOUNT,2))

      IF (NX*NY*NZ.GT.MAX**3)THEN
         WRITE(*,*) 'PROBLEM SIZE IS TOO BIG'
         GOTO 9999
      ENDIF

      HX = LX/NX
      HY = LY/NY
      HZ = LZ/NZ

      DO 100 K = 1, NZ+1
!         Z = 2400.D0 + (K-1)*HZ
         Z = (K-1)*HZ

         DO 100 J = 1, NY+1
!            Y = 2400.D0 + (J-1)*HY
            Y = (J-1)*HY

            IF((J.LE.NY).AND.(K.LE.NZ))THEN
               ELEMCOUNTER = ELEMCOUNTER + 1
               MIDPOINT(ELEMCOUNTER,1) = Y + HY/2.D0
               MIDPOINT(ELEMCOUNTER,2) = Z + HZ/2.D0
            ENDIF      
            DO 100 I = 1, NX+1
!               X = 400.D0 + (I-1)*HX
               X = (I-1)*HX

              XX = X
              YY = Y
              ZZ = Z

              goto 1
c
c subdomain (0,LY/2) * (0,LZ/2)
c

!      if ((j.gt.1).and.(j.lt.(NY/2+1)).and.(k.gt.1).and.(k.lt.(NZ/2+1)))
!     &     then
!         hatY = Y/(LY/2.0d0)
!         hatZ = Z/(LZ/2.0d0)
!         YY = LY/2.0d0*(hatY + 0.06d0*dsin(2.0d0*pi*hatY)
!     &        *dsin(2.0d0*pi*hatZ))
!         ZZ = LZ/2.0d0*(hatZ - 0.05d0*dsin(2.0d0*pi*hatY)
!     &        *dsin(2.0d0*pi*hatZ))
!      endif

c
c subdomain (LY/2, LY) * (0, LZ/2)
c

!      if ((j.gt.(NY/2+1)).and.(j.lt.(NY+1))
!     &     .and.(k.gt.1).and.(k.lt.(NZ/2+1))) then
!         YY = Y + (RANDOM() -0.5D0)*FAC*HY
!         ZZ = Z + (RANDOM() -0.5D0)*FAC*HZ
!      endif

c
c subdomain (0, LY/2) * (LZ/2,LZ)
c

!      if ((j.gt.1).and.(j.lt.(NY/2+1))
!     &     .and.(k.gt.(NZ/2+1)).and.(k.lt.(NZ+1))) then
!         YY = Y + (RANDOM() -0.5D0)*FAC*HY
!         ZZ = Z + (RANDOM() -0.5D0)*FAC*HZ
!      endif

   1  continue

      XC(I,J,K) = XX
      YC(I,J,K) = YY
      ZC(I,J,K) = ZZ
      
 100  CONTINUE

!      WRITE(1,10)'NX(2)=',NX,'','NY(2)=',NY,'','NZ(2)=',NZ
! 10   FORMAT(A,I4,A5,A,I4,A5,A,I4)

!      WRITE(1,15) 'XC2()='
!      WRITE(1,20) ((((XC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
!      WRITE(1,*)
!      WRITE(1,*) 'YC2()='
!      WRITE(1,20) ((((YC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
!      WRITE(1,*)
!      WRITE(1,*) 'ZC2()='
!      WRITE(1,20) ((((ZC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)

! 15   FORMAT(A9)
! 20   FORMAT(6(F10.4,3X))
!      CLOSE(1)

!      DO 200 K = 1, NZ
!      DO 200 J = 1, NY
!      DO 200 I = 1, NX

!      QTYPE = 1

c
c subdomain (LY/2, LY) * (0, LZ/2)
c

!      if ((j.GE.(NY/2+1)).and.(j.LE.NY)
!     &     .and.(k.GE.1).and.(k.LE.(NZ/2))) then
!         
!         QTYPE = 2
!      endif

c
c subdomain (0, LY/2) * (LZ/2,LZ)
c

!      if ((j.GE.1).and.(j.LE.(NY/2))
!     &     .and.(k.GE.(NZ/2+1)).and.(k.LE.NZ)) then
!         
!         QTYPE = 2
!         
!      endif

!      QUADTYPE(I,J,K) = QTYPE

! 200  CONTINUE

!      WRITE(2,*)'MPFAQU2()='
!      WRITE(2,25) ((((QUADTYPE(I,J,K)),I=1,NX),J=1,NY),K=1,NZ)
!25    FORMAT(8(I4,3X))
      
!      close(2)

      WRITE(3,30) (MIDPOINT(I,1),MIDPOINT(I,2),I=1,ELEMCOUNT)
30    FORMAT (2(F10.4,3X))
      DEALLOCATE (MIDPOINT)

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



      
