C
C Utility functions used in MFMFE
C
C

c======================================================================
      INTEGER FUNCTION MAPPING(IND)
c======================================================================
      IMPLICIT NONE
C
      INTEGER IND
C
      INTEGER SOLVER
C
      SOLVER = 2 ! 1: DIRECT SOLVER      2: ITERATIVE SOLVER

C OMIT SDIRECT      SOLVER = 1
C OMIT BANDDIRECT      SOLVER = 1

      IF (SOLVER.EQ.2) THEN

      IF(IND.EQ.-13)  MAPPING=6
      IF(IND.EQ.-12) MAPPING = 0
      IF(IND.EQ.-11) MAPPING= 7
      IF(IND.EQ.-10) MAPPING =-2
      IF(IND.EQ.-9) MAPPING= -8
      IF(IND.EQ.-8) MAPPING= -1
      IF(IND.EQ.-7) MAPPING= 8
      IF(IND.EQ.-6) MAPPING= 1
      IF(IND.EQ.-5) MAPPING= 9
      IF(IND.EQ.-4 ) MAPPING=  -6
      IF(IND.EQ.-3 ) MAPPING=  -10
      IF(IND.EQ.-2 ) MAPPING=  -5
      IF(IND.EQ.-1 ) MAPPING=  -12
      IF(IND.EQ.0 ) MAPPING=  -13
      IF(IND.EQ.1) MAPPING=-11
      IF(IND.EQ.2) MAPPING= -4
      IF(IND.EQ.3) MAPPING= -9
      IF(IND.EQ.4) MAPPING= -3
      IF(IND.EQ.5) MAPPING= 10
      IF(IND.EQ.6) MAPPING= 4
      IF(IND.EQ.7) MAPPING= 11
      IF(IND.EQ.8) MAPPING=   2
      IF(IND.EQ.9) MAPPING= -7
      IF(IND.EQ. 10) MAPPING= 3
      IF(IND.EQ. 11) MAPPING= 12
      IF(IND.EQ. 12) MAPPING= 5
      IF(IND.EQ. 13) MAPPING= 13

      ELSEIF (SOLVER.EQ.1) THEN

        MAPPING = IND

      ENDIF

      RETURN
      END


c=======================================================================
      real*8 function VolH(X)
c=======================================================================
      implicit none
c
c Volume of an element
c   Vertices:   Faces:
c     8----7     bottom = 1
c    /    /|     top    = 2
c   5----6 |     left   = 3
c   | 4  | 3     right  = 4
c   |    |/      front  = 5
c   1----2       back   = 6
c

      real*8 X(3,8)
      real*8  v1(3),v2(3),v3(3),v4(3),v5(3),v6(3),v7(3),v8(3)
      integer i
      real*8  f1(3),f2(3),f3(3),f4(3),f5(3),f6(3)
      real*8  vol,tetraVol
c
c Extra vertices (6 = one per face)
c
      do i = 1,3
        v1(i) = X(i,1)
        v2(i) = X(i,2)
        v3(i) = X(i,3)
        v4(i) = X(i,4)
        v5(i) = X(i,5)
        v6(i) = X(i,6)
        v7(i) = X(i,7)
        v8(i) = X(i,8)
      enddo

      do i=1,3
         f1(i) = .25*(v1(i) + v2(i) + v3(i) + v4(i))
         f2(i) = .25*(v5(i) + v6(i) + v7(i) + v8(i))
         f3(i) = .25*(v1(i) + v4(i) + v5(i) + v8(i))
         f4(i) = .25*(v2(i) + v3(i) + v6(i) + v7(i))
         f5(i) = .25*(v1(i) + v2(i) + v5(i) + v6(i))
         f6(i) = .25*(v3(i) + v4(i) + v7(i) + v8(i))
      enddo
c
c Tetrahedral volumes
c
      vol = 0.
c
c edges (12)
c
      vol = vol + tetraVol(v1,v2,f1,f5)
      vol = vol + tetraVol(v2,v3,f1,f4)
      vol = vol + tetraVol(v3,v4,f1,f6)
      vol = vol + tetraVol(v1,v4,f1,f3)
      vol = vol + tetraVol(v1,v5,f3,f5)
      vol = vol + tetraVol(v2,v6,f4,f5)
      vol = vol + tetraVol(v3,v7,f4,f6)
      vol = vol + tetraVol(v4,v8,f3,f6)
      vol = vol + tetraVol(v5,v6,f2,f5)
      vol = vol + tetraVol(v6,v7,f2,f4)
      vol = vol + tetraVol(v7,v8,f2,f6)
      vol = vol + tetraVol(v5,v8,f2,f3)
c
c vertices (8)
c
      vol = vol + tetraVol(v1,f1,f3,f5)
      vol = vol + tetraVol(v2,f1,f4,f5)
      vol = vol + tetraVol(v3,f1,f4,f6)
      vol = vol + tetraVol(v4,f1,f3,f6)
      vol = vol + tetraVol(v5,f2,f3,f5)
      vol = vol + tetraVol(v6,f2,f4,f5)
      vol = vol + tetraVol(v7,f2,f4,f6)
      vol = vol + tetraVol(v8,f2,f3,f6)
c
c center (4)
c
      vol = vol + tetraVol(f1,f2,f3,f5)
      vol = vol + tetraVol(f1,f2,f3,f6)
      vol = vol + tetraVol(f1,f2,f4,f5)
      vol = vol + tetraVol(f1,f2,f4,f6)
c
      VolH = vol

      return
      end
c=======================================================================
      real*8 function tetraVol(v1,v2,v3,v4)
c=======================================================================
      implicit none
c
c Volume of a tetrahedra
c
      real*8 v1(3),v2(3),v3(3),v4(3)
      real*8 det
      real*8 a1,a2,a3,b1,b2,b3,c1,c2,c3
      det(a1,a2,a3,b1,b2,b3,c1,c2,c3)
     &     = (a1*(b2*c3-c2*b3)-b1*(a2*c3-c2*a3)+c1*(a2*b3-b2*a3))
c
      tetraVol = abs( det(
     &     v1(1) - v4(1), v2(1) - v4(1), v3(1) - v4(1),
     &     v1(2) - v4(2), v2(2) - v4(2), v3(2) - v4(2),
     &     v1(3) - v4(3), v2(3) - v4(3), v3(3) - v4(3) ) ) / 6.
c
      return
      end

c=======================================================================
      logical function ELEIN(LI,LJ,LK,IDIM,JDIM,KDIM)
c=======================================================================
      implicit none
C
C check element (LI LJ LK) belongs to current processor including
c ghost layers such that keyout(LI,LJ,LK) defined
C
      INTEGER LI,LJ,LK,IDIM,JDIM,KDIM
C
      ELEIN = .FALSE.

      IF (((LI.GE.1).AND.(LI.LE.IDIM)).AND.
     &    ((LJ.GE.1).AND.(LJ.LE.JDIM)).AND.
     &   ((LK.GE.1).AND.(LK.LE.KDIM))) THEN
        ELEIN = .TRUE.
      ENDIF


      return
      end


c======================================================================
      SUBROUTINE PRINT1D(A,ROW)
c======================================================================
      IMPLICIT NONE
C
      INTEGER ROW
      REAL*8 A(ROW)
C
      INTEGER I

      DO I = 1,ROW
        WRITE(*,10) I, A(I)
      ENDDO

 10    FORMAT(I3,E15.3)


      RETURN
      END


c======================================================================
      SUBROUTINE PRINT1DI4(A,ROW)
c======================================================================
      IMPLICIT NONE
C
      INTEGER ROW
      INTEGER A(ROW)
C
      INTEGER I

      DO I = 1,ROW
        WRITE(*,10) I, A(I)
      ENDDO

 10    FORMAT(I3, I6)


      RETURN
      END



c======================================================================
      SUBROUTINE PRINT2D(A,ROW,COL)
c======================================================================
      IMPLICIT NONE
C
      INTEGER ROW,COL
      REAL*8 A(ROW,COL)
C
      INTEGER I,J

       DO I=1,ROW
         WRITE(0,'(1000(1x,E10.3))')(A(I,J),J=1,COL)
C         WRITE(*,'(1000(1x,E14.7))')(A(I,J),J=1,COL)
       ENDDO

      RETURN
      END



c======================================================================
      SUBROUTINE PRINT3D(A,R1,R2,R3)
c======================================================================
      IMPLICIT NONE
C
      INTEGER R1,R2,R3
      REAL*8 A(R1,R2,R3)
C
      INTEGER I,J,K
       DO K = 1, R3
         WRITE(*,*)'HIGH DIM:', K
         CALL PRINT2D(A(1,1,K),R1,R2)
       ENDDO

      RETURN
      END




c======================================================================
      SUBROUTINE PRINTSYSARY(NERR)
c======================================================================
      IMPLICIT NONE
C
      include 'blkary.h'
C
      INTEGER NERR
C
      INTEGER CPRINT(4)
      LOGICAL ONCEONLY
      EXTERNAL CWCPRINT
      DATA CPRINT/4*0/, ONCEONLY/.TRUE./
      IF (ONCEONLY) THEN
         ONCEONLY = .FALSE.
         CPRINT(1) = 3
         CPRINT(2) = N_XC
         CPRINT(3) = N_YC
         CPRINT(4) = N_ZC
      ENDIF

      call callwork(CWCPRINT,CPRINT)

      RETURN
      END



c======================================================================
      subroutine CWCprint(idim,jdim,kdim,ldim,il1,il2,jl1v,jl2v,
     &     kl1,kl2,keyout,nblk,xc,yc,zc)
c======================================================================
      IMPLICIT NONE
C
      include 'control.h'
c
      integer idim,jdim,kdim,ldim,il1,il2,jl1v(kdim),jl2v(kdim),
     &     kl1,kl2,keyout(idim,jdim,kdim),nblk
C
      REAL*8 xc(IDIM+1,JDIM+1,KDIM+1),yc(IDIM+1,JDIM+1,KDIM+1),
     &       zc(IDIM+1,JDIM+1,KDIM+1)
      integer i,j,k,jl1,jl2
c
c loop over all vertices (i,j,k)
c

      write(0,*)' '
      do 200 k = kl1,kl2+1
      if (k.lt.kl2+1)then
         jl1=jl1v(k)
         jl2=jl2v(k)
      else
         jl1=jl1v(kl2)
         jl2=jl2v(kl2)
      endif
      do 200 j = jl1,jl2+1
      do 200 i = il1,il2+1

  200 continue


      do 100 k = 1,kdim+1
      do 100 j = 1,jdim+1
      do 100 i = 1,idim+1
         write(0,99)'myprc I J K xc yc zc:',myprc,I,J,K,
     &             xc(i,j,k),yc(i,j,k),zc(i,j,k)
   99 format(A20,2X,I4,2X,3(I4),2X,3(E12.3))
  100 continue

      RETURN
      END




c======================================================================
      subroutine CWEPRINT(idim,jdim,kdim,ldim,il1,il2,jl1v,jl2v,
     &     kl1,kl2,keyout,nblk,ary)
c======================================================================
      IMPLICIT NONE
C
      include 'control.h'
c
      integer idim,jdim,kdim,ldim,il1,il2,jl1v(kdim),jl2v(kdim),
     &     kl1,kl2,keyout(idim,jdim,kdim),nblk
C
      real*8 ary(idim,jdim,kdim,-13:13)

C
      integer i,j,k,jl1,jl2
      integer row,col,pt,num
C
      if (myprc.eq.0) then
         OPEN(UNIT=1, FILE='cof.0.dat',STATUS='UNKNOWN')
      endif
      if (myprc.eq.1) then
         OPEN(UNIT=2, FILE='cof.1.dat',STATUS='UNKNOWN')
      endif

C loop over elements


      IF (numprc.eq.1) then
         num = 0
      else
          num =myprc + 1
      endif

      do 100 k=kl1,kl2
      do 100 j=jl1v(k),jl2v(k)
      do 100 i=il1,il2
      if (keyout(i,j,k).gt.0) then
c cof
         write(num,999)'myprc EI EJ EK,cof:',
     &      myprc,I,J,K,
     &      ary(i,j,k,-13),ary(i,j,k,-12),
     &      ary(i,j,k,-11),ary(i,j,k,-10),
     &      ary(i,j,k,-9),ary(i,j,k,-8),
     &      ary(i,j,k,-7),ary(i,j,k,-6),
     &      ary(i,j,k,-5),ary(i,j,k,-4),
     &      ary(i,j,k,-3),ary(i,j,k,-2),
     &      ary(i,j,k,-1),ary(i,j,k,0),
     &      ary(i,j,k,1),ary(i,j,k,2),
     &      ary(i,j,k,3),ary(i,j,k,4),
     &      ary(i,j,k,5),ary(i,j,k,6),
     &      ary(i,j,k,7),ary(i,j,k,8),
     &      ary(i,j,k,9),ary(i,j,k,10),
     &      ary(i,j,k,11),ary(i,j,k,12),
     &      ary(i,j,k,13)
  999 format(A20,2X,I2,2X,3(I4),2X,27(E12.3))

      endif
  100 continue

      do 200 k = 1,kdim
      do 200 j = 1,jdim
      do 200 i = 1,idim

  200 continue

      return
      end



c======================================================================
      subroutine wrM0(m0,row,col)
c======================================================================
c
      implicit none
c
      integer row,col
      real*8  m0(row,col)
c
      integer i,j
c
      open(57,file='m0.dat')
      write(57,*)
      do i=1,row
        write(57,*) i, (m0(i,j),j=1,col)
      enddo
      close(57)
c
      end
c

c======================================================================
      SUBROUTINE INITARYR8(A,DIM,VAL)
c======================================================================
      IMPLICIT NONE
C
      INTEGER DIM
      REAL*8 A(DIM),VAL
C
      INTEGER I
C
      DO I=1,DIM
        A(I) = VAL
      ENDDO

      RETURN
      END



c======================================================================
      SUBROUTINE INITARYR4(A,DIM,VAL)
c======================================================================
      IMPLICIT NONE
C
      INTEGER DIM
      REAL*4 A(DIM)
      REAL*8 VAL
C
      INTEGER I
C
      DO I=1,DIM
        A(I) = VAL
      ENDDO

      RETURN
      END


c======================================================================
      SUBROUTINE INITARYI4(A,DIM,IVAL)
c======================================================================
      IMPLICIT NONE
C
      INTEGER DIM,A(DIM),IVAL
C
      INTEGER I
C
      DO I=1,DIM
        A(I) = IVAL
      ENDDO

      RETURN
      END


c======================================================================
      SUBROUTINE COPYARYR8(A,B,DIM)
c======================================================================
      IMPLICIT NONE
C
C  COPY A TO B. DIMENSION OF A AND B IS DIM
C
      INTEGER DIM
      REAL*8 A(DIM),B(DIM)
C
      INTEGER I
C
      DO I = 1,DIM
        B(I) = A(I)
      ENDDO

      RETURN
      END



c======================================================================
      SUBROUTINE AddaxToby(a,x,b,y,dim)
c======================================================================
      IMPLICIT NONE
c  y = ax + by
C
      INTEGER DIM
      REAL*8 a,x(dim),b,y(dim)
C
      INTEGER i
C
      DO i = 1,dim
         y(I) = a*x(I) + b*y(i)
      ENDDO

      RETURN
      END


c======================================================================
      subroutine JacMat(X,hatX,JMAT)
c======================================================================
      implicit none

      real*8 X(3,8),hatX(3),JMAT(3,3)
c
      integer row

      do 100 row = 1,3
         JMAT(row,1) = X(row,2)-X(row,1) +
     &        (X(row,3)-X(row,4)-X(row,2)+X(row,1))*hatX(2) +
     &        (X(row,6)-X(row,5)-X(row,2)+X(row,1))*hatX(3) +
     &        (X(row,2)-X(row,1)-X(row,3)+X(row,4)-
     &        X(row,6)+X(row,5)+X(row,7)-X(row,8))*hatX(2)*hatX(3)
 100  continue


      do 200 row = 1,3
         JMAT(row,2) = X(row,4)-X(row,1) +
     &        (X(row,3)-X(row,4)-X(row,2)+X(row,1))*hatX(1) +
     &        (X(row,8)-X(row,5)-X(row,4)+X(row,1))*hatX(3) +
     &        (X(row,2)-X(row,1)-X(row,3)+X(row,4)-
     &        X(row,6)+X(row,5)+X(row,7)-X(row,8))*hatX(1)*hatX(3)
 200  continue


      do 300 row = 1,3
         JMAT(row,3) = X(row,5)-X(row,1) +
     &        (X(row,6)-X(row,5)-X(row,2)+X(row,1))*hatX(1) +
     &        (X(row,8)-X(row,5)-X(row,4)+X(row,1))*hatX(2) +
     &        (X(row,2)-X(row,1)-X(row,3)+X(row,4)-
     &        X(row,6)+X(row,5)+X(row,7)-X(row,8))*hatX(1)*hatX(2)
 300  continue


      return
      end

c======================================================================
      subroutine detMat(mat,det)
c======================================================================
      implicit none
      real*8 mat(3,3),det

      det = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) -
     &     mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) +
     &     mat(1,3)*(mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1))

      return
      end

c======================================================================
      SUBROUTINE ScaMulMat(ALPHA,A,Row,COL)
c======================================================================
      IMPLICIT NONE
C
C A = ALPHA*A
C
      INTEGER ROW,COL
      REAL*8 ALPHA,A(ROW,COL)
C
      INTEGER I,J

      DO 100 J=1,COL
      DO 100 I=1,ROW

        A(I,J)=ALPHA*A(I,J)

 100  CONTINUE

      RETURN
      END


c======================================================================
      SUBROUTINE getEYE(EYE,ROW,COL)
c======================================================================
      IMPLICIT NONE
C
      INTEGER ROW,COL
c      REAL*8 EYE(ROW,COL)
      DOUBLE PRECISION EYE(ROW,COL)
C
      INTEGER J

      EYE = 0.D0
      DO J = 1,COL
         EYE(J,J) = 1.D0
      ENDDO

      RETURN
      END

c======================================================================
      real*8 function Tri_AREA(p1,p2,p3)
c======================================================================
      implicit none
C
c     area of triangle  1/2 |a*b| = 1/2|a||b|sin(theta),
c     a and b are edge vectors
c     a = (a1,a2,a3)  b = (b1,b2,b3)
c     |a*b|^2 = |a|^2|b|^2 - (a dot b)^2
c
c
c          p3
c         /  \
c        /    \
c       p1 --- p2
C
C
      real*8 p1(3),p2(3),p3(3)
C
      integer dim
      real*8  a(3),b(3)
C
      do 100 dim = 1,3
         a(dim) = p3(dim) - p1(dim)
         b(dim) = p2(dim) - p1(dim)
 100  continue

c  triagle p3-p1-p2
      Tri_AREA = 0.5d0*DSQRT((a(1)**2+a(2)**2+a(3)**2)*
     &     (b(1)**2+b(2)**2+b(3)**2)-
     &     (a(1)*b(1)+a(2)*b(2)+a(3)*b(3))**2)

      return
      end


c======================================================================
      subroutine MatTranTo(A,Atran,nrow,ncol)
c======================================================================
      implicit none
c
c Atran = A^T
c
      integer nrow,ncol
      real*8 A(ncol,nrow),Atran(nrow,ncol)
c
      integer row,col
c
      do row = 1,nrow
      do col = 1,ncol
         Atran(row,col) = A(col,row)
      enddo
      enddo

      return
      end

c=======================================================================
      subroutine QuadG27(quadref,qw)
c=======================================================================
      implicit none
c
c  27 gauss points.
c      quadref: quad points
c           qw: weights
c
c     ^(y) (Top/Bottom)             c     4-----------------3
c     |                             c     |\                 \
c     |                             c     | \               |  \
c     |__________(x) (Left/Right)   c     |   8---------------- 7
c      \                            c     1   |             2   |
c        \                          c (i,j,k) |    (i,j,k)   \  |
c          (z) (Front/Back)         c       \ |                \|
c                                   c         5 ----------------6
c
c              4 ----- 3
c   | y        | 7 8 9 |
c   |          | 4 5 6 |
c   |          | 1 2 3 |
c   o ____x    1 ----  2
c
c
      real*8 quadref(3,27),qw(27)
c
      integer qpt,i,j,k,num,dim
      parameter(dim=3)

      real*8 w(3),w1,w2,w3,x(dim,3,3,3)
c
      w(1) = 5.0d0/18.0d0
      w(2) = 8.0d0/18.0d0
      w(3) = 5.0d0/18.0d0

      do 10 k = 1,3
      do 10 j = 1,3
      do 10 i = 1,3
         num = 9*(k-1) + 3*(j-1) + i
         qw(num) = w(i)*w(j)*w(k)
   10 continue

      w1 = (-dsqrt(3.0d0/5.0d0) + 1.0d0) * 0.5d0
      w2 = 0.5d0
      w3 = (dsqrt(3.0d0/5.0d0) + 1.0d0) * 0.5d0


      do 20 k = 1,3
      do 20 j = 1,3
         x(1,1,j,k) = w1
         x(1,2,j,k) = w2
         x(1,3,j,k) = w3
   20 continue

      do 30 k = 1,3
      do 30 i = 1,3
         x(2,i,1,k) = w1
         x(2,i,2,k) = w2
         x(2,i,3,k) = w3
   30 continue

      do 40 j = 1,3
      do 40 i = 1,3
         x(3,i,j,1) = w1
         x(3,i,j,2) = w2
         x(3,i,j,3) = w3
   40 continue


      do 50 k = 1,3
      do 50 j = 1,3
      do 50 i = 1,3
         num = 9*(k-1) + 3*(j-1) + i
         quadref(1,num) = x(1,i,j,k)
         quadref(2,num) = x(2,i,j,k)
         quadref(3,num) = x(3,i,j,k)
   50 continue


      return
      end


c=======================================================================
      subroutine BaTriLinG27(phi,quadref)
c=======================================================================
      implicit none
c
c  Trilinear basis functions evaluated at the 27 gauss points.
c
c  row of phi: index of quad points
c  col of phi: index of basis functions
c
c     ^(y) (Top/Bottom)             c     4-----------------3
c     |                             c     |\                 \
c     |                             c     | \               |  \
c     |__________(x) (Left/Right)   c     |   8---------------- 7
c      \                            c     1   |             2   |
c        \                          c (i,j,k) |    (i,j,k)   \  |
c          (z) (Front/Back)         c       \ |                \|
c                                   c         5 ----------------6
c
c              4 ----- 3
c   | y        | 7 8 9 |        phi1 = (1-x)(1-y)(1-z)
c   |          | 4 5 6 |        phi2 = x(1-y)(1-z)
c   |          | 1 2 3 |        phi3 = xy(1-z)
c   o ____x    1 ----  2        phi4 = (1-x)y(1-z)
c                               phi5 = (1-x)(1-y)z
c                               phi6 = x(1-y)z
c                               phi7 = xyz
c                               phi8 = (1-x)yz
c
c
      real*8 phi(27,8),quadref(3,27)
c
      integer i,j,k,num,dim,node
      parameter(dim=3)
c
      real*8 BasisTriLin

      do 50 node = 1,8
      do 50 k = 1,3
      do 50 j = 1,3
      do 50 i = 1,3
         num = 9*(k-1) + 3*(j-1) + i
         phi(num,node) = BasisTrilin(quadref(1,num),node)

   50 continue

      return
      end


c=======================================================================
      double precision function BasisTrilin(r,node)
c=======================================================================
      implicit none
c
c
c    Trilinear basis functions on reference element [0,1]*[0,1]*[0,1]
c
c
c     ^(y) (Top/Bottom)             c     4-----------------3
c     |                             c     |\                 \
c     |                             c     | \               |  \
c     |__________(x) (Left/Right)   c     |   8---------------- 7
c      \                            c     1   |             2   |
c        \                          c (i,j,k) |    (i,j,k)   \  |
c          (z) (Front/Back)         c       \ |                \|
c                                   c         5 ----------------6
c
c
c         phi1 = (1-x)(1-y)(1-z)
c         phi2 = x(1-y)(1-z)
c         phi3 = xy(1-z)
c         phi4 = (1-x)y(1-z)
c         phi5 = (1-x)(1-y)z
c         phi6 = x(1-y)z
c         phi7 = xyz
c         phi8 = (1-x)yz
c
      real*8 r(3)
      integer node
c
      real*8 val
c
      if (node.eq.1) then
         val = (1.0d0-r(1))*(1.0d0-r(2))*(1.d0-r(3))
      elseif (node.eq.2) then
         val = r(1)*(1.0d0-r(2))*(1.d0-r(3))
      elseif (node.eq.3) then
         val = r(1)*r(2)*(1.d0-r(3))
      elseif (node.eq.4) then
         val = (1.0d0-r(1))*r(2)*(1.d0-r(3))
      elseif (node.eq.5) then
         val = (1.0d0-r(1))*(1.0d0-r(2))*r(3)
      elseif (node.eq.6) then
         val = r(1)*(1.0d0-r(2))*r(3)
      elseif (node.eq.7) then
         val = r(1)*r(2)*r(3)
      elseif (node.eq.8) then
         val = (1.0d0-r(1))*r(2)*r(3)
      else
         write(*,*)'Input of BasisBilin is wrong'
         stop
      endif

      BasisTrilin = val

      return
      end




