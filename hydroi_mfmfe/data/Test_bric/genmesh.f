C     
C Generating logically rectangular hexahedral mesh for IPARS
C 
C
C Guangri Xue 05/17/2010 12/04/2010
C

      program genmesh
      IMPLICIT NONE

      INTEGER NX,NY,NZ,MAX,I,J,K
      REAL*8 HX,HY,HZ,X,Y,Z ,XX,YY,ZZ,LX,LY,LZ

      PARAMETER (MAX=200)

      REAL*8 XC(MAX+1,MAX+1,MAX+1),YC(MAX+1,MAX+1,MAX+1),
     &     ZC(MAX+1,MAX+1,MAX+1)
C
      REAL*8 XPERM(MAX,MAX,MAX)
      INTEGER ROCK(MAX,MAX,MAX)
C
      DOUBLE PRECISION TX,TY,TZ,BX,BY,BZ
      REAL*8 VAL1, VAL2,VAL3,VAL4
      INTEGER NUM
C          
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

      HX = 30.0d0/NX
      HY = 30.0d0/NY
      HZ = 4.0d0/NZ


      OPEN(UNIT=1, FILE='mesh.dat',STATUS='UNKNOWN')
      LX = 2.d0
      LY = 2.d0
      LZ = 2.d0


      DO 100 K = 1, NZ+1
      Z= -1.0d0 + (K-1)*HZ
      DO 100 J = 1, NY+1
      Y = -1.0d0 + (J-1)*HY
      DO 100 I = 1, NX+1
      X = -1.0d0 + (I-1)*HX

C X Y Z in [-1 1]  XX YY ZZ in [-1 1]

      call map_phycord(X,Y,Z,LX,LY,LZ,XX,YY,ZZ)

      XC(I,J,K) = XX
      YC(I,J,K) = YY
      ZC(I,J,K) = ZZ
            
 100  CONTINUE


      WRITE(1,111)'NX(1)=',NX,'','NY(1)=',NY,'','NZ(1)=',NZ
      WRITE(1,222) 'XC1()='
      WRITE(1,333) ((((XC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,222) 'YC1()='
      WRITE(1,333) ((((YC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
      WRITE(1,*)
      WRITE(1,222) 'ZC1()='
      WRITE(1,333) ((((ZC(I,J,K)),I=1,NX+1),J=1,NY+1),K=1,NZ+1)
     
      CLOSE(1)
 111  FORMAT(A,I4,A5,A,I4,A5,A,I4)
 222  FORMAT(A)
 333  FORMAT(6(F18.10,3X))

 9999 STOP

      END

c     =====================================================
      subroutine map_phycord(x,y,z,lx,ly,lz,xnew,ynew,znew)
c     =====================================================
      implicit none
C 
c     mapping type  
c                   0: nomapping
c                   1: starshape a
c                   2: starshpe b
c                   3: circle 3
c                   4: circle 4
c                 100: 
c                 101: SPE RSS 2011 paper
c     Guangri Xue 12/4/2010
C
C
      real*8 x,y,z,lx,ly,lz,xnew,ynew,znew
C
      integer maptype
      maptype = 0


      if (maptype.eq.0) then
         xnew = x
         ynew = y
         znew = z
      elseif (maptype.eq.1) then
         call map_stara(y,z,ynew,znew)
         xnew = x
      elseif (maptype.eq.2) then
         call map_starb(y,z,ynew,znew)
         xnew = x
      elseif (maptype.eq.3) then
         call map_circle3(y,z,ynew,znew)
         xnew = x
      elseif (maptype.eq.4) then
         call map_circle4(y,z,ynew,znew)
         xnew = x
      elseif (maptype.eq.100) then
         call map2d_100(y,z,ynew,znew)
         xnew = x
      elseif (maptype.eq.101) then
         call map101(x,y,z,xnew,ynew,znew)
      else
         write(0,*)'maptype=',maptype,' is not implemented'
         stop 
      endif

      xnew = (1.d0 + xnew) * lx/2.d0
      ynew = (1.d0 + ynew) * ly/2.d0
      znew = (1.d0 + znew) * lz/2.d0

      return
      end

c     =====================================================
      subroutine map_xdirection(x,alpha,xnew)
c     =====================================================
      implicit none
C
C     x to xnew based on alpha
C
C  Guangri Xue 05/18/2010
C
      real*8 x,alpha,xnew
      real*8 pi
      data pi/3.14159265358979323846d0/


c      xnew = x + 2.d0*dsin(dsqrt(2.d0)*pi*alpha)
      xnew = x - 2.d0*alpha**2
      
      return
      end

c     =====================================================
      subroutine map_stara(X,Y,Xnew,Ynew)
c     =====================================================
      implicit none
C
C  X Y in (-1, 1)x(-1,1)
C  Xnew Ynew in (-1, 1)x(-1,1)
C
C Guangri Xue 05/18/2010
C 
      real*8 X,Y,Xnew,Ynew
      real*8 r1,r,theta,d
C
      r1 = 1
      d= dmax1(dabs(x),dabs(y))
      r = dmax1(dsqrt(x**2 + y**2), 1e-10)
      xnew = r1 * d * x/r
      ynew = r1 * d * y/r

      theta = datan(dabs(ynew)/ dmax1(dabs(xnew),1e-10))
      r = 1 + .2*cos(6*theta);
      xnew = xnew*r;
      ynew = ynew*r;

      return
      end


c     =====================================================
      subroutine map_starb(X,Y,Xnew,Ynew)
c     =====================================================
      implicit none
C
C  X Y in (-1, 1)x(-1,1)
C  Xnew Ynew in (-1, 1)x(-1,1)
C
C Guangri Xue 05/17/2010
C 
      real*8 X,Y,Xnew,Ynew, val
C
      real*8 X1, Y1, alpha, beta, Xnew1,Ynew1, theta,r,w
C
      X1 = max(dabs(X),1.0e-10)
      Y1 = max(dabs(Y),1.0e-10)
      alpha = dmin1(1.0d0/X1, 1.0d0/Y1)
      beta = alpha*dsqrt(X1**2 + Y1**2)
      Xnew1 = X/beta
      Ynew1 = Y/beta

      theta = datan(Y1/X1)
      r = 1.0d0 + .2d0*dcos(6.0d0*theta)
      Xnew = Xnew1*r
      Ynew = Ynew1*r
      
      w = max(dabs(X),dabs(Y))
      w = w**2
      Xnew = w*Xnew + (1.0d0-w)*X/dsqrt(2.0d0)
      Ynew = w*Ynew + (1.0d0-w)*Y/dsqrt(2.0d0)

c      Xnew = X + 0.1*X
c      Ynew = Y - 0.1*Y

      return
      end
      
c
c     =====================================================
      subroutine map_circle3(xc,yc,xp,yp)
c     =====================================================
c
c     # on input,  (xc,yc) is a computational grid point
c     # on output, (xp,yp) is corresponding point in physical space
c
c     # map the square [-1,1] x [-1,1] to the unit circle
c     # as described in Section 3.3 of
c     #   Logically Rectangular Grids and Finite Volume Methods for PDEs
c     #       in Circular and Spherical Domains,
c     #   by Donna A. Calhoun, Christiane Helzel, and Randall J. LeVeque,
c     #   http://www.amath.washington.edu/~rjl/pubs/circles

c
      implicit double precision (a-h,o-z)
c
c     # radial projection mapping
      r1 = 1.d0
      d = dmax1(dabs(xc), dabs(yc))
      d = max(d, 1.d-10)

      dd = r1*d*(2.d0-d)/dsqrt(2.0d0)
      r = r1

      center = dd - dsqrt(r**2 - dd**2)

      xp = dd/d * dabs(xc)
      yp = dd/d * dabs(yc)

      if (dabs(yc).ge.dabs(xc)) then
         yp = center + dsqrt(r**2-xp**2)
      else
         xp = center + dsqrt(r**2-yp**2)
      endif
  
      if (xc.eq.0.d0) then
         xp = 0.d0
      else if (xc.lt.0.d0) then
         xp = -xp
      endif

      if (yc.eq.0.d0) then
         yp = 0.d0
      else if (yc.lt.0.d0) then
         yp = -yp
      endif


c      xp = xc
c      yp = yc
      return
      end

c
c     =====================================================
      subroutine map_circle4(xc,yc,xp,yp)
c     =====================================================
c
c     # on input,  (xc,yc) is a computational grid point
c     # on output, (xp,yp) is corresponding point in physical space
c
c     # map the square [-1,1] x [-1,1] to the unit circle
c     # as described in Section 3.3 of
c     #   Logically Rectangular Grids and Finite Volume Methods for PDEs
c     #       in Circular and Spherical Domains,
c     #   by Donna A. Calhoun, Christiane Helzel, and Randall J. LeVeque,
c     #   http://www.amath.washington.edu/~rjl/pubs/circles

c
      implicit double precision (a-h,o-z)
c
c     # radial projection mapping
      r1 = 1.d0
      d = dmax1(dabs(xc), dabs(yc))
      r = dmax1(dsqrt(xc**2 + yc**2), 1.d-10)
      xp = r1*d*xc/r
      yp = r1*d*yc/r

c     # interpolate between above mapping and Cartesian grid:
      w = dmax1(dabs(xc),dabs(yc))
      w = w**2
      w = dmin1(w, 1.d0)      !# for ghost cells
      xp = w*xp + (1.d0-w)*xc/dsqrt(2.0d0)
      yp = w*yp + (1.d0-w)*yc/dsqrt(2.0d0)

c      xp = xc
c      yp = yc
      return
      end


c     =====================================================
      subroutine map2d_100(x,y,xnew,ynew)
c     =====================================================
      implicit none
C
C
C  Guangri Xue 05/19/2010
C
      real*8 x,y,xnew,ynew
      real*8 pi
      data pi/3.14159265358979323846d0/

      xnew = x + 0.03d0*dsin(3.d0*pi*x)*dcos(3.d0*pi*y)      
      ynew = y + 0.05d0*dsin(3.d0*pi*x)*dcos(3.d0*pi*y)
      return
      end



c     =====================================================
      subroutine map101(x,y,z,xnew,ynew,znew)
c     =====================================================
      implicit none
C
C  x y z in [-1, 1]x[-1,1]x[-1,1]
C  xnew ynew znew in [-1, 1]x[-1,1]x[-1,1]
C
C Guangri Xue 12/04/2010
C 
      real*8 x,y,z,xnew,ynew,znew
      real*8 pi
      data pi/3.14159265358979323846d0/

c      xnew = x + 0.5d0*dcos(1.d0*pi*y)*dcos(1.d0*pi*z)
      xnew = x
      ynew = y + 0.05d0*dcos(1.d0*pi*y)*dcos(2.d0*pi*z)
      znew = z + 0.05d0*dsin(3.d0*pi*y)*dcos(3.d0*pi*z)


      return
      end
