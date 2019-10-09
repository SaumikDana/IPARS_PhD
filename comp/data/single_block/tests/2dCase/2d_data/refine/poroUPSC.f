c ***********************************************************
CSGT MODIFY IVAN-GERGINA PERM PROJECTION CODE TO GENERAL 3-D
c ***********************************************************      
c  this program projects a given porosity file onto possibly 
c  several (non-)matching grids
c  initial grid:     dimx x  dimy x  dimz
c  block partition: ndivx x ndivy x ndivz
c  i-th block, grid: nx(i) x ny(i) x nz(i)
c  dimensions of one cell in i-th block: dx(i) x dy(i) x dz(i)
c  offsets of the lower left corner of i-th block: [xoff(i),yoff(i),zoff(i)]
c  Maps the whole domain to unit cube, where computaions are done
c 

      program poroUPSC
      implicit none
c
      integer dimx,dimy,dimz,maxdim,ndivx,ndivy,ndivz
      integer nx(40),ny(40),nz(40),i,j,k,nb,nxx,nyy,nzz,l,factor
      parameter(maxdim=250,dimx=8,dimy=26,dimz=1)

      real*8 p(maxdim,maxdim,maxdim)
      real*8 xoff(40),yoff(40),zoff(40),Pin(dimx,dimy,dimz),
     $       dx(40),dy(40),dz(40),porosity,x1,y1,z1,x2,y2,z2,dxx,dyy,dzz
      character*30 fname,inputfile
      character*103 line
      write(inputfile,2)
 2    format('poroTest')

      open(33,file=inputfile,status='old',err=9000)
      ndivx = 1
      ndivy = 1
      ndivz = 1

cgp   skip first line
      read(33,*)

cgp   offsets of the lower (upper in ipars) left corner 
cgp   of subdomains
      xoff(1) = 0.0D0
      yoff(1) = 0.0D0
      zoff(1) = 0.0D0
c
cgp dim of subdomains
      nx(1) = 16
      ny(1) = 100
      nz(1) = 1

      read(33,*,err=9100)(((Pin(i,j,k),i=1,dimx),j=1,dimy),k=1,dimz)
      close(33)
      
      open(20,file='CWInj_16x100poro')
      do nb = 1,ndivx*ndivy*ndivz

cgp      dx, dy, dz for subdomains
         dx(nb) = 1.0D0/(ndivx*nx(nb))
         dy(nb) = 1.0D0/(ndivy*ny(nb))
         dz(nb) = 1.0D0/(ndivz*nz(nb))

cgp      refinement level
         l = 0
         write(fname,3)nb
c     
         factor = int(2**l)
         nxx = nx(nb)*factor
         nyy = ny(nb)*factor
         nzz = nz(nb)*factor
         dxx = dx(nb)/factor
         dyy = dy(nb)/factor
         dzz = dz(nb)/factor

         do k = 1,nzz
            z1 = zoff(nb) + (k-1)*dzz
            z2 = z1 + dzz
            do j = 1,nyy
               y1 = yoff(nb) + (j-1)*dyy
               y2 = y1 + dyy
               do i = 1,nxx
                  x1 = xoff(nb) + (i-1)*dxx
                  x2 = x1 + dxx
                  p(i,j,k) = porosity(x1,y1,z1,x2,y2,z2,Pin,dimx,dimy,
     $                              dimz)
               enddo
            enddo
         enddo
         if(nb<10) then
                 write(20,*)'POROSITY'//char(48+nb)//'() = '
                 write(20,9)(((p(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
         elseif(nb<100) then
                 write(20,*)'POROSITY'//char(48+nb/10)//
     &                   char(48+mod(nb,10))//'() = '
                 write(20,9)(((p(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
         endif
      enddo

 3    format('permK',i4.4)
 9    format(6F12.4)
c
      goto 9999

 9000 continue
      write(*,*) 'Error opening input file ', inputfile
      STOP 2

 9100 continue
      write(*,*) 'Error reading input file'
      STOP 3

 9999 end

      real*8 function porosity(x1,y1,z1,x2,y2,z2,Pin,dimx,dimy,dimz)
      implicit none
      integer dimx,dimy,dimz
      real*8 x1,y1,z1,x2,y2,z2,psum,Pin(dimx,dimy,dimz),dx,dy,dz
      integer i,j,k,i1,j1,k1,i2,j2,k2,nelx,nely,nelz

cgp   Whole domain is mapped to unit cube
      dx = 1.0D0/dimx
      dy = 1.0D0/dimy
      dz = 1.0D0/dimz

      porosity = 0.2D0

csgt  Upscale porosity by taking simple arithmetic mean (since
csgt  porosity enters as volumetric term in multi-phase flow 
csgt  equations and phase flux is not a function of porosity.
      i1 = 0
 3    i1 = i1+1
      if (x1.ge.i1*dx) goto 3
      i2 = i1-2
 4    i2 = i2+1
      if ((x2.gt.i2*dx).and.(i2.ne.dimx)) goto 4

      j1 = 0
 5    j1 = j1+1
      if (y1.ge.j1*dy) goto 5
      j2 = j1-2
 6    j2 = j2+1
      if ((y2.gt.j2*dy).and.(j2.ne.dimy)) goto 6

      k1 = 0
 7    k1 = k1+1
      if (z1.ge.k1*dz) goto 7
      k2 = k1-2
 8    k2 = k2+1
      if ((z2.gt.k2*dz).and.(k2.ne.dimz)) goto 8

      nelx=i2-i1+1
      nely=j2-j1+1
      nelz=k2-k1+1

      psum=0.0D0

csgt  Find arithmetic mean      
      do i=i1,i2
         do j=j1,j2
            do k=k1,k2
               psum=psum+Pin(i,j,k)
            enddo
         enddo
      enddo
      porosity=psum/(nelx*nely*nelz)

      return
      end
