c ***********************************************************
CSGT MODIFY IVAN'S PERM PROJECTION CODE TO GENERAL 3-D
c ***********************************************************      
c  this program projects a given permeability file onto several 
c  (non-)matching grids
c  initial grid:     dimx x  dimy x  dimz
c  block partition: ndivx x ndivy x ndivz
c  i-th block, grid: nx(i) x ny(i) x nz(i)
c  dimensions of one cell in i-th block: dx(i) x dy(i) x dz(i)
c  offsets of the lower left corner of i-th block: [xoff(i),yoff(i),zoff(i)]
c  Maps the whole domain to unit cube, where computaions are done
c 

      program permUPSC
      implicit none
c
      integer dimx,dimy,dimz,maxdim,ndivx,ndivy,ndivz
      integer nx(40),ny(40),nz(40),i,j,k,nb,nxx,nyy,nzz,l,factor
csgt      parameter(maxdim=250,dimx=6,dimy=17,dimz=20)
      parameter(maxdim=250,dimx=8,dimy=26,dimz=1)

      real*8 kx(maxdim,maxdim,maxdim),ky(maxdim,maxdim,maxdim),
     $       kz(maxdim,maxdim,maxdim) 
      real*8 xoff(40),yoff(40),zoff(40),KinX(dimx,dimy,dimz),
     $       KinY(dimx,dimy,dimz),KinZ(dimx,dimy,dimz),
     $       dx(40),dy(40),dz(40),perms,x1,y1,z1,x2,y2,z2,dxx,dyy,dzz
      character*30 fname,inputfile1,inputfile2,inputfile3
      character*103 line
      write(inputfile1,2)
csgt 2    format('Xperm.dat')
 2    format('permxTest')
      write(inputfile2,17)
csgt 17   format('Yperm.dat')
 17   format('permyTest')
      write(inputfile3,18)
csgt 18   format('Zperm.dat')
 18   format('permzTest')

      open(33,file=inputfile1,status='old',err=9000)
      open(34,file=inputfile2,status='old',err=9001)
      open(35,file=inputfile3,status='old',err=9002)
      ndivx = 1
      ndivy = 1
      ndivz = 1

c   skip first line
      read(33,*)
      read(34,*)
      read(35,*)

c   offsets of the lower (upper in ipars) left corner 
c   of subdomains
      xoff(1) = 0.0D0
      yoff(1) = 0.0D0
      zoff(1) = 0.0D0
c dim of subdomains
      nx(1) = 16
      ny(1) = 100
      nz(1) = 1

      read(33,*,err=9100)(((KinX(i,j,k),i=1,dimx),j=1,dimy),k=1,dimz)
      close(33)
      read(34,*,err=9100)(((KinY(i,j,k),i=1,dimx),j=1,dimy),k=1,dimz)
      close(34)
      read(35,*,err=9100)(((KinZ(i,j,k),i=1,dimx),j=1,dimy),k=1,dimz)
      close(35)
      
      open(20,file='CWInj_16x100permX')
      open(21,file='CWInj_16x100permY')
      open(22,file='CWInj_16x100permZ')
      do nb = 1,ndivx*ndivy*ndivz

c      dx, dy, dz for subdomains
         dx(nb) = 1.0D0/(ndivx*nx(nb))
         dy(nb) = 1.0D0/(ndivy*ny(nb))
         dz(nb) = 1.0D0/(ndivz*nz(nb))

c      refinement level
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
                  kx(i,j,k) = perms(1,x1,y1,z1,x2,y2,z2,KinX,dimx,dimy,
     $                              dimz)
                  ky(i,j,k) = perms(2,x1,y1,z1,x2,y2,z2,KinY,dimx,dimy,
     $                              dimz)
                  kz(i,j,k) = perms(3,x1,y1,z1,x2,y2,z2,KinZ,dimx,dimy,
     $                              dimz)
               enddo
            enddo
         enddo
         if(nb<10) then
                 write(20,*)'XPERM'//char(48+nb)//'() = '
                 write(20,9)(((kx(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
                 write(21,*)'YPERM'//char(48+nb)//'() = '
                 write(21,9)(((ky(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
                 write(22,*)'ZPERM'//char(48+nb)//'() = '
                 write(22,9)(((kz(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
         elseif(nb<100) then
                 write(20,*)'XPERM'//char(48+nb/10)//
     &                   char(48+mod(nb,10))//'() = '
                 write(20,9)(((kx(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
                 write(21,*)'YPERM'//char(48+nb/10)//
     &                   char(48+mod(nb,10))//'() = '
                 write(21,9)(((ky(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
                 write(22,*)'ZPERM'//char(48+nb/10)//
     &                   char(48+mod(nb,10))//'() = '
                 write(22,9)(((kz(i,j,k),i=1,nxx),j=1,nyy),k=1,nzz)
         endif
      enddo

 3    format('permK',i4.4)
 9    format(6F12.4)
c
      close(20)
      close(21)
      close(22)
      goto 9999

 9000 continue
      write(*,*) 'Error opening input file ', inputfile1
      STOP 2

 9001 continue
      write(*,*) 'Error opening input file ', inputfile2
      STOP 3

 9002 continue
      write(*,*) 'Error opening input file ', inputfile3
      STOP 4

 9100 continue
      write(*,*) 'Error reading input file'
      STOP 5

 9999 end

      real*8 function perms(idir,x1,y1,z1,x2,y2,z2,Kin,dimx,dimy,dimz)
      implicit none
      integer dimx,dimy,dimz
      real*8 x1,y1,z1,x2,y2,z2,ksum,kharm,Kin(dimx,dimy,dimz),dx,dy,dz
      integer idir,i,j,k,i1,j1,k1,i2,j2,k2,nelx,nely,nelz

c   Whole domain is mapped to unit cube
      dx = 1.0D0/dimx
      dy = 1.0D0/dimy
      dz = 1.0D0/dimz

      perms = -100.0D0

csgt  Upscale perms according to general rule to maintain 
csgt  perm heterogeneity & flow character on finest scale:-
csgt  for Kxx, arithmetic mean across all cells in y- and z-
csgt  and harmonic across x-, likewise for Kyy, Kzz; for 
csgt  now we are only interested in diagonal perms, but if 
csgt  full tensor, then for Kxy, arithmetic across all cells 
csgt  across z-, harmonic across x- and y-,likewise,Kyz, Kzx
csgt  This method holds even for refinements.
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

      ksum=0.
      kharm=0.
      goto (9,10,11), idir

csgt  Need to use Kxx here (assumed = Kin) 
 9    do i=i1,i2
         do j=j1,j2
            do k=k1,k2
               ksum=ksum+Kin(i,j,k)
            enddo
         enddo
         kharm=kharm+1.0D0/(ksum*nelx/(nely*nelz))
         ksum=0.0D0
      enddo
      perms = 1.0D0/kharm
      return

csgt  Need to use Kyy here (assumed = Kin)
 10   do j=j1,j2
         do i=i1,i2
            do k=k1,k2
               ksum=ksum+Kin(i,j,k)
            enddo
         enddo
         kharm=kharm+1.0D0/(ksum*nely/(nelx*nelz))
         ksum=0.0D0
      enddo
      perms = 1.0D0/kharm
      return
 
csgt  Need to use Kzz here (assumed = Kin)
 11   do k=k1,k2
         do i=i1,i2
            do j=j1,j2
               ksum=ksum+Kin(i,j,k)
            enddo
         enddo
         kharm=kharm+1.0D0/(ksum*nelz/(nelx*nely))
         ksum=0.0D0
      enddo
      perms = 1.0D0/kharm
      return
 
      end

