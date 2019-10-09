      program well
      implicit none
      character* 40 filename,tecname
      parameter (filename = 'WELLCUM.OUT',
     &           tecname = 'WELLCUM.dat')
      real*8 time(20000),watinj(20000),oilprod(20000),watprod(20000),
     $       gasinj(20000)
      integer i,j,k

      open(50,file=filename,status='old')
      open(70,file=tecname,status='new')
      do i = 1,42
         j = 18*(i-1)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)(time(k),k=j+1,j+18)
         read(50,*,end=100,err=100)(watinj(k),k=j+1,j+18)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)(oilprod(k),k=j+1,j+18)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)(watprod(k),k=j+1,j+18)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100)(woratio(k),k=j+1,j+18)
      enddo
 100  continue
      write(70,*)'TITLE = "WELL RATES"'
      write(70,*)
     $     'VARIABLES = "Time [days]","Water Injection Rate [stb/day]",' 
      write(70,*)'"Oil Production Rate [stb/day]",', 
     $     ' "Water Production Rate [stb/day]", "Water/Oil Ratio"'
      write(70,*)'ZONE F=BLOCK, T="", I =',j
      write(70,11)(time(i),i=1,j)
      write(70,*)
      write(70,11)(watinj(i),i=1,j)
      write(70,*)
      write(70,11)(oilprod(i),i=1,j)
      write(70,*)
      write(70,11)(watprod(i),i=1,j)
      write(70,*)
      write(70,11)(woratio(i),i=1,j)
      write(70,*)
      close(70)
 11   format(6 F15.6)
      end
