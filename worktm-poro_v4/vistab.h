c---- include vistab.dh

      integer vis_tabotype

      integer maxtabnum
      parameter (maxtabnum = 76)

      character*8 tabname(maxtabnum)
      character*8 tabx(maxtabnum),taby(maxtabnum)

      integer numrock(maxtabnum)
      real*8 xmin(maxtabnum),xmax(maxtabnum)

      integer ndatavals
      real*8 xdatavals (100),ydatavals(100)

      common /vistab/ xmin, xmax, xdatavals,ydatavals,
     &	ndatavals, numrock, vis_tabotype,
     &	tabname, tabx,taby

c-------- end include
