#-----------------------------------------------------------------------
# vistecbin.mak - link to tecplot library for binary output
#-----------------------------------------------------------------------

ifeq ($(SYSTEM),osx-openmpi-ifort)
  TECLIB = /Users/Shared/libtecio.a
endif

ifeq ($(SYSTEM),c7)
#  TECLIB = /opt/apps/sysnet/tecplot/2013/lib/libtecio.a
  TECLIB = /h1/bganis/tecio64.a
endif

ifeq ($(SYSTEM),sl6)
#  TECLIB = /opt/apps/sysnet/tecplot/2011/lib/libtecio.a
  TECLIB = /h1/bganis/tecio64.a
endif

ifeq ($(SYSTEM),bevo3)
#  TECLIB = /home/bganis/tecio64.a
#  TECLIB = /work/saumik/IPARSv3.1-Repo/tecio64.a  

  TECLIB = /work/tameem/sum_tim_work/tecio64.a  
endif

ifeq ($(SYSTEM),stampede)
  TECLIB = /home1/01028/bganis/tecio64.a
endif

