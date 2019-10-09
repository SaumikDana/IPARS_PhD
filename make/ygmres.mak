# gmres.mak - Make include file for GMRES solver with block Gauss-Seidel
# preconditioning. Pressure block is preconditioned by Fast Direct (Separable)
# Solver (p)dc3d.  

# Library files ######################################################

# BLAS_LIB   = /usr/lib/libblas.a

SOLVELIB = $(LAPACK_LIB) $(BLAS_LIB)

GMRESLIB = $(LAPACK_LIB) $(BLAS_LIB)

# Object files #######################################################

SOLVEOBJ = slgmres$(O) gr8sum$(O) slmsg$(O) mslblk$(O) slupdate$(O) \
	   hsfc$(O) sljacdebug$(O) sljacnorm$(O) ticama$(O) sol$(O) \
	   memman4$(O) $(HYGMRESOBJ) $(IYGMRESOBJ) $(TYGMRESOBJ) $(AYGMRESOBJ) $(NYGMRESOBJ)  \
	   divideinsert$(O) \
	   dgbtr$(O) dste$(O) intrfdc3d$(O) dc2d$(O) dc3d$(O) ums$(O) \
	   amg1r5$(O)  sllsor$(O)  gmg$(O) \
	   $(PAR_SOLVE_OBJ) 

GMRESOBJ = slgmres$(O) gr8sum$(O) slmsg$(O) mslblk$(O) slupdate$(O) \
	   hsfc$(O) sljacdebug$(O) sljacnorm$(O) ticama$(O) sol$(O) \
	   memman4$(O) $(HYGMRESOBJ) $(IYGMRESOBJ) $(TYGMRESOBJ) $(AYGMRESOBJ) $(NYGMRESOBJ) \
	   divideinsert$(O) \
	   dgbtr$(O) dste$(O) intrfdc3d$(O) dc2d$(O) dc3d$(O) ums$(O) \
	   amg1r5$(O)  sllsor$(O)  gmg$(O) \
	   $(PAR_SOLVE_OBJ)

SOLVDUALOBJ += ygdual$(O) ygdddual$(O) 

# Source files #######################################################

SORC = ..$(S)solve$(S)ygmres$(S)
SORP = ..$(S)solve$(S)util$(S)
UTIL = ..$(S)util$(S)
MEMM = ..$(S)memman$(S)
MODLH = ..$(S)hydroi$(S)
MODLI = ..$(S)blacki$(S)
#MODLN = ..$(S)blackp$(S)
MODLT = ..$(S)singlei$(S)
#MODLA = ..$(S)air$(S)

sol.h: $(SORC)sol.h
	$(COPYIT)

r8blas.h: $(SORC)r8blas.h
	$(COPYIT)

r8lapack.h: $(SORC)r8lapack.h
	$(COPYIT)

slblk.h: $(SORC)slblk.h
	$(COPYIT)

sprb.h: $(SORC)sprb.dh
	$(SETSIZE1)
	$(SIZEIT)

sprhandle.h: $(SORC)sprhandle.h
	$(COPYIT)

staticums.h: $(SORC)staticums.h
	$(COPYIT)

sol.f:  $(SORC)sol.df
	$(SETSIZE1)
	$(SIZEIT)

sol.o: 	sol.h sprb.h control.h layout.h sprhandle.h staticums.h

slgmres.f: $(SORC)slgmres.f
	$(COPYIT)

sljacdebug.f: $(SORC)sljacdebug.f
	$(COPYIT)

sljacnorm.f: $(SORC)sljacnorm.f
	$(COPYIT)

slupdate.f: $(SORC)slupdate.df
	$(SETSIZE1)
	$(SIZEIT)

ticama.f: $(SORC)ticama.df
	$(SETSIZE1)
	$(SIZEIT)

ticama$(O): ticama.f slblk.h sol.h control.h sprhandle.h

gr8sum.f: $(SORP)gr8sum.df
	$(SETSIZE1)
	$(SIZEIT)

slmsg.f: $(SORP)slmsg.df control.h
	$(SETSIZE1)
	$(SIZEIT)

mslblk.c: $(SORC)slblk.dc
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

mslblk$(O): mslblk.c  r8blas.h r8lapack.h cfsimple.h 

hsfc.c: $(SORC)hsfc.c
	$(COPYIT)

intrfdc3d.f: $(SORC)intrfdc3d.df
	$(SETSIZE)
	$(SIZEIT)

intrfdc3d$(O): intrfdc3d.f slblk.h sol.h layout.h control.h sprb.h

dc2d.f: $(SORC)dc2d.f
	$(COPYIT)

dc3d.f: $(SORC)dc3d.f
	$(COPYIT)

dc3d.o: staticums.h

pdc3d.f: $(SORC)pdc3d.f
	$(COPYIT)

pdc3d.o: staticums.h

divideinsert.f: $(SORC)divideinsert.f
	$(COPYIT)

divideinsert$(O): divideinsert.f sprb.h 

dgbtr.f: $(SORC)dgbtr.f
	$(COPYIT)

dste.f: $(SORC)dste.f
	$(COPYIT)

memman4.c: $(MEMM)memman4.dc
	$(SETSIZE1)
	$(SIZEIT)

memman4$(O): memman4.c memory.h

hsprb3.f: $(MODLH)hsprb3.df
	$(SETSIZE1)
	$(SIZEIT)

hsprb3$(O): hsprb3.f sprb.h control.h layout.h hfluids.h sprhandle.h sol.h

isprb3.f: $(MODLI)isprb3.df
	$(SETSIZE1)
	$(SIZEIT)

isprb3$(O): isprb3.f sprb.h control.h layout.h ifluid.h sprhandle.h sol.h

#nsprb3.f: $(MODLN)nsprb3.df
#	$(SETSIZE1)
#	$(SIZEIT)

nsprb3$(O): nsprb3.f sprb.h control.h layout.h nfluid.h sprhandle.h sol.h

tsprb3.f: $(MODLT)tsprb3.df
	$(SETSIZE1)
	$(SIZEIT)

tsprb3$(O): tsprb3.f sprb.h control.h layout.h tfluidsc.h sprhandle.h sol.h

#asprb3.f: $(MODLA)asprb3.df
#	$(SETSIZE1)
#	$(SIZEIT)

asprb3$(O): asprb3.f sprb.h control.h layout.h afluid.h sprhandle.h sol.h

ums.f: $(SORC)ums.f
	$(COPYIT)


amg1r5.f: $(SORC)amg1r5.f
	$(COPYIT)

sllsorc.h: $(SORC)sllsorc.dh
	$(SETSIZE1)
	$(SIZEIT)

sllsor.f: $(SORC)sllsor.df
	$(SETSIZE1)
	$(SIZEIT)

sllsor$(O): sllsor.f sllsorc.h control.h blkary.h layout.h

gmgmod.hpp: $(SORC)gmgmod.hpp
	$(COPYIT)

gmg.cpp: $(SORC)gmg.cpp
	$(COPYIT)

gmg$(O): gmg.cpp gmgmod.hpp

ygdual.f: $(SORC)ygdual.df sllsorc.h sblkc.h control.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

ygdual.o: ygdual.f sblkc.h sllsorc.h control.h msjunk.h

ygdddual.f: $(SORC)ygdddual.df sllsorc.h sblkc.h control.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

ygdddual.o: ygdddual.f sblkc.h sllsorc.h control.h msjunk.h
