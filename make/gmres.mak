# gmres.mak - Make include file for GMRES solver with block Gauss-Seidel
# preconditioning.

# Library files ######################################################

# BLAS_LIB   = /usr/lib/libblas.a

SOLVELIB = $(LAPACK_LIB) $(BLAS_LIB)

GMRESLIB = $(LAPACK_LIB) $(BLAS_LIB)

# Object files #######################################################

SOLVEOBJ = slgmres$(O) gr8sum$(O) slmsg$(O) slblk$(O) slupdate$(O) \
	   hsfc$(O) sljacdebug$(O) sljacnorm$(O) ticama$(O) sol$(O)

GMRESOBJ = slgmres$(O) gr8sum$(O) slmsg$(O) slblk$(O) slupdate$(O) \
	   hsfc$(O) sljacdebug$(O) sljacnorm$(O) ticama$(O) sol$(O)

# Source files #######################################################

SORC = ..$(S)solve$(S)gmres$(S)
SORP = ..$(S)solve$(S)util$(S)
UTIL = ..$(S)util$(S)

sol.h: $(SORC)sol.h
	$(COPYIT)

r8blas.h: $(SORC)r8blas.h
	$(COPYIT)

r8lapack.h: $(SORC)r8lapack.h
	$(COPYIT)

slblk.h: $(SORC)slblk.h
	$(COPYIT)

sol.f:  $(SORC)sol.df
	$(SETSIZE1)
	$(SIZEIT)

sol.o: 	sol.h

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

ticama$(O): ticama.f slblk.h sol.h

gr8sum.f: $(SORP)gr8sum.df
	$(SETSIZE1)
	$(SIZEIT)

slmsg.f: $(SORP)slmsg.df control.h
	$(SETSIZE1)
	$(SIZEIT)

slblk.c: $(SORC)slblk.dc
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

slblk$(O): slblk.c  r8blas.h r8lapack.h cfsimple.h

mslblk$(O): mslblk.c  r8blas.h r8lapack.h cfsimple.h

hsfc.c: $(SORC)hsfc.c
	$(COPYIT)







