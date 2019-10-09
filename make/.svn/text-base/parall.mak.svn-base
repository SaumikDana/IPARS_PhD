# parall.mak - Parallel framework make include file

######################### Library files ##############################

MPIFH    = mpif.h

######################### Object files ###############################

PARALOBJ= many$(O) putil$(O)  parall$(O) $(SLPAROBJ)
PARDUALOBJ=pdual$(O) pdualm$(O)
PAR_SOLVE_OBJ=pdc3d$(O)

######################## Source files ################################

SORC=..$(S)parall$(S)

many.f: $(SORC)many.df control.h scrat1.f layout.h output.h
	$(SETSIZE)
	$(SIZEIT)

many$(O) : scrat1$(O)

putil.f: $(SORC)putil.df control.h restc.h wells.h
	$(SETSIZE)
	$(SIZEIT)

parall.c: ..$(S)memman$(S)parall.dc memory.h
	$(SETSIZE)
	$(SIZEIT)
