# parall_c.mak - Parallel framework make include file using C routines
#                    + parallel version of the finnish package.

######################### Object files ###############################

PARALOBJ= putil$(O) \
	manyc$(O) manyf$(O) parbuf$(O) $(PAR_GRAPHOBJ)
PAR_SOLVE_OBJ = pdc3d$(O)


######################## Source files ################################

SORC=..$(S)parall$(S)

putil.f: $(SORC)putil.df control.h restc.h wells.h
	$(SETSIZE)
	$(SIZEIT)

parbuf.f: $(SORC)parbuf.df control.h layout.h scrat1.f output.h
	$(SETSIZE1)
	$(SIZEIT)

parbuf$(O) : scrat1$(O)

manyf.f: $(SORC)manyf.df control.h layout.h 
	$(SETSIZE1)
	$(SIZEIT)

manyc.c: $(SORC)manyc.c cfsimple.h
	$(SETSIZE1)
	$(SIZEIT)














