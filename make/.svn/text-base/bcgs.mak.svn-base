# bcgs.mak - BCGs/Multigrid solver make include file

# Object files #######################################################

SOLVEOBJ=mulgrd$(O) mulgrdm$(O) mulgrdw$(O) linwel$(O) bcgs$(O) bcgs_la$(O) bcgs_wrkla$(O) mlgrdwy$(O)
BCGSOBJ=mulgrd$(O) mulgrdm$(O) mulgrdw$(O) linwel$(O) bcgs$(O) bcgs_la$(O) bcgs_wrkla$(O) mlgrdwy$(O)
SLPAROBJ=mulgrdp$(O)
SOLVDUALOBJ+=mdual$(O)

# Source files #######################################################

mulgrdd.h: ..$(S)solve$(S)bcgs$(S)mulgrdd.dh
	$(SETSIZE)
	$(SIZEIT)

mulgrdc.h: ..$(S)solve$(S)bcgs$(S)mulgrdc.dh mulgrdd.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdpp.h: ..$(S)solve$(S)bcgs$(S)mulgrdpp.dh 
	$(SETSIZE)
	$(SIZEIT)

bcgs_la.h: ..$(S)solve$(S)bcgs$(S)bcgs_la.h
	$(COPYIT)

mulgrd.f: ..$(S)solve$(S)bcgs$(S)mulgrd.df mulgrdc.h control.h blkary.h \
	layout.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdw.f: ..$(S)solve$(S)bcgs$(S)mulgrdw.df mulgrdc.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdp.f: ..$(S)solve$(S)bcgs$(S)mulgrdp.df mulgrdpp.h control.h layout.h mulgrdc.h\
         scrat1.f
	$(SETSIZE)
	$(SIZEIT)

mulgrdp$(O) : scrat1$(O)

mulgrdm.c: ..$(S)solve$(S)bcgs$(S)mulgrdm.dc memory.h 
	$(SETSIZE)
	$(SIZEIT)

mdual.f: ..$(S)solve$(S)bcgs$(S)mdual.df sblkc.h mulgrdc.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

mlgrdwy.f: ..$(S)solve$(S)bcgs$(S)mlgrdwy.df mulgrdc.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

bcgs.f: ..$(S)solve$(S)bcgs$(S)bcgs.df mulgrdc.h control.h blkary.h \
	layout.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

bcgs_la.f: ..$(S)solve$(S)bcgs$(S)bcgs_la.df mulgrdc.h control.h blkary.h \
	msjunk.h bcgs_la.h
	$(SETSIZE)
	$(SIZEIT)

bcgs_wrkla.f: ..$(S)solve$(S)bcgs$(S)bcgs_wrkla.df control.h bcgs_la.h
	$(SETSIZE)
	$(SIZEIT)

linwel.f: ..$(S)wells$(S)linwel.df control.h mulgrdc.h wells.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)
