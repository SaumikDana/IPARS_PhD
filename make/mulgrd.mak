# mulgrd.mak - Multigrid solver make include file

# Object files #######################################################

SOLVEOBJ=mulgrd$(O) mulgrdm$(O) mulgrdw$(O) linwel$(O)
MULGRDOBJ=mulgrd$(O) mulgrdm$(O) mulgrdw$(O) linwel$(O)
SLPAROBJ=mulgrdp$(O)
SOLVDUALOBJ+=mdual$(O)

# Source files #######################################################

mulgrdd.h: ..$(S)solve$(S)mulgrd$(S)mulgrdd.dh
	$(SETSIZE)
	$(SIZEIT)

mulgrdc.h: ..$(S)solve$(S)mulgrd$(S)mulgrdc.dh mulgrdd.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdpp.h: ..$(S)solve$(S)mulgrd$(S)mulgrdpp.dh 
	$(SETSIZE)
	$(SIZEIT)

mulgrd.f: ..$(S)solve$(S)mulgrd$(S)mulgrd.df mulgrdc.h control.h blkary.h \
	layout.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdw.f: ..$(S)solve$(S)mulgrd$(S)mulgrdw.df mulgrdc.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

mulgrdp.f: ..$(S)solve$(S)mulgrd$(S)mulgrdp.df mulgrdpp.h control.h layout.h mulgrdc.h\
        scrat1.f
	$(SETSIZE)
	$(SIZEIT)

mulgrdp$(O) : scrat1$(O)

mulgrdm.c: ..$(S)solve$(S)mulgrd$(S)mulgrdm.dc memory.h 
	$(SETSIZE)
	$(SIZEIT)

mdual.f: ..$(S)solve$(S)mulgrd$(S)mdual.df sblkc.h mulgrdc.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

linwel.f: ..$(S)wells$(S)linwel.df control.h mulgrdc.h wells.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)
