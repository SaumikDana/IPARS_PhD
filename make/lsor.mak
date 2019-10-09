# lsor.mak - Line sor solver make include file

# Object files #######################################################

SOLVEOBJ=lsor$(O)
LSOROBJ=lsor$(O)

SOLVDUALOBJ+=ldual$(O)

# Source files #######################################################

lsorc.h: ..$(S)solve$(S)lsor$(S)lsorc.dh
	$(SETSIZE)
	$(SIZEIT)

lsor.f: ..$(S)solve$(S)lsor$(S)lsor.df lsorc.h restc.h unitsex.h
	$(SETSIZE)
	$(SIZEIT)

ldual.f: ..$(S)solve$(S)lsor$(S)ldual.df control.h sblkc.h blkary.h \
	lsorc.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)
