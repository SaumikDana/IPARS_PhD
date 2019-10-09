# hypre.mak - HYPRE make include file

FINCLUDES  = -I$(HYPRE_DIR)/include
FDEFS      = -DHAVE_CONFIG_H -DHYPRE_TIMING
LIBS_HYPRE = -L$(HYPRE_DIR)/lib -lHYPRE

# Object files #######################################################

HYPREOBJ=hypre$(O)
DHYPREOBJ=hypre_dual$(O)

DHYPREOBJ=hypre_dual$(O)

# Source files #######################################################

# real path of hypre files need to be corected.
SORC = ..$(S)solve$(S)hypre$(S)

hypre.h: $(SORC)hypre.dh
	$(SETSIZE1)
	$(SIZEIT)

hypre.f: $(SORC)hypre.df 
	$(SETSIZE1)
	$(SIZEIT)

hypre_dual.h: $(SORC)hypre_dual.dh
	$(SETSIZE1)
	$(SIZEIT)

hypre_dual.f: $(SORC)hypre_dual.df 
	$(SETSIZE1)
	$(SIZEIT)

# gp: dependence on mpfaary.h moved to mpfa.mak since hypre now 
# works also with bricks
#hypre$(O): hypre.f hypre.h control.h blkary.h layout.h mpfaary.h
hypre$(O): hypre.f hypre.h control.h blkary.h layout.h

hypre_dual$(O): hypre_dual.f hypre.h control.h blkary.h \
                layout.h sblkc.h hypre_dual.h

