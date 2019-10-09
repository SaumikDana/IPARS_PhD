#-----------------------------------------------------------------------
# dual.mak - Make include file for dual approximation block interface
#-----------------------------------------------------------------------

# Framework Object files #############################################

BLOCKSOBJ = $(SOLVDUALOBJ) inface$(O) blksin$(O) infcomm$(O) dualmod$(O) \
 adaptivity$(O) \
 $(PARDUALOBJ) $(DHYDROIOBJ) $(DSINGLEIOBJ) $(DSINGLEEOBJ) $(DBLACKIOBJ) \
 $(DCOMPOBJ) $(DTRCHEMOBJ) $(DMPFAOBJ) $(DHYPREOBJ)

# Source files #######################################################

SORC = ..$(S)blocks$(S)dual$(S)

sblkc.h : $(SORC)sblkc.dh
	$(SETSIZE)
	$(SIZEIT)

inface.c : $(SORC)inface.dc memory.h
	$(SETSIZE)
	$(SIZEIT)

blksin.f : $(SORC)blksin.df control.h layout.h sblkc.h \
	blkary.h unitsex.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

infcomm.f : $(SORC)infcomm.df control.h layout.h sblkc.h blkary.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

# bag8
initMPFA$(O) : dualmod$(O)
blksin$(O) : dualmod$(O)
dualmod.f: $(SORC)dualmod.df
	$(SETSIZE1)
	$(SIZEIT)

idata$(O): adaptivity$(O)

adaptivity.f: $(SORC)adaptivity.df layout.h
	$(SETSIZE1)
	$(SIZEIT)

SORC = ..$(S)parall$(S)

pdual.f : $(SORC)pdual.df
	$(SETSIZE)
	$(SIZEIT)

pdualm.c : $(SORC)pdualm.dc
	$(SETSIZE)
	$(SIZEIT)

# bag8
SORC = ..$(S)mpfa$(S)

mpfadual.f: $(SORC)mpfadual.df visual.h
	$(SETSIZE1)
	$(SIZEIT)

