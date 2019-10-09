# poroe.mak - Poroelastic model include file

# Object files  ##############################################

RUIJIE_OBJ = model_module0$(O) bc0$(O) dmat_ep$(O) gstiff0$(O) \
             hexa_NB0$(O) load_vector0$(O) master-big0$(O) \
             post_prcss0$(O) pre_prcss0$(O) utility0$(O) \
             crack_profile_setup$(O)

EOBJ = eidata$(O) earray$(O) ebdary$(O) eivdat$(O) estep$(O) \
        estdout$(O) ecompute$(O) esetup3d$(O) elastic$(O) \
        ebdarym$(O) erest$(O) eprojections$(O) $(RUIJIE_OBJ)

EVISOBJ = evisual$(O)

VISUALOBJ := $(VISUALOBJ) $(EVISOBJ)

# Source files  ##############################################
SORC=..$(S)porohex$(S)

fracture.h: ../util/fracture.dh
	$(SETSIZE1)
	$(SIZEIT)

moddefs.h: $(SORC)../util/moddefs.dh
	$(SETSIZE1)
	$(SIZEIT)

earydat.h: $(SORC)earydat.dh
	$(SETSIZE1)
	$(SIZEIT)

emodel.h: $(SORC)emodel.dh moddefs.h
	$(SETSIZE1)
	$(SIZEIT)

ebdary.h: $(SORC)ebdary.dh
	$(SETSIZE1)
	$(SIZEIT)

#saumik
idata.f: earydat.h
initial.f: emodel.h
impfa.f: emodel.h

eidata.f: $(SORC)eidata.df msjunk.h control.h blkary.h unitsex.h \
         earydat.h emodel.h ebdary.h visual.h mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

earray.f: $(SORC)earray.df msjunk.h control.h layout.h earydat.h emodel.h hypre.h \
                 mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

ebdarym.c: $(SORC)ebdarym.dc memory.h
	$(SETSIZE1)
	$(SIZEIT)

ebdary.f: $(SORC)ebdary.df msjunk.h control.h readdat.h scrat1.f \
        unitsex.h ebdary.h emodel.h earydat.h
	$(SETSIZE1)
	$(SIZEIT)

ebdary$(O) : scrat1$(O)

eivdat.f: $(SORC)eivdat.df msjunk.h control.h times.h blkary.h \
        layout.h earydat.h emodel.h ebdary.h mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

estep.f: $(SORC)estep.df msjunk.h control.h blkary.h times.h \
        ebdary.h emodel.h layout.h fracture.h
	$(SETSIZE1)
	$(SIZEIT)

esetup3d.f: $(SORC)esetup3d.df msjunk.h control.h layout.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

elastic.f: $(SORC)elastic.df msjunk.h control.h layout.h ebdary.h \
        emodel.h earydat.h wells.h mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

ecompute.f: $(SORC)ecompute.df control.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

estdout.f: $(SORC)estdout.df control.h msjunk.h layout.h blkary.h \
        earydat.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

erest.f: $(SORC)erest.df control.h restc.h blkary.h output.h \
        earydat.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

eprojections.f: $(SORC)eprojections.df blkary.h control.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

evisual.h : $(SORC)evisual.dh
	$(SETSIZE1)
	$(SIZEIT)
                                                                                
evisual.f : $(SORC)evisual.df visual.h evisual.h emodel.h
	$(SETSIZE1)
	$(SIZEIT)

model_module0.f90: $(SORC)model_module0.f90
	$(COPYIT)

bc0.f90: $(SORC)bc0.f90
	$(COPYIT)

dmat_ep.f90: $(SORC)dmat_ep.f90
	$(COPYIT)

gstiff0.f90: $(SORC)gstiff0.f90
	$(COPYIT)

hexa_NB0.f90: $(SORC)hexa_NB0.f90
	$(COPYIT)

load_vector0.f90: $(SORC)load_vector0.f90
	$(COPYIT)

master-big0.f90: $(SORC)master-big0.f90
	$(SETSIZE1)
	$(SIZEIT)

post_prcss0.f90: $(SORC)post_prcss0.f90
	$(COPYIT)

pre_prcss0.f90: $(SORC)pre_prcss0.f90
	$(COPYIT)

utility0.f90: $(SORC)utility0.f90
	$(COPYIT)

crack_profile_setup.f90: $(SORC)crack_profile_setup.f90
	$(COPYIT)

