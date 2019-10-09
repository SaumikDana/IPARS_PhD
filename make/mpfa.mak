# mpfa.mak - MPFA make include file

########################## Object files #############################

MOBJA=read3$(O) fcns$(O) impfa$(O) buildmpfa$(O) mpfa_tran$(O) \
      mfarray$(O)

MPFAOBJ=$(MOBJA)

DMPFAOBJ=mpfadual$(O)

###################### Source files ##################################

MPFA = ..$(S)mpfa$(S)

read3.f: $(MPFA)read3.df layout.h control.h readdat.h scrat1.f
	$(SETSIZE1)
	$(SIZEIT)

read3$(O) : scrat1$(O)

fcns.f: $(MPFA)fcns.df control.h blkary.h
	$(SETSIZE1)
	$(SIZEIT)

buildmpfa.f: $(MPFA)buildmpfa.df control.h mpfaary.h layout.h control.h
	$(SETSIZE1)
	$(SIZEIT)

impfa.f: $(MPFA)impfa.df control.h layout.h wells.h mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

mpfa_tran.f: $(MPFA)mpfa_tran.df control.h
	$(SETSIZE1)
	$(SIZEIT)

mfarray.f: $(MPFA)mfarray.df control.h mpfaary.h
	$(SETSIZE1)
	$(SIZEIT)

mpfaary.h: $(MPFA)mpfaary.dh
	$(SETSIZE1)
	$(SIZEIT)

# gp moved from hypre.mak
hypre.f: mpfaary.h
