#-----------------------------------------------------------------------
# pcg.mak - Make include file for the PCG solver 
# 		with diagonal preconditioning
#-----------------------------------------------------------------------

# Library files ######################################################

SOLVELIB = 

PCGLIB = 

# Object files #######################################################

SOLVEOBJ = pcgi$(O) pcg$(O) pcg_stub$(O) pcg_istub$(O) \
	   d1mach$(O) slmsg$(O) gr8sum$(O) 

PCGOBJ   = pcgi$(O) pcg$(O) pcg_stub$(O) pcg_istub$(O) \
	   d1mach$(O) slmsg$(O) gr8sum$(O) 

# Source files #######################################################

SORC = ..$(S)solve$(S)pcg$(S)

SORU = ..$(S)solve$(S)util$(S)

pcg.h: $(SORC)pcg.dh
	$(SETSIZE1)
	$(SIZEIT)

pcg.f: $(SORC)pcg.df 
	$(SETSIZE1)
	$(SIZEIT)

pcg$(O): pcg.h 

pcgi.f: $(SORC)pcgi.df 
	$(SETSIZE1)
	$(SIZEIT)

pcgi$(O): pcg.h 

pcg_stub.f: $(SORC)pcg_stub.df
	$(SETSIZE1)
	$(SIZEIT)

pcg_stub$(O):

pcg_istub.f: $(SORC)pcg_istub.df
	$(SETSIZE1)
	$(SIZEIT)

pcg_istub$(O):

d1mach.c : $(SORU)d1mach.c 
	$(COPYIT)

d1mach$(O):

# --- the files below are common to GMRES and PCG solver 

slmsg.f : $(SORU)slmsg.df
	$(SETSIZE1)
	$(SIZEIT)

gr8sum.f : $(SORU)gr8sum.df
	$(SETSIZE1)
	$(SIZEIT)

#-----------------------------------------------------------------------
