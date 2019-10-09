# trchem.mak - transport-chemistry multimodel modular type file: 
# must be included after "regular" model makefiles

# Object files #######################################################

TROBJ =   trisdat$(O) trarray$(O) trstep$(O) trivdat$(O) trgdv$(O) \
	  trflow$(O)  \
          trx_step$(O)  trx_kntc$(O)  trx_equ$(O)  trx_rnsf$(O)  \
          trutil$(O) trwdata$(O) trwell$(O)  trbdary$(O) trdiff$(O)\
          trfim$(O)
TRVISOBJ = trvisual$(O)

DTRCHEMOBJ=trdual$(O)

#SGT added mortar based multi-block objects
TR_MBOBJ = trm_util$(O) trfault$(O)

#####################################################################
## TRCHEM Multi Model specific part 
#################### 	FIRST STAGE: do not touch
## reset the variables used by unix.mak 
MODELOBJ =
VISUALOBJ = 
MBOBJ =
#
SOLVEOBJ = 
SOLVELIB = 

# sgt 
MODELOBJ = $(SINGLEIOBJ) $(SINGLEEOBJ) $(HYDROIOBJ) $(HYDROEOBJ) \
           $(BLACKIOBJ) $(COMPOBJ) $(TROBJ) $(H2TROBJ) $(T2TROBJ)
# sgt

VISUALOBJ = $(IVISOBJ) $(HVISOBJ) $(XVISOBJ) $(SVISOBJ) $(TVISOBJ) $(TRVISOBJ)

# add the objects for the models and solvers to be included : to remove
# duplicated files use the makefile text utility sort

SOLOBJ = $(BCGSOBJ) $(GMRESOBJ) $(PCGOBJ)
SOLVEOBJ = $(sort $(SOLOBJ))
SOLLIB = $(GMRESLIB) $(PCGLIB)
SOLVELIB = $(SOLLIB)

### END OF MMODEL modifications


# Source files #######################################################

SORC=..$(S)trchem$(S)

trisdat.f : $(SORC)trisdat.df  trmodel.h trarydat.h
	$(SETSIZE1)
	$(SIZEIT)

trmodel.h : $(SORC)trmodel.dh moddefs.h
	$(SETSIZE1)
	$(SIZEIT)

trdvars.h : $(SORC)trdvars.dh
	$(SETSIZE1)
	$(SIZEIT)

moddefs.h : $(SORC)../util/moddefs.dh
	$(SETSIZE1)
	$(SIZEIT)

trarydat.h : $(SORC)trarydat.h
	$(SETSIZE1)
	$(SIZEIT)

trarray.f : $(SORC)trarray.df  trarydat.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trstep.f : $(SORC)trstep.df  trarydat.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trivdat.f : $(SORC)trivdat.df  trarydat.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trgdv.f : $(SORC)trgdv.df  trarydat.h trmodel.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

trfim.f : $(SORC)trfim.df  trarydat.h trmodel.h control.h
	$(SETSIZE1)
	$(SIZEIT)


trflow.f : $(SORC)trflow.df  trarydat.h trmodel.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

trvisual.h: $(SORC)trvisual.dh 
	$(SETSIZE1)
	$(SIZEIT)

trvisual.f: $(SORC)trvisual.df trvisual.h trarydat.h control.h visual.h \
	layout.h blkary.h     
	$(SETSIZE1)
	$(SIZEIT)
 
trx_step.f: $(SORC)trx_step.df trarydat.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)
 
trx_kntc.f: $(SORC)trx_kntc.df
	$(SETSIZE1)
	$(SIZEIT)

trx_equ.f: $(SORC)trx_equ.df
	$(SETSIZE1)
	$(SIZEIT)

trx_rnsf.f: $(SORC)trx_rnsf.df
	$(SETSIZE1)
	$(SIZEIT)

trutil.f: $(SORC)trutil.df
	$(SETSIZE1)
	$(SIZEIT)

trbdary.h: $(SORC)trbdary.dh 
	$(SETSIZE1)
	$(SIZEIT)

trbdary.f: $(SORC)trbdary.df unitsex.h boundary.h control.h layout.h \
        trbdary.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trwdata.f : $(SORC)trwdata.df  wells.h trmodel.h control.h
	$(SETSIZE1)
	$(SIZEIT)

trwell.f : $(SORC)trwell.df  wells.h trmodel.h control.h
	$(SETSIZE1)
	$(SIZEIT)

hwell.f: trmodel.h

twell.f: trmodel.h

trdiff.f : $(SORC)trdiff.df control.h trmodel.h trarydat.h blkary.h
	$(SETSIZE1)
	$(SIZEIT)

trdual.f: $(SORC)trdual.df control.h blkary.h trarydat.h trmodel.h sblkc.h \
        trdvars.h unitsex.h layout.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

trfault.f: $(SORC)trfault.df control.h trarydat.h layout.h blkary.h mbvars.h \
	 mb_utilv.h tr_mbvars.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trm_util.f: $(SORC)trm_util.f trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

trm_util.o: trm_util.f trarydat.h mbvars.h mb_f.h
