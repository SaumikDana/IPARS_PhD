# blacki.mak - Implicit black oil make include file

# Object files #######################################################

BLACKIOBJ=iidata$(O) iarray$(O) iivdat$(O) istep$(O) iprop$(O) \
  istdout$(O) iiwell$(O) irest$(O) itran$(O)

MODELOBJ = $(BLACKIOBJ)

VISUALOBJ=ivisual$(O)       
IVISOBJ=ivisual$(O)       

DBLACKIOBJ=idual$(O)

IYGMRESOBJ = isprb3$(O)

# Source files #######################################################

SORC=..$(S)blacki$(S)

iarydat.h: $(SORC)iarydat.h
	$(COPYIT)

ibaldat.h: $(SORC)ibaldat.dh
	$(SETSIZE1)
	$(SIZEIT)

ifluid.h: $(SORC)ifluid.dh
	$(SETSIZE1)
	$(SIZEIT)

iidata.f: $(SORC)iidata.df control.h unitsex.h ifluid.h ibaldat.h iarydat.h
	$(SETSIZE1)
	$(SIZEIT)

iarray.f: $(SORC)iarray.df control.h iarydat.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

iivdat.f: $(SORC)iivdat.df control.h blkary.h times.h layout.h iarydat.h \
	ibaldat.h ifluid.h rock.h wells.h scrat1.f msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

iivdat$(O): scrat1$(O)

istep.f: $(SORC)istep.df control.h blkary.h wells.h iarydat.h layout.h \
	 ibaldat.h unitsex.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

iprop.f: $(SORC)iprop.df control.h ibaldat.h ifluid.h
	$(SETSIZE1)
	$(SIZEIT)

istdout.f: $(SORC)istdout.df control.h iarydat.h blkary.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

iiwell.f: $(SORC)iiwell.df control.h wells.h ifluid.h ibaldat.h layout.h \
	 unitsex.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)
 
irest.f: $(SORC)irest.df control.h wells.h ibaldat.h restc.h iarydat.h
	$(SETSIZE1)
	$(SIZEIT)

itran.f : $(SORC)itran.df control.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

ivisual.h: $(SORC)ivisual.dh
	$(SETSIZE1)
	$(SIZEIT)

ivisual.f: $(SORC)ivisual.df ivisual.h iarydat.h control.h visual.h rock.h \
	layout.h ifluid.h ibaldat.h blkary.h
	$(SETSIZE1)
	$(SIZEIT)

idual.f: $(SORC)idual.df control.h blkary.h iarydat.h sblkc.h ifluid.h \
	layout.h ibaldat.h rock.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)
