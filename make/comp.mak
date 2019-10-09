# comp.mak - Compositional make include file

# Object files #######################################################

COMPOBJ=xisdat$(O) xiadat$(O) xtdata$(O) xarray$(O) xivdat$(O) \
  xstep$(O) xstdout$(O) xwdata$(O) xrest$(O) xquit$(O) xpore$(O) \
  xflash$(O) xplace$(O) xprop$(O) xdiff$(O) xtran$(O) xwell$(O) \
  xthermal$(O) xutil$(O) xgendat$(O)

VISUALOBJ=xvisual$(O) 
DCOMPOBJ=xdual$(O)

XYGMRESOBJ = xsprb3$(O)

# Source files #######################################################

SORC=..$(S)comp$(S)

xarydat.h: $(SORC)xarydat.h
	$(SETSIZE1)
	$(SIZEIT)

xbaldat.h: $(SORC)xbaldat.dh
	$(SETSIZE1)
	$(SIZEIT)

xcompwel.h: $(SORC)xcompwel.dh
	$(SETSIZE1)
	$(SIZEIT)

xgendat.f: $(SORC)xgendat.df
	$(SETSIZE1)
	$(SIZEIT)

xthermal.h: $(SORC)xthermal.dh
	$(SETSIZE1)
	$(SIZEIT)

xiter.h: $(SORC)xiter.h
	$(COPYIT)

xmodel.h: $(SORC)xmodel.dh
	$(SETSIZE1)
	$(SIZEIT)

xparam.h: $(SORC)xparam.h
	$(COPYIT)

xwells.h: $(SORC)xwells.dh
	$(SETSIZE1)
	$(SIZEIT)

xresprop.h: $(SORC)xresprop.h
	$(COPYIT)

xisdat.f: $(SORC)xisdat.df control.h unitsex.h xcompwel.h xgendat.f  \
        xthermal.h xresprop.h xmodel.h xparam.h xiter.h xbaldat.h  \
        blkary.h scrat1.f
	$(SETSIZE1)
	$(SIZEIT)

xisdat$(O) : scrat1$(O)

xisdat$(O) : xgendat$(O)

xiadat.f: $(SORC)xiadat.df control.h unitsex.h blkary.h xarydat.h \
        xgendat.f xparam.h xmodel.h xresprop.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

xtdata.f: $(SORC)xtdata.df control.h xgendat.f xbaldat.h xparam.h
	$(SETSIZE1)
	$(SIZEIT)

xarray.f: $(SORC)xarray.df control.h xarydat.h msjunk.h xmodel.h \
        blkary.h
	$(SETSIZE1)
	$(SIZEIT)

xivdat.f: $(SORC)xivdat.df control.h blkary.h times.h unitsex.h xarydat.h \
	xmodel.h xgendat.f xthermal.h wells.h xcompwel.h msjunk.h xiter.h \
        xresprop.h xparam.h xbaldat.h
	$(SETSIZE1)
	$(SIZEIT)

xstep.f: $(SORC)xstep.df control.h blkary.h wells.h xarydat.h xparam.h \
	 xbaldat.h unitsex.h msjunk.h xwells.h xgendat.f xmodel.h xiter.h
	$(SETSIZE1)
	$(SIZEIT)

xstdout.f: $(SORC)xstdout.df control.h xarydat.h blkary.h xgendat.f \
        xparam.h xmodel.h xbaldat.h unitsex.h xcompwel.h xthermal.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)
	
xwdata.f: $(SORC)xwdata.df control.h wells.h xcompwel.h xparam.h \
          xmodel.h unitsex.h blkary.h xresprop.h xwells.h xiter.h
	$(SETSIZE1)
	$(SIZEIT)
	
xquit.f: $(SORC)xquit.df control.h 
	$(SETSIZE1)
	$(SIZEIT)
	
xpore.f: $(SORC)xpore.df control.h layout.h xparam.h msjunk.h xmodel.h
	$(SETSIZE1)
	$(SIZEIT)
	
xrest.f: $(SORC)xrest.df control.h 
	$(SETSIZE1)
	$(SIZEIT)
	
xflash.f: $(SORC)xflash.df xparam.h xiter.h xresprop.h wells.h control.h \
        xarydat.h msjunk.h xwells.h xgendat.f xthermal.h
	$(SETSIZE1)
	$(SIZEIT)
	
xplace.f: $(SORC)xplace.df control.h xparam.h xmodel.h xbaldat.h
	$(SETSIZE1)
	$(SIZEIT)
	
xprop.f: $(SORC)xprop.df control.h xparam.h xmodel.h rock.h \
        xbaldat.h xiter.h
	$(SETSIZE1)
	$(SIZEIT)
	
xdiff.f: $(SORC)xdiff.df control.h layout.h blkary.h xmodel.h
	$(SETSIZE1)
	$(SIZEIT)

xtran.f: $(SORC)xtran.df control.h xparam.h xmodel.h layout.h xbaldat.h
	$(SETSIZE1)
	$(SIZEIT)
	
xwell.f: $(SORC)xwell.df control.h xparam.h xmodel.h xwells.h msjunk.h \
         layout.h xcompwel.h xgendat.f wells.h xiter.h xresprop.h
	$(SETSIZE1)
	$(SIZEIT)

xthermal.f: $(SORC)xthermal.df control.h blkary.h layout.h xarydat.h xparam.h \
	xmodel.h 
	$(SETSIZE1)
	$(SIZEIT)

xutil.f: $(SORC)xutil.df control.h rock.h xparam.h xmodel.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

xvisual.h: $(SORC)xvisual.dh 
	$(SETSIZE1)
	$(SIZEIT)

xvisual.f: $(SORC)xvisual.df xvisual.h xarydat.h control.h visual.h rock.h \
	layout.h xbaldat.h
	$(SETSIZE1)
	$(SIZEIT)


xdual.f: $(SORC)xdual.df control.h blkary.h xarydat.h sblkc.h xmodel.h \
	xparam.h layout.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)
