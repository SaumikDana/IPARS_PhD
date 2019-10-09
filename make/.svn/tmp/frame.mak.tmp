# frame.mak - Framework make include file

########################## Object files #############################

FDRIV=driver_f$(O) 
FOBJA=scrat1$(O) ipars$(O) read1$(O) read2$(O) units$(O) \
      comp$(O) table$(O) idata$(O)
FOBJB=extvar$(O) memman1$(O) memman2$(O) divide$(O) timer$(O) prtout$(O)
FOBJC=tdata$(O) stdout$(O) initial$(O) iwell$(O) owell$(O) prop$(O) restart$(O)
FOBJD=meminfo$(O) memman3$(O) ccallc$(O) rockutil$(O) bdaryin$(O) bdaryout$(O) 
FOBJE=bdutil$(O) readdatc$(O)
FLIB=lib_f$(O)

FRAMELIBOBJ=$(FLIB) $(FOBJA) $(FOBJB) $(FOBJC) $(FOBJD) $(FOBJE)
FRAMEOBJ=$(FDRIV) $(FOBJA) $(FOBJB) $(FOBJC) $(FOBJD) $(FOBJE)

###################### Source files ##################################

UTIL = ..$(S)util$(S)

msjunk.h: ..$(S)drive$(S)msjunk.h
	$(COPYIT)

ctypes.h: $(UTIL)ctypes.dh
	$(SETSIZE1)
	$(SIZEIT)

cfsimple.h: $(UTIL)cfsimple.h
	$(COPYIT1)	

control.h: ..$(S)drive$(S)control.dh mcontrol.h
	$(SETSIZE)
	$(SIZEIT)

mcontrol.h: ..$(S)drive$(S)mcontrol.dh 
	$(SETSIZE1)
	$(SIZEIT)

scrat2.h: ..$(S)input$(S)scrat2.dh
	$(SETSIZE)
	$(SIZEIT)

readdat.h: ..$(S)input$(S)readdat.dh
	$(SETSIZE)
	$(SIZEIT)

blkary.h: ..$(S)input$(S)blkary.dh
	$(SETSIZE)
	$(SIZEIT)

rock.h: ..$(S)input$(S)rock.dh
	$(SETSIZE)
	$(SIZEIT)

output.h: ..$(S)output$(S)print$(S)output.dh
	$(SETSIZE)
	$(SIZEIT)

compc.h: $(UTIL)compc.h
	$(COPYIT)

unitsex.h: ..$(S)input$(S)unitsex.h
	$(COPYIT)

restc.h: ..$(S)output$(S)restart$(S)restc.h
	$(COPYIT)

utldat.h: $(UTIL)utldat.dh
	$(SETSIZE)
	$(SIZEIT)

memory.h: ..$(S)memman$(S)memory.dh ctypes.h
	$(SETSIZE)
	$(SIZEIT)

layout.h: ..$(S)memman$(S)layout.dh
	$(SETSIZE)
	$(SIZEIT)

times.h: $(UTIL)times.dh 
	$(SETSIZE)
	$(SIZEIT)

wells.h: ..$(S)wells$(S)wells.dh
	$(SETSIZE)
	$(SIZEIT)

rockpg.h: $(UTIL)rockpg.h
	$(COPYIT)

scrat1.f: ..$(S)input$(S)scrat1.df
	$(SETSIZE1)
	$(SIZEIT)

driver_f.f: ..$(S)drive$(S)driver_f.f
	$(COPYIT)

lib_f.f: ..$(S)drive$(S)lib_f.f
	$(COPYIT)

ipars.f: ..$(S)drive$(S)ipars.df layout.h control.h scrat1.f blkary.h \
	wells.h msjunk.h output.h restc.h
	$(SETSIZE)
	$(SIZEIT)

ipars$(O): scrat1$(O)

idata.f: ..$(S)input$(S)idata.df layout.h control.h rock.h blkary.h \
	output.h unitsex.h utldat.h msjunk.h times.h readdat.h
	$(SETSIZE)
	$(SIZEIT)

tdata.f: ..$(S)input$(S)tdata.df control.h layout.h output.h unitsex.h restc.h
	$(SETSIZE)
	$(SIZEIT)

read1.f: ..$(S)input$(S)read1.df scrat1.f control.h readdat.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

read1$(O): scrat1$(O)

read2.f: ..$(S)input$(S)read2.df scrat1.f control.h layout.h readdat.h
	$(SETSIZE)
	$(SIZEIT)

read2$(O): scrat1$(O)

units.f: ..$(S)input$(S)units.f
	$(COPYIT)

divide.f: ..$(S)memman$(S)divide.df layout.h control.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

comp.f: $(UTIL)comp.df control.h output.h compc.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

table.f: $(UTIL)table.df utldat.h output.h control.h scrat2.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

prtout.f: ..$(S)output$(S)print$(S)prtout.df control.h layout.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

stdout.f: ..$(S)output$(S)print$(S)stdout.df control.h layout.h output.h unitsex.h
	$(SETSIZE)
	$(SIZEIT)

timer.f: $(UTIL)timer.df times.h
	$(SETSIZE)
	$(SIZEIT)

initial.f: $(UTIL)initial.df control.h layout.h blkary.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

iwell.f: ..$(S)wells$(S)iwell.df control.h wells.h blkary.h layout.h msjunk.h unitsex.h
	$(SETSIZE)
	$(SIZEIT)

owell.f: ..$(S)wells$(S)owell.df control.h wells.h utldat.h
	$(SETSIZE)
	$(SIZEIT)

prop.f: $(UTIL)prop.df control.h utldat.h rock.h wells.h blkary.h layout.h rockpg.h
	$(SETSIZE)
	$(SIZEIT)

restart.f: ..$(S)output$(S)restart$(S)restart.df layout.h control.h blkary.h output.h msjunk.h restc.h
	$(SETSIZE)
	$(SIZEIT)

extvar.c: $(UTIL)extvar.dc compc.h ctypes.h
	$(SETSIZE)
	$(SIZEIT)

memman1.c: ..$(S)memman$(S)memman1.dc memory.h
	$(SETSIZE)
	$(SIZEIT)

memman2.c: ..$(S)memman$(S)memman2.dc memory.h
	$(SETSIZE)
	$(SIZEIT)

memman3.c : ..$(S)memman$(S)memman3.dc memory.h cfsimple.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

meminfo.c: ..$(S)memman$(S)meminfo.c cfsimple.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

ccallc.f: $(UTIL)ccallc.df
	$(SETSIZE1)
	$(SIZEIT)

rockutil.f: $(UTIL)rockutil.df rock.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

boundary.h: $(UTIL)boundary.dh
	$(SETSIZE1)
	$(SIZEIT)

bdaryin.f: $(UTIL)bdaryin.df boundary.h control.h
	$(SETSIZE1)
	$(SIZEIT)

bdaryout.f: $(UTIL)bdaryout.df boundary.h control.h
	$(SETSIZE1)
	$(SIZEIT)

bdutil.c: $(UTIL)bdutil.c
	$(COPYIT1)
	$(CC2ANSI)

readdatc.c: $(UTIL)readdatc.dc 
	$(SETSIZE1)
	$(SIZEIT)

cputime.c: $(UTIL)cputime.dc
	$(SETSIZE)
	$(SIZEIT)
