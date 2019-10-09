#-----------------------------------------------------------------------
# visual.mak - Make include file for the visualization routines
#-----------------------------------------------------------------------

# Library files ######################################################
GRAPHLIB = 

# Object files #######################################################

# The model dependent files are defined in VISUALOBJ

GOBJA = $(VISUALOBJ) visout$(O) vistab$(O) visgrid$(O) visual$(O) 
GOBJB = visual1$(O) visual2$(O) visual3$(O) visual4$(O) visual5$(O) \
        visual6$(O) visual9$(O)
GOBJC = visslice$(O) wellvis$(O) vistec$(O)

GRAPHOBJ = $(GOBJA) $(GOBJB) $(GOBJC)

# Source files #######################################################

VINCL = ..$(S)visual$(S)includes$(S)
VSORC = ..$(S)visual$(S)source$(S)
TSORC = ..$(S)visual$(S)tecplot$(S)

visual.h : $(VINCL)visual.dh
	$(SETSIZE1)
	$(SIZEIT)

vistab.h : $(VINCL)vistab.dh
	$(SETSIZE1)
	$(SIZEIT)

# from old visual.mak

vistabc.h: $(VINCL)vistabc.h
	$(COPYIT)

vistab.f: $(TSORC)vistab.df control.h vistabc.h
	$(SETSIZE1)
	$(SIZEIT)

visgrid.f: $(TSORC)visgrid.df control.h vistabc.h layout.h wells.h \
           msjunk.h rockpg.h
	$(SETSIZE1)
	$(SIZEIT)

# end old visual.mak

visualc.h : $(VINCL)visualc.dh
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual.c : $(VSORC)visual.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

vistec.c : $(TSORC)vistec.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual1.c : $(TSORC)visual1.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual2.c : $(TSORC)visual2.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual3.c : $(TSORC)visual3.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visipol.c : $(VSORC)visipol.c
	$(COPYIT)

visual4.c : $(TSORC)visual4.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual5.c : $(TSORC)visual5.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual6.c : $(TSORC)visual6.dc visualc.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual9.f : $(TSORC)visual9.df visual.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visout.f: $(VSORC)visout.df visual.h control.h layout.h \
             unitsex.h visual.h vistab.h
	$(SETSIZE1)
	$(SIZEIT)

wellvis.f: $(VSORC)wellvis.df 
	$(SETSIZE1)
	$(SIZEIT)

vis_sli.h: $(VINCL)vis_sli.dh
	$(SETSIZE1)
	$(SIZEIT)

visslice.f: $(VSORC)visslice.df vis_sli.h
	$(SETSIZE1)
	$(SIZEIT)

visual$(O): visual.c visualc.h cfsimple.h

vistec$(O): vistec.c visualc.h cfsimple.h

visual1$(O): visual1.c visualc.h cfsimple.h

visual2$(O): visual2.c visualc.h cfsimple.h

visual3$(O): visual3.c visualc.h cfsimple.h

visipol$(O): visipol3.c visualc.h cfsimple.h

visual5$(O): visual5.c visualc.h cfsimple.h

visual6$(O): visual6.c visualc.h cfsimple.h

visual9$(O): visual9.f visual.h

################### Post-processing tools

SCRIPT = ..$(S)visual$(S)scripts$(S)

postvis: slip scripts

slip.c: $(SCRIPT)slip.dc
	$(SETSIZE1)
	$(SIZEIT)

slip: slip.c
	$(CC) -o slip slip.c

scripts: 
	ln -fs $(SCRIPT)mteccos .
	ln -fs $(SCRIPT)mteccoa .
	ln -fs $(SCRIPT)tecco .
	ln -fs $(SCRIPT)mtecsli .
	ln -fs $(SCRIPT)mtectab .
	ln -fs $(SCRIPT)TEC*.src .

