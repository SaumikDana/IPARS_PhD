#-----------------------------------------------------------------------
# visvtk.mak - Make include file for the VTK visualization routines
#-----------------------------------------------------------------------

GRAPHLIB += -Wl,-rpath,$(VTKPATH) -L$(VTKPATH) $(VTKLIBS) 

# Object files #######################################################

# The model dependent files are defined in VISUALOBJ

GRAPHOBJ += vtkmem$(O) visvtk$(O) visual7$(O) visual8$(O)

# Source files #######################################################

VISVTKINCL = ..$(S)visual$(S)includes$(S)
SORCVTK = ..$(S)visual$(S)vtk$(S)
CPPXTRAFLGS = -D_FILE_OFFSET_BITS=64

visvtk.h : $(VISVTKINCL)visvtk.dh
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

vtkmem.c : $(SORCVTK)vtkmem.dc cfsimple.h visualc.h memory.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visvtk.c : $(SORCVTK)visvtk.dc visualc.h cfsimple.h
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual7.cpp : $(SORCVTK)visual7.dcpp visualc.h visvtk.h 
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

visual8.cpp : $(SORCVTK)visual8.dcpp visualc.h visvtk.h 
	$(SETSIZE1)
	$(SIZEIT)
	$(CC2ANSI)

vtkmem$(O): vtkmem.c memory.h

visvtk$(O): visvtk.c visualc.h cfsimple.h

visual7$(O) : visual7.cpp visualc.h cfsimple.h visvtk.h
	$(CPP) $(CPPXTRAFLGS) $(CPPFLAGS) -I. -I$(VTKINCL) -Wno-deprecated visual7.cpp

visual8$(O) : visual8.cpp visualc.h cfsimple.h visvtk.h
	$(CPP) $(CPPXTRAFLGS) $(CPPFLAGS) -I. -I$(VTKINCL) -Wno-deprecated visual8.cpp
