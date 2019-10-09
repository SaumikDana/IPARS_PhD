# tecpc.mak - Makefile for tecwell.EXE - IBMPC version

# Usage:
#        NMAKE -F tecpc.mak          (production version)
#        NMAKE -F tecpc.mak DEBUG=1  (debug version)

EXENAM=tecwell.exe
OBJS=tecwell.obj


FORT=c:\msdev\bin\fl32.exe
LINK=c:\msdev\bin\link.exe
LIBS=c:\msdev\lib\kernel32.lib

!if "$(DEBUG)"=="1"

FFLAGS=/Zi /c /nologo /G4 /ML /Fd"tecwell.pdb" 
LFLAGS=/debug /nologo /pdb:"tecwell.pdb"

!else

FFLAGS=/c /nologo /G4 /ML /Ox
LFLAGS=/nologo

!endif

.SUFFIXES:
.SUFFIXES:.obj .f

.f.obj:
   $(FORT) $(FFLAGS) $<  

$(EXENAM) : $(OBJS)
   $(LINK) @<<
   $(LFLAGS) $(OBJS)
<<
