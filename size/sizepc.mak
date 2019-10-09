# sizepc.mak - Makefile for SIZE.EXE - IBMPC version

# Usage:
#        Execute in a "work" directory
#        NMAKE -F ..\size\sizepc.mak          (production version)
#        NMAKE -F ..\size\sizepc.mak DEBUG=1  (debug version)

SORC=..\size\size.f
EXENAM=size.exe
OBJS=size.obj

FORT=c:\msdev\bin\fl32.exe
LINK=c:\msdev\bin\link.exe
LIBS=c:\msdev\lib\kernel32.lib

!if "$(DEBUG)"=="1"

FFLAGS=/Zi /c /nologo /G5 /ML /Fd"size.pdb" 
LFLAGS=/debug /nologo /pdb:"size.pdb"

!else

FFLAGS=/c /nologo /G5 /ML /Ox
LFLAGS=/nologo

!endif

.SUFFIXES:
.SUFFIXES:.obj .f

$(SORC)$(EXENAM):
   $(FORT) $(FFLAGS) $(SORC)
   $(LINK) @<<
   $(LFLAGS) $(OBJS)
<<
