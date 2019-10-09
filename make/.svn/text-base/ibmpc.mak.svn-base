# ibmpc.mak - Machine and compiler make include file

# make DEBUG=1         Builds debug program

FORT=fl32.exe
CC=cl.exe
CPP=cl.exe
LINK=c:\progra~1\vs\vc98\bin\link.exe
SYSLIB=kernel32.lib

!if "$(DEBUG)"=="1"

# FFLAGS=/Zi /c /nologo /G4 /ML /Fd"ipars.pdb"
FFLAGS=/Zi /c /nologo /G4 /ML /Fs
CFLAGS=/c /Gz /G4 /Gs /nologo /Zi /W3
LFLAGS=/debug /nologo /pdb:"ipars.pdb"

!else

FFLAGS=/c /nologo /G4 /ML /Ox
CFLAGS=/c /Gz /G4 /Gs /nologo
LFLAGS=/nologo /map:ipars.map

!endif

.SUFFIXES:
.SUFFIXES: .obj .f .c .h .df .dc .dh .in

.f.obj:
   $(FORT) $(FFLAGS) $*.f >>trash

.c.obj:
   $(CC) $(CFLAGS) $*.c >>trash

$(EXENAM):$(OBJS)
	$(LINK) >> TRASH @<<
	$(OBJS)
	/OUT:$(EXENAM) $(LFLAGS)
	$(SYSLIB) $(LIBS)
<<

clean:
	del $(WORK)\*.f
	del $(WORK)\*.c
	del $(WORK)\*.h
	del $(WORK)\*.obj
	del $(WORK)\ech
	del $(WORK)\trash
	del $(EXENAM)
cleanup:
	del $(WORK)\*.f
	del $(WORK)\*.c
	del $(WORK)\*.h
	del $(WORK)\*.obj
	del $(WORK)\ech
	del $(WORK)\trash