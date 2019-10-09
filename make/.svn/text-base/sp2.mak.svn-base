# sp2.mak - Machine and compiler make include file

ARCH     = sp2
CC       = mpcc
FORT     = mpxlf
LINK     = mpxlf
LFLAGS   = -g -bloadmap:ipars.map
FFLAGS   = -c -g
CFLAGS   = -c -qcpluscmt -g
SYSLIB   =

BLAS = -lesslp2

.f.o:
	$(FORT) $(FFLAGS) $*.f

.c.o:
	$(CC) $(CFLAGS) $*.c

$(EXENAM): $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS) -o $(EXENAM) $(SYSLIB) $(LIBS) -lmpi 

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(EXENAM)
