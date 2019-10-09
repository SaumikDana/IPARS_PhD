# rs6k.mak - Machine and compiler make include file

ARCH   = rs6000
CC     = cc
FORT   = xlf
LINK   = xlf
LFLAGS = 
FFLAGS = -c -O3
CFLAGS = -c -qcpluscmt -O3
SYSLIB =
OBJST    = $(OBJS) cputime.o

.f.o:
	$(FORT) $(FFLAGS) $*.f

.c.o:
	$(CC) $(CFLAGS) -P $*.c
	$(CC) $(CFLAGS) $*.i

$(EXENAM): $(OBJST)
	$(LINK) $(OBJST) $(LFLAGS) -o $(EXENAM) $(SYSLIB) $(LIBS)

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(EXENAM)
