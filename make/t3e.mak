# t3e.mak - Machine and compiler make include file

CC       = cc
FORT     = f90
LINK     = f90
LFLAGS   = -g
FFLAGS   = -c -g -dp -Rbn
CFLAGS   = -c -qcpluscmt -g
SYSLIB   = -lX11

.f.o:
	$(FORT) $(FFLAGS) $*.f

.c.o:
	$(CC) $(CFLAGS) $*.c

$(EXENAM): $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS) -o $(EXENAM) $(SYSLIB) $(LIBS)

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(EXENAM)
