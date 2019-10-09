# beo.mak - Machine and compiler make include file

ARCH     = Linux 
CC       = pgcc
FORT     = pg77 -I/usr/mpich/include/
LINK     = mpif77
LFLAGS   = -g -lf2c -lm 
FFLAGS   = -c  -O -fno-second-underscore 
CFLAGS   = -c  -O
SYSLIB   =

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
