# x2.mak - Machine and compiler make include file for a pc cluster
# that runs linux and uses myrinet communications

ARCH     = Linux 
CC       = mpicc
FORT     = mpif90 
LINK     = mpif90
LFLAGS   = -g -lm /usr/pgi/linux86/lib/libpgftnrtl.a  
FFLAGS   = -c  -O
CFLAGS   = -c  -O -B
SYSLIB   =

mpif.h:
	cp /usr/mpich-eth/build/LINUX/ch_p4/include/mpif.h $(WORK)

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
