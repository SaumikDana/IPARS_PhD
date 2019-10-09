# x2eth.mak - Machine and compiler make include file for a pc cluster
# that runs linux and uses ethernet communications

ARCH     = Linux 
CC       = mpicc
FORT     = mpif90 
LINK     = mpif90
LFLAGS   = -O -lm /usr/pgi/linux86/lib/libpgftnrtl.a  
FFLAGS   = -c  -O
CFLAGS   = -c  -O -B
SYSLIB   =

mpif.h:
	cp /usr/mpich-gm/include/mpif.h $(WORK)
#	cp /usr/mpich-gm/build/LINUX/ch_p4/include/mpif.h $(WORK)

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
