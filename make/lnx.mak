# lnx.mak - Machine and compiler make include file

ARCH     = Linux 
CC       = gcc
FORT     = g77
LINK     = g++
LFLAGS   = -lm -lf2c
#FFLAGS   = -c  -O -fno-second-underscore
#CFLAGS   = -c  -O
FFLAGS   = -c  -g -fno-second-underscore
CFLAGS   = -c  -g
SYSLIB   =
OBJST    = $(OBJS) cputime.o

.f.o:
	$(FORT) $(FFLAGS) $*.f

.c.o:
	$(CC) $(CFLAGS) $*.c

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

