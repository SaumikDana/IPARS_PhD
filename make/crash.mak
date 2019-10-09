# crash.mak

#### reset SIZE so that the make would pick up error of SIZE preprocessor

SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

#### define intel compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90
LINK     = mpif90

.PHONY: $(EXENAM) size crash clean

default: crash

crash:
	@echo "Error: set SYSTEM environment variable or change makefile"
	@false

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.C
	rm -f $(WORK)/*.cpp
	rm -f $(WORK)/*.hpp
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -fr $(WORK)/ii_files/
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(WORK)/dum*
	rm -f $(WORK)/*.f90
	rm -f $(WORK)/*.mod
	rm -f $(EXENAM)
	rm -f size
