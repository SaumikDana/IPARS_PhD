# lonestar.mak - Machine and pgi-compiler make file for TACC-Ranger 
# Linux Clusters (lonestar IA-32 system)

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

#######################################################
#### add paths to include files and libs necessary for parallel jobs
PARALINC = -I$(MPICH_HOME)/include/
RANGER_PGILAPACK = /share/apps/pgi/7.1/linux86-64/7.1-2/lib/
#####################################################################
# IMPORTANT: must correct location of PV3 libs and include files
#####################################################################
# location of PV3 library libPV3.a and of PVM libraries
PV3LIBD = -L/$(HOME)/PV3 -L$(PVM_ROOT)/lib/$(PVM_ARCH) 

# location of pV3.h
PV3INCD = $(HOME)/PV3

# location of mpiPV3.h
PV3INCP = $(HOME)/PV3

# location of NetMPI.c
PV3INCS = $(HOME)/PV3

# do not change this ... 
NetMPI.c : $(PV3INCS)/NetMPI.c 
	$(COPYIT1)

######################################################## COMPILERS and FLAGS
#### define intel compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90
LINK     = mpif90

LFLAGS   = -lg2c -lm -lstdc++ 
FFLAGS   = -DBEOWULF -DWant_c_files -DNO_OSTREAMWITHASSIGN -g -traceback -c  -Duse_netCDF \
            $(PARALINC) $(MACE_FFLAGS) $(DAGHMB_FFLAGS) $(VIS_FFLAGS)
CFLAGS   = -DBEOWULF -D__IFC -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN -g -traceback -c  \
            $(PARALINC) $(MACE_CFLAGS) $(DAGHMB_CFLAGS) $(VIS_CFLAGS)
CPPFLAGS = -DBEOWULF -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN -g -traceback -c \
            $(PARALINC) $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS)
###########################################################
# system used by mortar.mak and mace.mak, macesysflag by mace.mak

MBSYSFLAG   = -DBEOWULF 
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
SYSTEM      = ranger
MACESYSFLAG = -DBEOWULF -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

########################################################### BLAS/LAPACK
# LPACK library

#LAPACK_LIB = -libmace.a
LAPACK_LIB = -L$(RANGER_PGILAPACK) -lblas -llapack
OBJST    = $(OBJS) cputime.o
############################################################
# define the default dependencies

.f.o:
	$(FORT) $(FFLAGS) $*.f

%.o:%.f90
	$(FORT90) -c $(FFLAGS90) -o $@ $<

.c.o:
	$(CC) $(CFLAGS) $*.c

.C.o:
	$(CPP) $(CPPFLAGS) $*.C

.cpp.o:
	$(CPP) $(CPPFLAGS) $*.cpp

$(EXENAM): $(OBJST) $(LIBS_TO_STAMP) 
	$(LINK) $(OBJST) -o $(EXENAM) $(LFLAGS) $(LIBS) 

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
	rm -f $(EXENAM)

