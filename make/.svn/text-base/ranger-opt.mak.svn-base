# lonestar.mak - Machine and compiler make include file for TACC Cray-Dell 
# Linux Clusters (lonestar IA-32 system)
# Xiuli Gai, 11/24/0

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

#######################################################
#### add paths to include files and libs necessary for parallel jobs
#PARALINC = -I/opt/MPI/intel9/mvapich/0.9.7/lib/
PARALINC = -I$(MPICH_HOME)/include/
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

LFLAGS  = -lcxa -lg2c -lunwind -lm -lstdc++ -Wl,-rpath,$(TACC_MKL_LIB) 
FFLAGS = -O2 -c -mcmodel=medium -Duse_netCDF $(PARALINC) $(MACE_FFLAGS) \
          $(DAGHMB_FFLAGS) $(VIS_FFLAGS)
CFLAGS = -DBEOWULF -D__IFC -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN \
         -mcmodel=medium -O2 -Wcheck -c $(PARALINC) $(MACE_CFLAGS) \
          $(DAGHMB_CFLAGS) $(VIS_CFLAGS)
CPPFLAGS = -DBEOWULF -DWant_c_files -DDEBUG_PRINT -Wno-deprecated \
           -DNO_OSTREAMWITHASSIGN -O2 -Wcheck -c $(PARALINC) $(MACE_CPPFLAGS) \
           $(DAGHMB_CPPFLAGS)
###########################################################
# system used by mortar.mak and mace.mak, macesysflag by mace.mak

MBSYSFLAG  = -DBEOWULF 
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
SYSTEM    = lonestar
MACESYSFLAG = -DBEOWULF -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

########################################################### BLAS/LAPACK
# LPACK library

##LAPACK_LIB = -libmace.a
LAPACK_LIB = -L$(TACC_MKL_LIB) -lmkl -lmkl_lapack64 -lmkl_em64t -Vaxlib -lpthread -lguide
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

