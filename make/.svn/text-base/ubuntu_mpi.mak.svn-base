# ubuntu_mpi.mak - Machine and compiler make file for Linux, multiple processors

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

MPI_HOME = /h1/bganis/mpich2-1.3.1
PARALINC = $(MPI_HOME)$(S)include
PARALLIB  = -L$(MPI_HOME)$(S)lib
######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

CC       = ${MPI_HOME}/bin/mpicc
CPP      = ${MPI_HOME}/bin/mpicxx
FORT     = ${MPI_HOME}/bin/mpif90
FORT90   = ${MPI_HOME}/bin/mpif90
LINK     = ${MPI_HOME}/bin/mpif90

#### path of hypre code and library
HYPRE_DIR  = ../solve/hypre
#HYPRE_DIR_LIB =  /h1/gxue/IPARSv3.1-MFMFE/solve/hypre
FINCLUDES  = -I$(HYPRE_DIR)/include
FDEFS      = -DHAVE_CONFIG_H -DHYPRE_TIMING
#LINKOPTS   = -g -pedantic -Wall
LINKOPTS   = -g
#LIBS_HYPRE = -L$(HYPRE_DIR)/lib -lHYPRE -lg2c -lm
#LIBS_HYPRE = -L$(HYPRE_DIR)/lib -lHYPRE -lg2c
#2.0.0
#LIBS_HYPRE = -L$(HYPRE_DIR)/lib -lHYPRE -lHYPRE_LSI
#2.7.0
#LIBS_HYPRE = -L$(HYPRE_DIR_LIB)/lib -lHYPRE
LIBS_HYPRE = -L$(HYPRE_DIR)/lib -lHYPRE



#
#LFLAGS   =  -lm -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) 
#FFLAGS   = -c -g -CB -mcmodel=large -Duse_netCDF -L$(PARALINC) $(MACE_FFLAGS) $(DAGHMB_FFLAGS)
#FFLAGS90 = -g
#CFLAGS   = -c -g -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE -L$(PARALINC) \
#            $(MACE_CFLAGS) $(DAGHMB_CFLAGS) 
#CPPFLAGS = -c -g -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE -L$(PARALINC) \
#            $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS)

LFLAGS   = -i_dynamic -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) 
FFLAGS   = -c $(PARALINC) $(MACE_FFLAGS) $(DAGHMB_FFLAGS) \
	    -shared-intel -mcmodel=medium
CFLAGS   = -c -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE -L$(PARALINC) \
            $(MACE_CFLAGS) $(DAGHMB_CFLAGS) 
CPPFLAGS = -c -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE -L$(PARALINC) \
            $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS) $(TRILINOS_CPPFLAGS)

ifdef DEBUG_FLAGS
  FFLAGS   += -g -CB -traceback -fp-stack-check -O0 -fpe0
  CFLAGS   += -g
  CPPFLAGS += -g
else
  FFLAGS   := $(FFLAGS) -O3 -vec-report -unroll-aggressive
  CFLAGS   := $(CFLAGS) -O3
  CPPFLAGS := $(CPPFLAGS) -O3
endif

INCLUDES = -I

###########################################################
# arch and mbsysflag used only by mortar.mak
ARCH      = Linux 
MBSYSFLAG  = -DLINUX 
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
SYSTEM    = linux-64mpich2
MACESYSFLAG = -DLINUX -DWant_c_files -DDEBUG_PRINT -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

############################################################
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)
#########
BLAS_LIB = -L/opt/intel/Compiler/11.1/073/mkl/lib/em64t \
         -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
LAPACK_LIB = /opt/intel/Compiler/11.1/073/mkl/lib/em64t/libmkl_lapack95_ilp64.a
OBJST    = $(OBJS) cputime.o

#for SAMG library
#LIBAMG = /opt/apps/samg/old64bit/libamg.so
LIBAMG = /opt/local/samg/old64bit/libamg.so
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

$(EXENAM):  logstamp $(OBJST) $(LIBS_TO_STAMP) 
	$(LINK) $(OBJST) -o  $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) $(LIBAMG)  \
        -L/usr/lib/libg2c.so \
        $(PARALLIB) -lmpich       
	rm -f logstamp 
	date > logstamp

logstamp: 
	rm -f logstamp 
	date > logstamp

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
	rm -f $(EXENAM)
	rm -f logstamp 

# sclean removes sources but leaves the executable
sclean:
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







