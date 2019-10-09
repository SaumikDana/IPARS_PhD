# osx.mak - Machine and compiler make include file for Mac OS X

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90
LINK     = mpif90

#### path of hypre code and library

#HYPRE_DIR  = /Users/Shared/hypre-2.10.0b/src/hypre
#HYPRE_DIR  = /Users/Shared/hypre-2.11.0/src/hypre
HYPRE_DIR  = /Users/Shared/hypre-2.11.1/src/hypre

SUPERLU_CFLAGS = -DAdd_ -DUSE_VENDOR_BLAS -I/Users/Shared/SuperLU_DIST_3.3/SRC
SUPERLU_LFLAGS = /Users/Shared/SuperLU_DIST_3.3/lib/libsuperlu_dist_3.3.a \
                 -L/Users/Shared/parmetis-4.0.3/build/Darwin-x86_64/libparmetis -lparmetis \
                 -L/Users/Shared/parmetis-4.0.3/build/Darwin-x86_64/libmetis -lmetis

# vtk-6.2
VTKPATH = /usr/local/VTK-6.2/lib
VTKINCL = /usr/local/VTK-6.2/include/vtk-6.2
VTKLIBS = -lvtkCommonCore-6.2 -lvtkCommonDataModel-6.2  \
          -lvtkCommonExecutionModel-6.2 -lvtkCommonSystem-6.2 \
          -lvtkCommonMath-6.2 -lvtkCommonMisc-6.2 \
          -lvtkCommonTransforms-6.2 -lvtksys-6.2 \
          -lvtkIOCore-6.2 -lvtkIOXML-6.2 -lvtkzlib-6.2

# vtk-7.0
#VTKPATH = /usr/local/VTK-7.0/lib
#VTKINCL = /usr/local/VTK-7.0include/vtk-7.0
#VTKLIBS = -lvtkCommonCore-7.0 -lvtkCommonDataModel-7.0 -lvtkIOXML-7.0

LFLAGS   =  -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) $(LIBS_HYPRE) \
            -Wl,-w $(SUPERLU_LFLAGS) $(TECLIB)
FFLAGS   = -c -Duse_netCDF $(MACE_FFLAGS) $(DAGHMB_FFLAGS)
FFLAGS90 = -c 
CFLAGS   = -c -DSAMG_UNIX_LINUX \
           -DSAMG_LCASE_USCORE  \
            $(MACE_CFLAGS) $(DAGHMB_CFLAGS) $(SUPERLU_CFLAGS) 
CPPFLAGS = -c -DSAMG_UNIX_LINUX \
           -DSAMG_LCASE_USCORE \
            $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS) 

ifdef DEBUG_FLAGS
  FFLAGS   += -g -CB -O0 -fpic -Wl,-no_pie -CB -traceback -fp-stack-check -fpe0
  FFLAGS90 += -g -CB -O0 -fpic -Wl,-no_pie -CB -traceback -fp-stack-check -fpe0
  CFLAGS   += -g -O0 -fpic -Wl,-no_pie -traceback -fp-stack-check
  CPPFLAGS += -g -O0 -fpic -Wl,-no_pie -traceback -fp-stack-check
else
#  FFLAGS   += -O3 -unroll-aggressive
#  FFLAGS90 += -O3 -unroll-aggressive
#  CFLAGS   += -O3
#  CPPFLAGS += -O3
  FFLAGS   += -O2 -unroll-aggressive
  FFLAGS90 += -O2 -unroll-aggressive
  CFLAGS   += -O2
  CPPFLAGS += -O2
endif

###########################################################
# arch and mbsysflag used only by mortar.mak
ARCH      = Linux 
MBSYSFLAG  = -DLINUX 
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
MACESYSFLAG = -DLINUX -DWant_c_files -DDEBUG_PRINT  -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

############################################################
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)
#########

#MKL_LIB = /usr/bin/ifort-2011-base/mkl/lib
#MKL_LIB = /opt/intel/composer_xe_2015/mkl/lib/
MKL_LIB = ${MKLROOT}/lib
#BLAS_LIB = -L${MKL_LIB} \
#         -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
BLAS_LIB = ${MKL_LIB}/libmkl_intel_lp64.a \
           ${MKL_LIB}/libmkl_sequential.a \
           ${MKL_LIB}/libmkl_core.a
LAPACK_LIB = ${MKL_LIB}/libmkl_lapack95_ilp64.a
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

$(EXENAM): size logstamp $(OBJST) $(LIBS_TO_STAMP)
	$(LINK) $(OBJST) -o $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) $(LIBAMG)
	rm -f logstamp
	date > logstamp
	touch logstamp

size:
	$(FORT) -o size ../size/size.f

logstamp: 
	rm -f logstamp 
	date > logstamp

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.f90
	rm -f $(WORK)/*.mod
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
	rm -f $(WORK)/dum
	rm -f $(EXENAM)
	rm -f size
	rm -f logstamp
