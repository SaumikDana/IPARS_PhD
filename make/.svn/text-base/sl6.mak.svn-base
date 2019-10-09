# sl6.mak - Machine and compiler make file for Scientific Linux 6, multiple processors

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

HYPRE_DIR  = /h1/gergina/hypre-2.9.0b/src/hypre-sl6-$(INTEL_VERSION)

SUPERLU_CFLAGS = -DAdd_ -DUSE_VENDOR_BLAS -I/h1/bganis/SuperLU_DIST_3.3/SRC
SUPERLU_LFLAGS = /h1/bganis/SuperLU_DIST_3.3/lib/libsuperlu_dist_3.3.a \
                 -L/h1/bganis/parmetis-4.0.3/build/Linux-x86_64/libparmetis -lparmetis \
                 -L/h1/bganis/parmetis-4.0.3/build/Linux-x86_64/libmetis -lmetis

VTKPATH = $(VTK_LIB)
VTKINCL = $(VTK_INC)
VTKLIBS = -lvtkIO -lvtkFiltering -lvtkCommon

LFLAGS   = -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) $(LIBS_HYPRE) \
           $(SUPERLU_LFLAGS) $(TECLIB)
FFLAGS   = -c $(MACE_FFLAGS) $(DAGHMB_FFLAGS) \
            -shared-intel -mcmodel=medium $(FDEFS)
CFLAGS   = -c -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE \
            $(MACE_CFLAGS) $(DAGHMB_CFLAGS) $(DISCOVER_CFLAGS) $(SUPERLU_CFLAGS)
CPPFLAGS = -c -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE \
            $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS) $(DISCOVER_CPPFLAGS) \
            $(TRILINOS_CPPFLAGS)

# DEBUG_FLAGS should be defined in work directory makefile.
ifdef DEBUG_FLAGS
  FFLAGS   += -g -O0 -traceback -fp-stack-check -CB -fpe0
  FFLAGS90 += -g -O0 -traceback -fp-stack-check -CB -fpe0
  CFLAGS   += -g -O0 -traceback -fp-stack-check
  CPPFLAGS += -g -O0 -traceback -fp-stack-check
else
  FFLAGS   += -O3 -unroll-aggressive
  FFLAGS90 += -O3 -unroll-aggressive
  CFLAGS   += -O3
  CPPFLAGS += -O3
endif

###########################################################
# arch and mbsysflag used only by mortar.mak
ARCH      = Linux 
MBSYSFLAG  = -DLINUX 
#-DDAGH_NO_MPI
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
MACESYSFLAG = -DLINUX -DWant_c_files -DDEBUG_PRINT  -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS \
              -DDISCOVER

############################################################
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)
#########
# defined in mkl module
BLAS_LIB = $(BLAS_LIBS)
LAPACK_LIB = $(LAPACK_LIBS)
OBJST    = $(OBJS) cputime.o

#for SAMG library
#if using the serial SAMG
#LIBAMG = -L$(AMGHOME) -lamg
# if using the parallel SAMGp - NOT yet in repo
#LIBAMG = -L$(AMGHOME)$(S)samg -lamg -L$(AMGHOME)$(S)mpi -lmpistubs
#
############################################################


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
	$(LINK) $(OBJST) -o  $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) \
	-L/usr/lib/libg2c.so
	rm -f logstamp
	date > logstamp
	touch logstamp
	chmod g+w * 2>/dev/null || true

size:
	$(FORT) -o size ../size/size.f

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
	rm -f $(WORK)/*.f90
	rm -f $(WORK)/*.mod
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



