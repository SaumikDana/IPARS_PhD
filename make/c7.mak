# sl6.mak - Machine and compiler make file for Scientific Linux 6, multiple processors

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

# In /opt/apps/ossw/libraries/mpich2/mpich2-3.1.4/c7/intel-16.0/bin/

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90
LINK     = mpif90

#### path of various solver libraries

ifeq ($(MPI_IMPLEMENTATION),openmpi)
  HYPRE_DIR  = /h1/bganis/hypre-2.11.1-openmpi/src
else
  HYPRE_DIR  = /org/centers/csm/hypre-2.10.0b
endif

SUPERLU_CFLAGS = -DAdd_ -DUSE_VENDOR_BLAS -I/h1/bganis/SuperLU_DIST_3.3/SRC
SUPERLU_LFLAGS = /h1/bganis/SuperLU_DIST_3.3/lib/libsuperlu_dist_3.3.a \
                 -L/h1/bganis/parmetis-4.0.3/build/Linux-x86_64/libparmetis -lparmetis \
                 -L/h1/bganis/parmetis-4.0.3/build/Linux-x86_64/libmetis -lmetis

VTK_INC = /opt/apps/ossw/libraries/vtk/vtk-6.3.0/c7/intel-16.0/include/vtk-6.3
VTK_LIB = /opt/apps/ossw/libraries/vtk/vtk-6.3.0/c7/intel-16.0/lib
VTKPATH = $(VTK_LIB)
VTKINCL = $(VTK_INC)
VTKLIBS = -lvtkCommonCore-6.3 -lvtkCommonDataModel-6.3 -lvtkIOXML-6.3

DEALII_DIR=/h2/shlee/Software/deal.II-8.4.1
DEALII_TRILINOS_DIR=/h2/shlee/Software/trilinos-11.4.1
DEALII_P4EST_DIR=/h2/shlee/Software/FAST

DEALII_CFLAGS = -DTBB_IMPLEMENT_CPP0X=1 -Ddeal_II_dimension=2 \
-I$(DEALII_DIR)/include \
-I$(DEALII_DIR)/include/deal.II/bundled \
-I$(DEALII_TRILINOS_DIR)/include \
-I$(DEALII_P4EST_DIR)/include \
-fpic -ansi -w2 -wd68 -wd135 -wd175 -wd177 -wd191 -wd193 -wd279 -wd327 -wd383 \
-wd981 -wd1418 -wd1478 -wd1572 -wd2259 -wd21 -wd2536 -wd15531 -wd111 -wd128 -wd185 \
-wd280 -qopenmp-simd -std=c++14 -Wno-return-type -O0 -no-ansi-alias -ip

DEALII_LFLAGS = -shared-intel -rdynamic -fuse-ld=gold -qopenmp  \
-L$(DEALII_DIR)/lib -ldeal_II \
-L$(DEALII_TRILINOS_DIR)/lib \
-lstratimikos -lstratimikosbelos -lstratimikosaztecoo -lstratimikosamesos -lstratimikosml \
-lstratimikosifpack -lbelostpetra -lbelosepetra -lbelos -lml -lifpack -lamesos -lgaleri-xpetra \
-lgaleri -laztecoo -lisorropia -lthyraepetra -lthyracore -lxpetra-sup -lxpetra-ext -lxpetra \
-lepetraext -ltpetraext -ltpetrainout -ltpetra -ltriutils -lzoltan -lepetra -lkokkosdisttsqr \
-lkokkosnodetsqr -lkokkoslinalg -lkokkosnodeapi -lkokkos -lrtop -lsacado -ltpi \
-lteuchosremainder -lteuchosnumerics -lteuchoscomm -lteuchosparameterlist -lteuchoscore \
-lboost_iostreams-mt -lboost_serialization-mt -lboost_system-mt -lboost_thread-mt \
-L$(DEALII_P4EST_DIR)/lib -lp4est -lsc \
-limf -lipgo -lirc -lsvml -lirc_s -ldl -lc -lbz2 -ltbb -lz \
-Wl,-rpath,$(DEALII_DIR)/lib:$(DEALII_TRILINOS_DIR)/lib:$(DEALII_P4EST_DIR)/lib

LFLAGS   = -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) $(LIBS_HYPRE) \
           $(SUPERLU_LFLAGS) $(TECLIB) $(TRILINOS_LIBS)
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
	rm -f size
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

