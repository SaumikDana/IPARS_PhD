# ubuntu.mak - Machine and compiler make file for Linux, single processor

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

CC       = icc
CPP      = icpc
FORT     = ifort
FORT90   = ifort 
LINK     = ifort
#
LFLAGS   = -i_dynamic -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) 
DBGFLAGS = -g -traceback -fp-stack-check -O0
FFLAGS   = -c $(DBGFLAGS) -CB -fpe0 -shared-intel -mcmodel=medium \
           -Duse_netCDF $(MACE_FFLAGS) $(DAGHMB_FFLAGS)
FFLAGS90 = $(DBGFLAGS)
CFLAGS   = -c $(DBGFLAGS) -shared-intel -mcmodel=medium \
           -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE  $(MACE_CFLAGS) $(DAGHMB_CFLAGS) 
CPPFLAGS = -c $(DBGFLAGS) -shared-intel -mcmodel=medium \
           -DSAMG_UNIX_LINUX -DSAMG_LCASE_USCORE  $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS)
INCLUDES = -I

###########################################################
# arch and mbsysflag used only by mortar.mak
ARCH      = Linux 
MBSYSFLAG  = -DLINUX -DDAGH_NO_MPI
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
SYSTEM    = linux
MACESYSFLAG = -DLINUX -DWant_c_files -DDEBUG_PRINT  -DACE_NO_MPI -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

############################################################
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)
#########
BLAS_LIB = -L/opt/intel/Compiler/11.1/073/mkl/lib/em64t \
         -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
LAPACK_LIB = /opt/intel/Compiler/11.1/073/mkl/lib/em64t/libmkl_lapack95_ilp64.a
OBJST    = $(OBJS) cputime.o

#for SAMG library
#LIBAMG = /opt/apps/samg/64bit/libamg.so
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
	-L/usr/lib/libg2c.so 
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







