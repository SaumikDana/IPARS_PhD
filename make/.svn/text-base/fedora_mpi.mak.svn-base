# lnx.mak - Machine and compiler make include file for Linux, single processor

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

PARALINC = /opt/mpich/intel/1.2.7p1/include
######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90 
LINK     = mpif90
#
LFLAGS   =  -lm -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) 
FFLAGS   = -c  -g -L$(PARALINC) $(MACE_FFLAGS) $(DAGHMB_FFLAGS)
CFLAGS   = -c  -g -L$(PARALINC) $(MACE_CFLAGS) $(DAGHMB_CFLAGS) 
CPPFLAGS   = -c -g -L$(PARALINC) $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS)

# Added for Matt's model
FFLAGS90 = -g
INCLUDES = -I
#LIBAMG=/lusr/samg/32bit/libamg_ifc71-p4_r6.so

###########################################################
# arch and mbsysflag used only by mortar.mak
ARCH      = Linux 
MBSYSFLAG  = -DLINUX 
#-DDAGH_NO_MPI
# system used by mortar.mak and mace.mak, macesysflag by mace.mak
SYSTEM    = linux
MACESYSFLAG = -DLINUX -DWant_c_files -DDEBUG_PRINT  -DNO_OSTREAMWITHASSIGN -DANSI_HEADERS

############################################################
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)
#########
BLAS_LIB = -L/opt/intel/mkl/8.1.1/lib/em64t -lmkl_def -lmkl_lapack -lguide -lvml -lpthread
LAPACK_LIB = /opt/intel/mkl/8.1.1/lib/em64t/libmkl_lapack.a
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

$(EXENAM):  logstamp $(OBJS) $(LIBS_TO_STAMP) 
#	$(LINK) $(OBJS) -o  $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) /usr/lib/libf2c.a
#	$(LINK) $(OBJS) -o  $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) -L/usr/lib/libg2c.so \
	-L/opt/intel/fce/9.1.036/lib -Bstatic -lifport -lifcore -limf \
	-Bdynamic -lcxa -lirc -lunwind -lc -lirc_s
	$(LINK) $(OBJS) -o  $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) -L/usr/lib/libg2c.so \
	-L/opt/mpich/intel/1.2.7p1/lib -lmpich -lfmpich -lpmpich++ 
	rm -f logstamp 
	date > logstamp
#	touch logstamp

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







