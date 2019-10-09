# beo.mak - Machine and compiler make include file for beowulf / wonderland

#### reset SIZE so that the make would pick up error of SIZE preprocessor
SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

#######################################################
#### add paths to include files and libs necessary for parallel jobs

PARALINC = -I/bgsys/drivers/ppcfloor/comm/include/

######################################################## COMPILERS and FLAGS
#### define compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
LINK     = mpif90

VTKPATH = /gpfs/DDNgpfs3/xkthomassu/vtk/lib/vtk-5.4
VTKINCL = /gpfs/DDNgpfs3/xkthomassu/vtk/include/vtk-5.4
VTKFLAGS = -dynamic
VTKLIBS = -lvtkRendering -lvtkGraphics -lvtkGeovis -lvtkIO -lvtkexpat \
          -lvtkpng -lvtktiff -lvtkjpeg -lvtkFiltering -lvtkViews \
          -lvtkCommon -lpthread -lvtkmetaio -lvtkzlib \
          -lvtksys -lvtkImaging -lvtkftgl -lvtkfreetype -lvtkverdict \
          -lvtkDICOMParser -lvtkNetCDF -lvtksqlite -lvtkWidgets -lvtkproj4 \
          -lvtkInfovis -lvtkHybrid -lvtkParallel -lvtkexoIIc -lvtklibxml2 \
          -lvtkalglib -Wl,-rpath-link /usr/X11R6/lib -Wl,-rpath-link /usr/lib \
          -L/usr/lib/libGL.so -L/usr/X11R6/lib/libX11.so -L/usr/X11R6/lib/libXt.so \
          -L/usr/X11R6/lib/libSM.so -L/usr/X11R6/lib/libICE.so

# in the above, g++ will also work: mpi libraries are added in PARALLLIB

LFLAGS   = -O2 -lm -lstdc++ $(MACE_LFLAGS) $(DAGHMB_LFLAGS) -Wl,--allow-multiple-definition
FFLAGS   = -O2 -c $(PARALINC) $(MACE_FFLAGS) $(DAGHMB_FFLAGS)
CFLAGS   = -O2 -c -D_IBMR2 $(PARALINC) $(MACE_CFLAGS) $(DAGHMB_CFLAGS)
CPPFLAGS   = -O2 -c -D_IBMR2 $(PARALINC) $(MACE_CPPFLAGS) $(DAGHMB_CPPFLAGS)

########################################################### MBlock
SYSTEM    = bgene
MACESYSFLAG = -DBEOWULF -DWant_c_files -DDEBUG_PRINT 
############################################################

########################################################### BLAS/LAPACK
# use libblas.a etc. for static libs (faster, default) 
# or lblas, llapack for dynamically linked libs (smaller executable)

BLAS_LIB = -L/opt/ibmmath/essl/4.4/lib -lesslbg 
OBJST    = $(OBJS) cputime.o
############################################################
# define the default dependencies

.f.o:
	$(FORT) $(FFLAGS) $*.f

.c.o:
	$(CC) $(CFLAGS) $*.c

.C.o:
	$(CPP) $(CPPFLAGS) $*.C

.cpp.o:
	$(CPP) $(CPPFLAGS) $*.cpp

$(EXENAM): $(OBJST) $(LIBS_TO_STAMP) 
	$(LINK) $(OBJST) -o $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) 

clean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.C
	rm -f $(WORK)/*.cpp
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -fr $(WORK)/ii_files/
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(EXENAM)

# sclean removes sources but leaves the executable
sclean:
	rm -f $(WORK)/*.f
	rm -f $(WORK)/*.c
	rm -f $(WORK)/*.C
	rm -f $(WORK)/*.cpp
	rm -f $(WORK)/*.h
	rm -f $(WORK)/*.o
	rm -f $(WORK)/*.i
	rm -fr $(WORK)/ii_files/
	rm -f $(WORK)/*.lst
	rm -f $(WORK)/ech
	rm -f $(WORK)/dum*

