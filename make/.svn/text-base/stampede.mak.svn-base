# stampede.mak

#### reset SIZE so that the make would pick up error of SIZE preprocessor

SIZEIT= rm -f dum?; $(SIZE) < ech 2> dum1; cat dum1; grep 0 dum1 > dum2;

#### define intel compilers, linker etc.

CC       = mpicc
CPP      = mpicxx
FORT     = mpif90
FORT90   = mpif90
LINK     = mpif90

#### path of hypre code and library
#HYPRE_DIR  = $(TACC_HYPRE_LIB)
HYPRE_DIR = /home1/01028/bganis/hypre-2.10.0b/src

SUPERLU_CFLAGS = -DAdd_ -DUSE_VENDOR_BLAS -I/home1/01028/bganis/SuperLU_DIST_3.3/SRC
SUPERLU_LFLAGS = /home1/01028/bganis/SuperLU_DIST_3.3/lib/libsuperlu_dist_3.3.a \
                 -L/home1/01028/bganis/parmetis-4.0.3/build/Linux-x86_64/libparmetis -lparmetis \
                 -L/home1/01028/bganis/parmetis-4.0.3/build/Linux-x86_64/libmetis -lmetis

#VTKPATH = /home1/01777/gurpreet/VTK-7.0/lib
#VTKINCL = /home1/01777/gurpreet/VTK-7.0/include/vtk-7.0
#VTKLIBS =  -lvtkCommonDataModel-7.0 -lvtkIOXML-7.0 \
           -lvtkCommonTransforms-7.0 -lvtkCommonMisc-7.0 \
           -lvtkCommonExecutionModel-7.0 -lvtkCommonDataModel-7.0 \
           -lvtkCommonCore-7.0 -lvtksys-7.0 -lvtkCommonMath-7.0 \
           -lvtkIOCore-7.0 -lvtkCommonSystem-7.0 -lvtkzlib-7.0

VTKPATH = /home1/01028/bganis/VTK-6.2/lib
VTKINCL = /home1/01028/bganis/VTK-6.2/include/vtk-6.2
VTKLIBS = -lvtkalglib-6.2 -lvtkChartsCore-6.2 -lvtkCommonColor-6.2 \
          -lvtkCommonComputationalGeometry-6.2 -lvtkCommonCore-6.2 -lvtkCommonDataModel-6.2 \
          -lvtkCommonExecutionModel-6.2 -lvtkCommonMath-6.2 -lvtkCommonMisc-6.2 -lvtkCommonSystem-6.2 \
          -lvtkCommonTransforms-6.2 -lvtkDICOMParser-6.2 -lvtkDomainsChemistry-6.2 -lvtkexoIIc-6.2 \
          -lvtkexpat-6.2 -lvtkFiltersAMR-6.2 -lvtkFiltersCore-6.2 -lvtkFiltersExtraction-6.2 \
          -lvtkFiltersFlowPaths-6.2 -lvtkFiltersGeneral-6.2 -lvtkFiltersGeneric-6.2 \
          -lvtkFiltersGeometry-6.2 -lvtkFiltersHybrid-6.2 -lvtkFiltersHyperTree-6.2 \
          -lvtkFiltersImaging-6.2 -lvtkFiltersModeling-6.2 -lvtkFiltersParallel-6.2 \
          -lvtkFiltersParallelImaging-6.2 -lvtkFiltersProgrammable-6.2 -lvtkFiltersSelection-6.2 \
          -lvtkFiltersSMP-6.2 -lvtkFiltersSources-6.2 -lvtkFiltersStatistics-6.2 -lvtkFiltersTexture-6.2 \
          -lvtkFiltersVerdict-6.2 -lvtkfreetype-6.2 -lvtkftgl-6.2 -lvtkGeovisCore-6.2 -lvtkgl2ps-6.2 \
          -lvtkhdf5_hl-6.2 -lvtkhdf5-6.2 -lvtkImagingColor-6.2 -lvtkImagingCore-6.2 \
          -lvtkImagingFourier-6.2 -lvtkImagingGeneral-6.2 -lvtkImagingHybrid-6.2 -lvtkImagingMath-6.2 \
          -lvtkImagingMorphological-6.2 -lvtkImagingSources-6.2 -lvtkImagingStatistics-6.2 \
          -lvtkImagingStencil-6.2 -lvtkInfovisCore-6.2 -lvtkInfovisLayout-6.2 -lvtkInteractionImage-6.2 \
          -lvtkInteractionStyle-6.2 -lvtkInteractionWidgets-6.2 -lvtkIOAMR-6.2 -lvtkIOCore-6.2 \
          -lvtkIOEnSight-6.2 -lvtkIOExodus-6.2 -lvtkIOExport-6.2 -lvtkIOGeometry-6.2 -lvtkIOImage-6.2 \
          -lvtkIOImport-6.2 -lvtkIOInfovis-6.2 -lvtkIOLegacy-6.2 -lvtkIOLSDyna-6.2 -lvtkIOMINC-6.2 \
          -lvtkIOMovie-6.2 -lvtkIONetCDF-6.2 -lvtkIOParallel-6.2 -lvtkIOParallelXML-6.2 -lvtkIOPLY-6.2 \
          -lvtkIOSQL-6.2 -lvtkIOVideo-6.2 -lvtkIOXML-6.2 -lvtkIOXMLParser-6.2 -lvtkjpeg-6.2 \
          -lvtkjsoncpp-6.2 -lvtklibxml2-6.2 -lvtkmetaio-6.2 -lvtkNetCDF_cxx-6.2 -lvtkNetCDF-6.2 \
          -lvtkoggtheora-6.2 -lvtkParallelCore-6.2 -lvtkpng-6.2 -lvtkproj4-6.2 \
          -lvtkRenderingAnnotation-6.2 -lvtkRenderingContext2D-6.2 -lvtkRenderingContextOpenGL-6.2 \
          -lvtkRenderingCore-6.2 -lvtkRenderingFreeType-6.2 -lvtkRenderingFreeTypeOpenGL-6.2 \
          -lvtkRenderingGL2PS-6.2 -lvtkRenderingImage-6.2 -lvtkRenderingLabel-6.2 -lvtkRenderingLIC-6.2 \
          -lvtkRenderingLOD-6.2 -lvtkRenderingOpenGL-6.2 -lvtkRenderingVolume-6.2 \
          -lvtkRenderingVolumeOpenGL-6.2 -lvtksqlite-6.2 -lvtksys-6.2 -lvtktiff-6.2 -lvtkverdict-6.2 \
          -lvtkViewsContext2D-6.2 -lvtkViewsCore-6.2 -lvtkViewsInfovis-6.2 -lvtkzlib-6.2

DEALII_DIR=/work/04398/mj23366/Software/dealii/dealii-8.4.1
DEALII_TRILINOS_DIR=$(TACC_TRILINOS_DIR)
DEALII_P4EST_DIR=$(P4EST_DIR)/FAST

DEALII_CFLAGS = -DTBB_IMPLEMENT_CPP0X=1 -Ddeal_II_dimension=2 \
-I$(DEALII_DIR)/build/include \
-I$(DEALII_DIR)/include \
-I$(DEALII_DIR)/bundled/tbb41_20130401oss/include \
-I$(DEALII_DIR)/bundled/umfpack/UMFPACK/Include \
-I$(DEALII_DIR)/bundled/umfpack/AMD/Include \
-I$(DEALII_DIR)/bundled/boost-1.56.0/include \
-I$(DEALII_DIR)/bundled/muparser_v2_2_4/include \
-I$(DEALII_TRILINOS_DIR)/include \
-I/opt/apps/intel15/mvapich2_2_1/phdf5/1.8.16/x86_64/include \
-I/opt/apps/intel15/boost/1.55.0/x86_64/include \
-I/opt/apps/intel15/mvapich2_2_1/parallel-netcdf/4.3.3.1/x86_64/include \
-I$(DEALII_P4EST_DIR)/include \
-pedantic -fpic -Wall -Wextra -Wpointer-arith -Wwrite-strings \
-Wsign-compare -Wswitch -Woverloaded-virtual -Wno-long-long \
-std=c++14 -Wno-parentheses -fstrict-aliasing 

#-I/opt/apps/intel/15/composer_xe_2015.2.164/mkl/include \
#-I/opt/apps/intel15/mvapich2/2.1/include \

DEALII_LFLAGS = -rdynamic -Wl,-rpath,/opt/apps/intel/15/composer_xe_2015.2.164/compiler/lib/intel64 \
-Wl,-rpath,/opt/apps/intel/15/composer_xe_2015.2.164/compiler/lib/intel64 \
-Wl,-rpath  -Wl,/opt/apps/intel15/mvapich2/2.1/lib \
-Wl,--enable-new-dtags \
-Wl,-rpath,/work/04398/mj23366/Software/dealii/dealii-8.4.1/build/lib:/opt/apps/intel15/mvapich2/2.1/lib:/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib:/opt/apps/intel15/mvapich2_2_1/parallel-netcdf/4.3.3.1/x86_64/lib:/opt/apps/intel15/mvapich2_2_1/phdf5/1.8.16/x86_64/lib:/opt/apps/intel15/mvapich2_2_1/p4est/1.1/FAST/lib \
-L$(DEALII_DIR)/build/lib -ldeal_II -lbz2 -ldl \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos_muelu.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos_ifpack2.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos_amesos2.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos_tpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos_sacado.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstokhos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmoochothyra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmoocho.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/librythmos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmuelu-adapters.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmuelu-interface.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmuelu.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/liblocathyra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/liblocaepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/liblocalapack.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libloca.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libnoxepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libnoxlapack.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libnox.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libphalanx.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libintrepid.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteko.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libfei_trilinos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libfei_base.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikosbelos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikosaztecoo.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikosamesos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikosml.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libstratimikosifpack.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libifpack2-adapters.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libifpack2.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libanasazitpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libModeLaplace.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libanasaziepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libanasazi.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libmapvarlib.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libfastqlib.a \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libblotlib.a \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libplt.a \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libsvdi_cgi.a \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libsvdi_cdr.a \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libsuplib.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libsupes.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libaprepro_lib.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libchaco.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIonit.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIotr.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIohb.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIogn.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIopg.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIoexo_fac.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIopx.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIofx.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIoex.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libIoss.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libnemesis.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libexodus_for.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libexodus.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libamesos2.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libbelostpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libbelosepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libbelos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libml.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libifpack.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libzoltan2.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libpamgen_extras.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libpamgen.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libamesos.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libgaleri-xpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libgaleri-epetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libaztecoo.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libisorropia.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libxpetra-sup.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libxpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libthyratpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libthyraepetraext.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libthyraepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libthyracore.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libepetraext.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetraext.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetrainout.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libkokkostsqr.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetrakernels.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetraclassiclinalg.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetraclassicnodeapi.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtpetraclassic.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libtriutils.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libshards.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libzoltan.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libepetra.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libsacado.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/librtop.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchoskokkoscomm.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchoskokkoscompat.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchosremainder.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchosnumerics.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchoscomm.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchosparameterlist.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libteuchoscore.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libkokkosalgorithms.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libkokkoscontainers.so \
/home1/apps/intel15/mvapich2_2_1/trilinos/12.6.4/lib/libkokkoscore.so \
/opt/apps/intel15/mvapich2_2_1/parallel-netcdf/4.3.3.1/x86_64/lib/libnetcdf.so \
/opt/apps/intel15/mvapich2_2_1/phdf5/1.8.16/x86_64/lib/libhdf5.so -lz -lmkl_sequential \
/opt/apps/intel15/mvapich2/2.1/lib/libmpicxx.so -lrt \
/opt/apps/intel15/mvapich2_2_1/p4est/1.1/FAST/lib/libp4est.so \
/opt/apps/intel15/mvapich2_2_1/p4est/1.1/FAST/lib/libsc.so \
-lmkl_intel_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lgfortran -lquadmath -lc 

#/opt/apps/intel15/mvapich2/2.1/lib/libmpifort.so -lX11 /opt/apps/intel15/mvapich2/2.1/lib/libmpi.so

DATASPACES_DIR=/work/01871/melrom/DSinstall

LFLAGS   = -lstdc++ $(LIBS_HYPRE) $(SUPERLU_LFLAGS) $(TECLIB)
FFLAGS   = -c -shared-intel -mcmodel=medium $(FDEFS)
CFLAGS   = -c -mcmodel=medium $(SUPERLU_CFLAGS)
CPPFLAGS  = -c

#DEBUG_FLAGS=1

ifdef DEBUG_FLAGS
  FFLAGS   += -g -CB -traceback -fp-stack-check -O0 -fpe0
  CFLAGS   += -g -Wcheck -O0
  CPPFLAGS += -g -Wcheck -O0
else
  FFLAGS   += -O3 -unroll-aggressive
  CFLAGS   += -O3
  CPPFLAGS += -O3
#  FFLAGS   += -O2 -unroll-aggressive
#  CFLAGS   += -O2
#  CPPFLAGS += -O2
endif

########################################################### BLAS/LAPACK
# LAPACK library

BLAS_LIB = -L$(TACC_MKL_LIB) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
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
	$(LINK) $(OBJST) -o $(EXENAM) $(LFLAGS) $(LIBS) $(BLAS_LIB) $(TRILINOS_LIBS)
	rm -f logstamp
	date > logstamp
	touch logstamp
	chmod g+rw * 2>/dev/null || true

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
 
