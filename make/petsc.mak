# petsc.mak - PETSC linear solver make include file
# This file is not portable

# Library files ######################################################

PETSC_DIR  = /u4/sbalay/petsc-2.0.17
PETSC_ARCH = rs6000
BOPT       = O

FC_LIB     = /usr/lib/libxlf.a /usr/lib/libxlf90.a \
  -bI:/usr/lpp/xlf/lib/lowsys.exp
BLAS_LIB   = -lblas $(FC_LIB)
LAPACK_LIB = /pds/lib/liblapack.a

MPI_DIR    = /u4/sbalay/mpich
X11_LIB    = -lX11

SOLVELIB = -L$(PETSC_DIR)/lib/lib$(BOPT)/$(PETSC_ARCH)\
  -lpetscfortran -lpetscts -lpetscsnes  -lpetscsles \
  -lpetscmat -lpetscvec -lpetscsys $(X11_LIB) \
  $(LAPACK_LIB) $(BLAS_LIB) $(FC_LIB) -lm -lmpe

# Source files #######################################################

PETSC_FINCLUDE = $(PETSC_DIR)/include/finclude

mpif.h : /u4/sbalay/mpich/include/mpif.h
	$(COPYIT)

petsc.h : $(PETSC_FINCLUDE)/petsc.h
	$(COPYIT)

mat.h : $(PETSC_FINCLUDE)/mat.h
	$(COPYIT)

vec.h : $(PETSC_FINCLUDE)/vec.h
	$(COPYIT)

sles.h : $(PETSC_FINCLUDE)/sles.h
	$(COPYIT)
