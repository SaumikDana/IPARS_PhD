#TRILINOS_BUILD=$(HOME)/Trilinos/sandbox3/build-cmake-ia64-mpi
TRILINOS_BUILD=/org/centers/csm/gergina/Trilinos/build-mpich2
include $(TRILINOS_BUILD)/packages/teuchos/Makefile.export.Teuchos 
include $(TRILINOS_BUILD)/packages/epetra/Makefile.export.Epetra 
include $(TRILINOS_BUILD)/packages/tpetra/Makefile.export.Tpetra 
include $(TRILINOS_BUILD)/packages/ml/Makefile.export.ML
include $(TRILINOS_BUILD)/packages/aztecoo/Makefile.export.AztecOO
 
CXX=$(Epetra_CXX_COMPILER)
CXXFLAGS=-g $(Epetra_CXX_FLAGS) $(AztecOO_INCLUDE_DIRS) $(ML_INCLUDE_DIRS) $(Epetra_INCLUDE_DIRS) $(Tpetra_INCLUDE_DIRS) $(Teuchos_INCLUDE_DIRS) -DUNIT_TEST
#F77=$(Epetra_FORTRAN_COMPILER)
#FFLAGS=-g #-Wall

LD=$(CXX)
LDFLAGS=$(AztecOO_LIBRARY_DIRS) $(ML_LIBRARY_DIRS) $(Epetra_LIBRARY_DIRS) \
        $(Tpetra_LIBRARY_DIRS) $(Teuchos_LIBRARY_DIRS) $(AztecOO_LIBRARIES) \
	    $(ML_LIBRARIES) $(Epetra_LIBRARIES) $(Teuchos_LIBRARIES)  \
	    $(Epetra_TPL_LIBRARIES) $(Epetra_EXTRA_LD_FLAGS) \
        -lmpi -lmpi_f90 -lmpi_cxx \
        -L/usr/lib/gcc/x86_64-redhat-linux/4.1.2 -lstdc++ -lgcc
#-lgfortran

all: unit_test


unit_test: trilinos.o
	$(LD) *.o -o $@ $(LDFLAGS) 

#%.o: %.f
#	$(F77) $(FFLAGS) -c $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.exe *.o

