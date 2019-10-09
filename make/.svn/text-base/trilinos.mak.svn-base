# Trilinos solver makefile

# Library files ######################################################

ifdef DEBUG_FLAGS
  TRILINOS_BUILD = /org/centers/csm/gergina/Trilinos/build.10.12.2-hack-sl6-11.1
else
  TRILINOS_BUILD = /org/centers/csm/gergina/Trilinos/build.10.12.2-hack-opt-sl6-11.1
endif

include $(TRILINOS_BUILD)/packages/teuchos/Makefile.export.Teuchos
include $(TRILINOS_BUILD)/packages/epetra/Makefile.export.Epetra
include $(TRILINOS_BUILD)/packages/tpetra/Makefile.export.Tpetra
include $(TRILINOS_BUILD)/packages/ml/Makefile.export.ML
include $(TRILINOS_BUILD)/packages/aztecoo/Makefile.export.AztecOO

TRILINOS_CPPFLAGS=$(Epetra_CXX_FLAGS) $(AztecOO_INCLUDE_DIRS) $(ML_INCLUDE_DIRS) \
            $(Epetra_INCLUDE_DIRS) $(Tpetra_INCLUDE_DIRS) $(Teuchos_INCLUDE_DIRS)

TRILINOS_LIBS=$(AztecOO_LIBRARY_DIRS) $(ML_LIBRARY_DIRS) $(Epetra_LIBRARY_DIRS) \
        $(Tpetra_LIBRARY_DIRS) $(Teuchos_LIBRARY_DIRS) $(AztecOO_LIBRARIES) \
        $(ML_LIBRARIES) $(Epetra_LIBRARIES) $(Teuchos_LIBRARIES)

# Object files #######################################################

TRILINOSOBJ = trilinos$(O) trilinos_driver$(O)

# Source files #######################################################

SORC = ..$(S)solve$(S)trilinos$(S)

trilinos.h: $(SORC)trilinos.dh
	$(SETSIZE1)
	$(SIZEIT)

trilinos.cpp: $(SORC)trilinos.cpp
	$(COPYIT)

trilinos_driver.f: $(SORC)trilinos_driver.df
	$(SETSIZE1)
	$(SIZEIT)

trilinos_driver$(O): control.h layout.h blkary.h trilinos.h

