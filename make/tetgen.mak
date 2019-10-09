# tetgen.mak - TetGen include file

# Object files #######################################################

TETGENOBJ=tetcall$(O) tetgen$(O) predicates$(O)

# Source files #######################################################

SORC = ..$(S)memman$(S)

tetgen.h: $(SORC)tetgen.h
	$(COPYIT)

tetcall$(O): $(SORC)tetcall.cpp tetgen.h
	mpicxx -O3 -c $(SORC)tetcall.cpp 

tetgen$(O): $(SORC)tetgen.cpp tetgen.h
	mpicxx -O0 -c $(SORC)tetgen.cpp

predicates$(O): $(SORC)predicates.cpp tetgen.h
	mpicxx -O0 -c $(SORC)predicates.cpp


