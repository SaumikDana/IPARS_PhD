# metis.mak - Metis include file

# Object files #######################################################

METISOBJ=metis$(O)

METISPATH=$(S)home$(S)saumik$(S)metis_lib

METISLIB=-L$(METISPATH) -lmetis

# Source files #######################################################

SORC = ..$(S)memman$(S)

metis.h: $(SORC)metis.h
	$(COPYIT)

metis.c: $(SORC)metis.dc memory.h metis.h
	$(SETSIZE1)
	$(SIZEIT)

metis$(O): metis.c 

