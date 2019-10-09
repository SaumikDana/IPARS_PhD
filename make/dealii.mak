# deal.ii makefile

# Library files ######################################################

IPARS_LIB_OBJ = ipars_dealii$(O)

DRIVER_OBJ = driver$(O)

DEALII_OBJ = IPACS-V3.0$(O)

IPARS_DS_OBJ = ipars_ds$(O)

DRIVER_DS_OBJ = driver_ds$(O)

DEALII_DS_OBJ = IPACS_ds$(O)

SORC = ..$(S)dealii$(S)

ipars_dealii.f: $(SORC)ipars_dealii.df scrat1.f blkary.h control.h layout.h tarydat.h wells.h
	$(SETSIZE1)
	$(SIZEIT)

ipars_dealii$(O): scrat1$(O) adaptivity$(O)

driver.cpp: $(SORC)driver.cpp ipars_dealii.h
	$(COPYIT1)

ipars_dealii.h: $(SORC)ipars_dealii.h
	$(COPYIT1)

IPACS-V3.0.cc: $(SORC)IPACS-V3.0.cc ipars_dealii.h
	$(COPYIT1)

ipars_ds.f: $(SORC)ipars_ds.df control.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

driver_ds.cpp: $(SORC)driver_ds.cpp ipars_dealii.h
	$(COPYIT1)

IPACS_ds.cc: $(SORC)IPACS_ds.cc ipars_dealii.h
	$(COPYIT1)

