# hypre.mak - HYPRE make include file

# Object files #######################################################

SUPERLUOBJ = superlupara$(O) superlu_mod$(O) sp_ienv$(O) dcreate_dist_matrix$(O) \
            superlu_c2f_dwrap$(O) superlu_porohex$(O)

# Source files #######################################################

# real path of hypre files need to be corected.
SORC = ..$(S)solve$(S)superlu$(S)

superlu_porohex$(O): superlu_porohex.f90
superlu_mod$(O): superlu_mod.f90
superlupara$(O): superlupara.f90
sp_ienv$(O): sp_ienv.c
dcreate_dist_matrix$(O): dcreate_dist_matrix.c
superlu_c2f_dwrap$(O): superlu_c2f_dwrap.c

superlu_porohex.f90: $(SORC)superlu_porohex.f90
	$(COPYIT)

superlu_mod.f90: $(SORC)superlu_mod.f90
	$(COPYIT)

superlupara.f90: $(SORC)superlupara.f90
	$(COPYIT)

sp_ienv.c: $(SORC)sp_ienv.c
	$(COPYIT)

dcreate_dist_matrix.c: $(SORC)dcreate_dist_matrix.c
	$(COPYIT)

superlu_c2f_dwrap.c: $(SORC)superlu_c2f_dwrap.c
	$(COPYIT)

