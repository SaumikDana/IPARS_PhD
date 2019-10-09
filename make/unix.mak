# unix.mak - Makefile Executive for IPARS on Unix

# Copy this file to ipars/work/makefile then edit it to uncomment the
# appropriate lines

# make                 Builds production program
# make clean           Deletes work files

########################## Unix/Dos Controls #########################

# Define the slash for file names
S=/

# Define the target file name
EXENAM=ipars

# Define the object file extension
O=.o

# Define the copy instruction
COPY=cp

############################## Misc ##################################

default:$(EXENAM)

SIZE=.$(S)size
SIZDAT=.$(S)ipars.siz
SETSIZE=echo $(SIZDAT) $@ > ech
SETSIZE1=echo $(SIZDAT) $< $@ > ech
SIZEIT=$(SIZE) < ech

CC2ANSIex = .#(S)cpluscomm2ansi

MAKDIR=..$(S)make$(S)

WORK=.

COPYIT=$(COPY) $? $(WORK)
COPYIT1=$(COPY) $< $(WORK)

.SUFFIXES:
.SUFFIXES: .o .f .c .C .cpp .h .df .dc .dh .dC .in .obj

# DEBUG_FLAGS=1

####################### Framework Include Files ######################

# include $(MAKDIR)parall.mak
# include $(MAKDIR)frame.mak

###################### Linear Solver (one only) ######################

# include $(MAKDIR)lsor.mak
# include $(MAKDIR)mulgrd.mak
# include $(MAKDIR)bcgs.mak
# include $(MAKDIR)gmres.mak
# include $(MAKDIR)ygmres.mak
# include $(MAKDIR)ygmres_samg.mak
# include $(MAKDIR)petsc.mak           out of date
# include $(MAKDIR)pcg.mak             out of date
# include $(MAKDIR)hypre.mak
# include $(MAKDIR)trilinos.mak

####################### Multi-block (one only) #######################

# include $(MAKDIR)mortar.mak          out of date 
# include $(MAKDIR)dual.mak
# include $(MAKDIR)dealii.mak

############################ Graphics ################################

# include $(MAKDIR)visual.mak
# include $(MAKDIR)vistecbin.mak
# include $(MAKDIR)visvtk.mak           

################### Physical Model Include Files #####################

# include $(MAKDIR)hydroi.mak           
# include $(MAKDIR)hydroe.mak          out of date 
# include $(MAKDIR)blacki.mak           
# include $(MAKDIR)singlee.mak         out of date
# include $(MAKDIR)singlei.mak           
# include $(MAKDIR)comp.mak             
# include $(MAKDIR)comp_mfmfe.mak  
# include $(MAKDIR)singlei_mfmfe.mak 
# include $(MAKDIR)mpfa.mak           

##################### Combine Object/Lib Files #######################

MODELOBJ = $(HYDROIOBJ) $(HYDROEOBJ) $(SINGLEIOBJ) $(SINGLEEOBJ) \
  $(BLACKIOBJ) $(COMPOBJ)

SOLVEOBJ = $(LSOROBJ) $(BCGSOBJ) $(MULGRDOBJ) $(GMRESOBJ) $(PCGBJ) \
  $(HYPREOBJ) $(TRILINOSOBJ)

OBJS = $(FRAMEOBJ) $(MODELOBJ) $(BLOCKSOBJ) $(SOLVEOBJ) $(GRAPHOBJ) $(PARALOBJ)

MODELLIB = $(HYDROILIB) $(HYDROELIB) $(SINGLEILIB) $(SINGLEELIB) \
  $(BLACKILIB) $(COMPLIB)

LIBS = $(FRAMELIB) $(BLOCKSLIB) $(MODELLIB) $(SOLVELIB) $(GRAPHLIB) $(PARALLIB)

########### Machine and Compiler Include File (one only) #############
# This include file must be the last one

include $(MAKDIR)systems.mak

