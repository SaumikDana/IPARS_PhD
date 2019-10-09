# pc.mak - IBMPC Makefile Executive for IPARS

# Copy this file to ipars/work/makefile then edit it to uncomment the
# appropriate lines

# make                 Builds production program
# make clean           Deletes work files

########################## Unix/Dos Controls #########################

# Define the slash for file names
S=^\

# Define the target file name
EXENAM=ipars.exe

# Define the object file extension
O=.obj

# Define the copy instruction
COPY=copy

############################## Misc ##################################

default:$(EXENAM)

SIZE=..$(S)size$(S)size
SIZDAT=ipars.siz
SETSIZE=echo $(SIZDAT) $@ > ech
SETSIZE1=echo $(SIZDAT) $** $@ > ech
SIZEIT=$(SIZE) < ech

MAKDIR=..$(S)make$(S)

WORK=.

COPYIT=$(COPY) $? $(WORK)

.SUFFIXES:
.SUFFIXES: .o .f .c .C .h .df .dc .dh .dC .in .obj

################# Framework Include Files ############################

# !include $(MAKDIR)frame.mak
# !include $(MAKDIR)parall.mak

###################### Linear Solver (one only) ######################

# !include $(MAKDIR)lsor.mak
# !include $(MAKDIR)mulgrd.mak
# !include $(MAKDIR)gmres.mak            Out of date
# !include $(MAKDIR)petsc.mak            Out of date

####################### Multi-block (one only) #######################

# !include $(MAKDIR)mortar.mak           Out of date
# !include $(MAKDIR)dual.mak

############################ Graphics ################################

# !include $(MAKDIR)visual.mak

################ Physical Model Include Files ########################

# !include $(MAKDIR)hydroi.mak           
# !include $(MAKDIR)hydroe.mak           Out of date
# !include $(MAKDIR)blacki.mak           
# !include $(MAKDIR)singlee.mak          Out of date
# !include $(MAKDIR)singlei.mak           
# !include $(MAKDIR)comp.mak

###################### Combine Object/Lib Files ######################

MODELOBJ = $(HYDROIOBJ) $(HYDROEOBJ) $(SINGLEIOBJ) $(SINGLEEOBJ) \
  $(BLACKIOBJ) $(COMPOBJ)

OBJS = $(FRAMEOBJ) $(MODELOBJ) $(BLOCKSOBJ) $(SOLVEOBJ) $(GRAPHOBJ) $(PARALOBJ)

MODELLIB = $(HYDROILIB) $(HYDROELIB) $(SINGLEILIB) $(SINGLEELIB) \
  $(BLACKILIB) $(COMPLIB)

LIBS = $(FRAMELIB) $(MODELLIB) $(BLOCKSLIB) $(SOLVELIB) $(GRAPHLIB) $(PARALLIB)

########### Machine and Compiler Include File (one only) #############
# This include file must be the last one

# !include $(MAKDIR)ibmpc1.mak
# !include $(MAKDIR)ibmpc2.mak