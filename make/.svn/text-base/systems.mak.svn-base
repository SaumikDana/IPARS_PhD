# systems.mak

# Unified makefile for different computer systems, compilers, libraries.
# To use, put "export SYSTEM=<system>" in your ~/.bashrc file,
# and add a case below.

# Ben Ganis
# 2/15/17

ifeq ($(SYSTEM),sl6)
  include $(MAKDIR)sl6.mak
else ifeq ($(SYSTEM),c7)
  include $(MAKDIR)c7.mak
else ifeq ($(SYSTEM),osx)
  include $(MAKDIR)osx.mak
else ifeq ($(SYSTEM),osx-openmpi-ifort)
  include $(MAKDIR)osx.mak
else ifeq ($(SYSTEM),bevo3)
  include $(MAKDIR)bevo3.mak
else ifeq ($(SYSTEM),stampede)
  include $(MAKDIR)stampede.mak
else ifeq ($(SYSTEM),stampede2)
  include $(MAKDIR)stampede2.mak
else
  FAILMSG = "Error: set SYSTEM environment variable or change makefile"
  include $(MAKDIR)crash.mak
endif
