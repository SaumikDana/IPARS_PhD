#!/bin/bash

# Ben Ganis
# 1/31/11

# Run this command in the IPARSv2 root directory to generate
# a 'tags' file.  In VIM, this file enables the use of ctrl-] 
# and ctrl-t to step in and out of subroutines and definitions.
# Also, one can use :tag foo to jump to a specific subroutine. 

# Note: add -e for emacs

#CMD="ctags -R --exclude=work* --exclude=wrk* --exclude=hydroe --langmap=c:.dc.,c++:.dcpp,fortran:.df.dh.h"
#CMD="ctags --exclude=wrk* --exclude=comp  --exclude=work* --langmap=c:+.dc.,c++:+.dcpp,fortran:+.df.dh.h *"
CMD="ctags -R --exclude=wrk*  --exclude=work* --langmap=c:+.dc.,c++:+.dcpp,fortran:+.df.dh.h *"
echo $CMD
$CMD

