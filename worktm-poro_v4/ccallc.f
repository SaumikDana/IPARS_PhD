c This is a collection of short programs which resolve
c the problem in mixed language environment.
c MS FORTRAN & MS Visual C++

c _BLKOFF@##

        subroutine myblkoff(nblk, ioff,joff,koff,nerr)
         integer nblk,ioff,joff,koff,nerr

C         include 'msjunk.h'

         call blkoff(nblk,ioff,joff,koff,nerr)

        return
        end


c _BLKDIM@##

         subroutine myblkdim(nblk, ioff,joff,koff,nerr)
           integer nblk,ioff,joff,koff,nerr

C         include 'msjunk.h'

          call blkdim(nblk,ioff,joff,koff,nerr)

          return
          end


c _CALLWORK@##

         subroutine mycallwork(workrtn,myarr)

          external workrtn
          integer myarr(*)

C         include 'msjunk.h'

          call callwork(workrtn,myarr)

          return
          end
