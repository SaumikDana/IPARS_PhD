c************************************************************************
c
c Subroutine: pdc3d
c
c Purpose:
c   A fast direct method for solving the block tridiagonal
c   linear system
c 
c   Au=f,
c
c   where the matrix A is separable, that is, the system
c   can be expressed in the form:
c
c   (A1(x)M2(x)M3 + M1(x)A2(x)M3 + M1(x)M2(x)A3 + ch*M1(x)M2(x)M3)u = f.
c
c   The notation "(x)" denotes the tensor product of the matrices,
c   that is, A(x)B = {a_ij*B}.
c   A1, A2 and A3 are symmetric tridiagonal matrices of dimension
c   n1, n2 and n3, respectively. M1, M2 and M3 are diagonal matrices of the
c   same dimension. Restriction: the matrices M1 and M2 must be positive
c   definite and the matrix A must be nonsingular.
c
c   The above system can be written in a block form
c
c   C_i u_i-1 + D_i u_i + C_i+1 u_i+1 = f_i,                      (1)
c
c   where u_i and f_i are the i:th blocks of length n2*n3 of the vectors
c   u and f, respectively. Here C_i = a1(i)*M2(x)M3, i=2,...,n1, and
c   D_i = (b1(i) + ch*d1(i))*M2(x)M3 + d1(i)*(A2(x)M3+M2(x)A3), i=1,...,n1.
c
c
c Version: 1.0
c
c Date: 02 Jul 1997
c
c Parameters:
c
c   Input:
c          n1     - The dimension of the matrices A1 and M1
c          n2     - The dimension of the matrices A2 and M2
c          n3     - The dimension of the matrices A3 and M3
c          ldf3   - The first leading dimension of the three-dimensional 
c                   array f; ldf3 should be at least n3. The right-hand
c                   side vector f is assumed to be stored into an
c                   array f(ldf3,ldf2,*) (corresponding to lexicographic
c                   numbering of grid points, i.e., in the order 3-2-1).
c          ldf2   - The second leading dimension of the three-dimensional 
c                   array f; ldf2 should be at least n2
c          a1     - The codiagonal of the matrix A1;
c                   the first components is in position a1(2)
c          b1     - The diagonal of the matrix A1
c          d1     - The diagonal of the matrix M1
c          a2     - The codiagonal of the matrix A2;
c                   the first components is in position a2(2)
c          b2     - The diagonal of the matrix A2
c          d2     - The diagonal of the matrix M2
c          a3     - The codiagonal of the matrix A3;
c                   the first component is in position a3(2)
c          b3     - The diagonal of the matrix A3
c          d3     - The diagonal of the matrix M3
c          ch     - The coefficient in the equation
c          ldw    - The length of double precision workspace;
c                   a safe (but only approximate) minimum value is
c                   6*nlmax*(n1+n2) + max(9*n1,11*n2*n3) + max(9*n2,10*n3)
c                   where
c                   nlmax = max(1 + max(int(log4(n1)), 0),
c                               1 + max(int(log4(n2)), 0)).
c          liw    - The length of integer workspace;
c                   a safe (but only approximate) minimum value is
c                   2*(4**nlmax-1)/3 + 5*nlmax + 6*n1 + 5*n2 + 12
c                   where nlmax is the same as previously
c          init   - Flags which indicate whether the purpose of
c                   the call is to initialize the internal data structures
c                   (init(1) = .true. or init(2) = .true. or
c                    init(3) = .true.) or, otherwise, to solve the problem;
c                   Before solution, the subroutine has to be called at least
c                   once with init(2) = .true. and init(3) = .true.
c                   The three initialization stages are:
c                     init(1) = .true.: the communicators created earlier
c                       by init(2) = .true. are freed; can be used to
c                       alter the number of processes used in the solution;
c                     init(2) = .true.: create new internal communicators;
c                       this should be performed at least once and every
c                       time after (or within) a call with init(1) = .true.
c                     init(3) = .true.: initialize other internal workspace
c                       of the subroutine and the values of ilf and iuf;
c                       this should be performed at least once and every
c                       time after (or within) a call with init(2) = .true.;
c                       can be used to change some of the parameters n1, a1,
c                       b1, d1, n2, a2, b2 or d2.
c                       Note1: the values of the parameters n3, a3, b3, d3,
c                       ldf2, ldf3, f and ch can be changed without
c                       reinitialization keeping in mind the restrictions
c                       for their reasonable values
c                       Note2: If a working complete version of ScaLAPACK
c                       and BLACS libraries is available, the initialization
c                       can also be run in parallel. To do this, remove the
c                       comments in front of the lines 659-669 and 672.
c          inicom - The MPI communicator to be used in the parallel
c                   execution
c          heter  - Flag which indicates whether coefficients of
c                   the equation are 2d heterogeneous (in plane {2,3}).
c                   If .true., then instead of efficient solver dc2d, the plane
c                   PCG with three LU preconditioners mf2d is used,
c                   with higher arithmetical
c                   complexity O[(n2n3)**alpha], 1.2<=alpha<=1.3, and
c                   higher cost of the initialization (LU factorization)
c                   complexity O[(n2n3)**alpha], 1.6<=alpha<=1.7, and
c                   larger memory requirements
c
c   Input/output:
c          f      - On entry f contains the vector blocks of
c                   the right hand side vector stored by this process;
c                   on successful exit f contains the correponding
c                   blocks of the solution;
c                   The components are stored according to the
c                   block representation (1), that is, the first
c                   ldf2*ldf3 components of f contain the block f_ilf etc;
c                   The solution is returned in the same order;
c                   IMPORTANT: When the subroutine is called with
c                   init(3) = .true., it computes the new values of
c                   ilf and iuf. These describe the part of the right
c                   hand side vector (and also the solution vector) blocks
c                   which are stored by each process. So, each process
c                   has to be given iuf-ilf+1 blocks of size ldf2*ldf3.
c                   These blocks correspond to blocks
c                   F(*,*,i), i=ilf,...,iuf
c                   in the original right hand side vector F in AU=F,
c                   but they have to be placed in the first positions
c                   of the parameter vector f, that is:
c
c                   f(*,*,i) = F(*,*,i+ilf-1), i=1,...,iuf-ilf+1,
c
c                   The solution vector will be computed on top of f
c                   and the same formula defines the distribution of
c                   solution vector blocks.
c          ilf    - The number of the first vector block of f
c                   of size ldf2*ldf3 stored by this process;
c                   Computed in the initialization if init(3) = .true.
c                   This should not be changed by user;
c          iuf    - The number of the last vector block of f
c                   of size ldf2*ldf3 stored by this process;
c                   Computed in the initialization if init(3) = .true.
c                   This should not be changed by user
c
c   Output:
c          ierr   - Error flag indicating failure.
c                   Possible return values:
c                   0  no error
c                   1  n1 < 4**(1+int(eps + log(2.d0*size)/log(4.d0)))
c                      or n1 < 1 or n2 < 1 or n3 < 1 or ldf2 < n2 or
c                      ldf3 < n3,  where size is the number of processes
c                   2  double precision workspace too short;
c                      the required amount of workspace can
c                      be found in iw(1) if liw > 5
c                   3  integer workspace too short;
c                      the required amount of workspace can
c                      be found in iw(2) if liw > 5
c                   4  failure in the LAPACK subroutine dstebz or
c                      in the ScaLAPACK subroutine pdstebz while
c                      solving eigenvalues;
c                      possibly one of the arrays a1, b1 or d1
c                      is incorrect
c                   5  failure in the LAPACK subroutine dstein
c                      while solving eigenvectors;
c                      possibly one of the arrays a1, b1 or d1
c                      is incorrect
c                   6  failure in the LAPACK subroutine dstebz while
c                      solving eigenvalues;
c                      possibly one of the arrays a2, b2 or d2
c                      is incorrect
c                   7  failure in the LAPACK subroutine dstein
c                      while solving eigenvectors;
c                      possibly one of the arrays a2, b2 or d2
c                      is incorrect
c                   8  The number of processes associated to the
c                      communicator inicom is not of the form
c                      2**k, k >= 0.
c                   9  Error in a MPI subroutine
c
c   Workspace:
c          dw     - Double precision workspace, length at least ldw
c          iw     - Integer workspace, length at least liw
c
c
c Subroutines called:
c   dstebz and dstein from the LAPACK library. pdstebz from
c   the ScaLAPACK library. blacs_get, blacs_gridmap and
c   blacs_gridexit from the BLACS library. dcopy, dscal and
c   daxpy from the BLAS1 library. These subroutines can be
c   obtained from the NETLIB archive. dc2d, inispl, getbnd
c   from dc2d.f.
c
c
c Language: FORTRAN
c
c Portability: FORTRAN-77 with do-enddo extension
c
c
c Algorithm:
c   PSCR-method (Partial Solution Variant of Cyclic Reduction aka Divide &
c   conquer method) for linear systems with separable block tridiagonal
c   matrices.
c
c References:
c   T. Rossi and J. Toivanen:
c   A Parallel Fast Direct Solver for Block Tridiagonal Systems with
c   Separable Matrices of Arbitrary Dimension,
c   Report 21/96, Laboratory of Scientific Computing,
c   University of Jyvaskyla, 1996.
c
c
c Authors: Tuomo Rossi   (tro@math.jyu.fi),
c          Jari Toivanen (tene@math.jyu.fi)
c
c Address: University of Jyvaskyla
c          Department of Mathematics
c          Laboratory of Scientific Computing
c          P.O. Box 35
c          FIN-40351 Jyvaskyla
c          Finland
c
c************************************************************************
      subroutine pdc3d(n1,n2,n3,f,ldf2,ldf3,ilf,iuf,a1,b1,d1,a2,b2,d2,
     &                 a3,b3,d3,ch,dw,ldw,iw,liw,inicom,init,heter,ierr)
c
      include 'mpif.h'
c
      integer n1, n2, n3, ldf2, ldf3, ilf, iuf, ldw
      integer liw, iw(liw), inicom, ierr
      double precision f(ldf2*ldf3*n1), a1(n1), b1(n1), d1(n1)
      double precision a2(*), b2(*), d2(*), a3(*), b3(*), d3(*)
      double precision ch, dw(ldw)
      logical init(3),heter
c
      integer icomm, ieig, iwev, iv1, iv3, ivr, ig, ir, ix
      integer ip4, iiwev, isplit
      integer iep, ife, ilb, ile, ilen, ilenf, iloc, ilocf, ilocl, isp
      integer iub, i, j, k, level, ll, locs, nl, nl2, np, ns
      integer iiw2d, liw2d, idw2d, ldw2d, comlvl, rank, size, blocktype
      double precision c, eps
      parameter (eps=1.d-13)
      include 'staticums.h'
c
      ierr = 0
c
      if (n1.lt.1.or.n2.lt.1.or.n3.lt.1.or.ldf2.lt.n2.or.
     &    ldf3.lt.n3) then
         ierr = 1
         return
      end if
      if (liw.lt.6) then
         ierr = 3
         return
      end if
c
      icomm  = 7
c
      if (init(1)) then
         comlvl = iw(5)
         call frcoms(iw(icomm),comlvl,ierr)         
         if (ierr.ne.0) return
      end if
c
      if (init(2)) then
c
c Initialize the communicators and generate a block datatype
c
         call mkcoms(iw(icomm),size,rank,comlvl,inicom,ierr)
         if (ierr.ne.0) return
         call MPI_TYPE_VECTOR(n2,n3,ldf3,MPI_DOUBLE_PRECISION,
     &                        blocktype,ierr)
         if (ierr.ne.0) return
         call MPI_TYPE_COMMIT(blocktype,ierr)
         if (ierr.ne.0) return
         iw(3) = size
         iw(4) = rank
         iw(5) = comlvl
         iw(6) = blocktype
      else
         size      = iw(3)
         rank      = iw(4)
         comlvl    = iw(5)
         blocktype = iw(6)
      end if
c
      nl  = 1 + max(int(eps + log(dble(n1))/log(4.d0)),0)
      nl2 = 1 + max(int(eps + log(dble(n2))/log(4.d0)),0)
      if (4**(1+int(eps + log(2.d0*size)/log(4.d0))).gt.n1) then
         ierr = 1
         return
      end if
c
c Pointers to the real work space
c
      if ( heter ) then
        if      ( n2*n3 .le. N2Value(1,2) ) then
           Multiplier = N2Value(1,1)
        else if ( n2*n3 .le. N2Value(2,2) ) then
           Multiplier = N2Value(2,1)
        else if ( n2*n3 .le. N2Value(3,2) ) then
           Multiplier = N2Value(3,1)
        else if ( n2*n3 .le. N2Value(4,2) ) then
           Multiplier = N2Value(4,1)
        else
           Multiplier = N2Value(5,1)
        end if
c       ldw2d = Multiplier*n2*n3 
        ldw2d = 3*Multiplier*n2*n3 
      else
        ldw2d = 6*n2*nl2 + max(9*n2, 10*n3)
      end if

      k     = (n1 + 2*size - 1)/size
      k     = min(k, n1)
      ieig  = 1
      iwev  = ieig  + 6*k*nl
      iv1   = ieig  + 6*k*nl
      iv3   = iv1   + n2*n3
      ig    = iv3   + n2*n3
      ir    = ig    + 3*n2*n3
      ix    = ir    + 3*n2*n3
      ivr   = ix    + 2*n2*n3
      idw2d = ivr   + n2*n3
      iw(1) = max(idw2d + ldw2d - 1,iwev + 9*n1 - 1)
      if ( heter ) iw(1) = max(idw2d + ldw2d/2 - 1,iwev + 9*n1 - 1)
      if (iw(1).gt.ldw) then
         ierr = 2
         return
      end if
c
c Pointers to the integer work space
c
      if ( heter ) then
c       liw2d  = N2Index*n2*n3
        liw2d  = 3*N2Index*n2*n3
      else
        liw2d  = (4**nl2 - 1)/3 + 2*nl2 + 5*n2 + 4
      end if

      ip4    = icomm  + comlvl
      iiwev  = ip4    + nl + 1
      isplit = iiwev  + 6*n1
      iiw2d  = isplit + nl + (4**nl - 1)/3
      iw(2)  = iiw2d  + liw2d - 1
      if (iw(2).gt.liw) then
         ierr = 3
         return
      end if
c
      if (init(3)) then
         iw(ip4) = 1
         do k=1,nl
            iw(ip4+k) = 4*iw(ip4+k-1)
         end do
c
c Make the division into strips
c
         call inispl(n1,iw(isplit),nl,iw(ip4))
c
         level = nl
         call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
         call getbnd(level,ilocf,ilb,iub,iw(isplit),iw(ip4))
         ilf = max(ilb,1)
         call getbnd(level,ilocl,ilb,iub,iw(isplit),iw(ip4))
         iuf = iub - 1
c
         ilenf = iuf - ilf + 1
c
c Compute the eigenvalues and eigenvectors for the problems
c
         do level=1,nl
            call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
c
            do iloc=ilocf,ilocl
               call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
               ilen = iub - ilb - 1
               if (ilen.gt.0) then
                  isp = isplit + level + (iw(ip4+level) - 1)/3
     &                + 4*(iloc - 1)
                  if (level.eq.nl) isp = isplit
                  if (np.le.1) then
                     ife = 1
                     ile = ilen
                     iep = ieig + 6*(ilb - ilf + 1 + ilenf*(level - 1))
                  else
                     ife = max(ilf-ilb,1)
                     ile = iuf - ilb
                     iep = ieig + 6*ilenf*(level - 1)
                  end if
                  call peigval(ilen,ife,ile,a1(ilb+1),b1(ilb+1),
     &                        d1(ilb+1),dw(iep),dw(iwev),iw(iiwev),
     &                        rank,np,size,ierr)
                  if (ierr.ne.0) return
                  call peigvec(ilen,ife,ile,a1(ilb+1),b1(ilb+1),
     &                        d1(ilb+1),dw(iep),iw(isp),dw(iwev),
     &                        iw(iiwev),ierr)
                  if (ierr.ne.0) return
               end if
            end do
         end do
c
c Initialize dc2d
c
         if (heter) then
         call mf2d(n2,n3,f,ldf3,a2,b2,d2,a3,b3,d3,ch,dw(idw2d),ldw2d,
     &             iw(iiw2d),liw2d,.true.,ierr)
         else
         call dc2d(n2,n3,f,ldf3,a2,b2,d2,a3,b3,d3,ch,dw(idw2d),ldw2d,
     &             iw(iiw2d),liw2d,.true.,ierr)
         end if
         if (ierr.ne.0) then
            ierr = ierr + 2
         end if
      end if
c
      if (init(1).or.init(2).or.init(3)) return
c
      ilenf = iuf - ilf + 1
c
c First stage, bottom level
c
      level = nl
      call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
c
      do iloc=ilocf,ilocl
c
c Find the bounds for the strip
c
         call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
         ilen = iub - ilb - 1
         if (ilen.gt.0) then
            k = ilb - ilf + 1
            iep = ieig + 6*(k + ilenf*(level - 1))
            ll = k*ldf2*ldf3
c
            if (ilen.eq.1) then
c
c Problem with one grid column
c
               c = dw(iep+1)**2
               do j=1,n2
                  do k=1,n3
                     dw(ix+(j-1)*n3+k-1) = c*f(ll+(j-1)*ldf3+k)
                  end do
               end do
               c = dw(iep) + ch
               if (heter) then
               call mf2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,ierr)
               else
               call dc2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,ierr)
               end if
               call upfr3d(n1,n2,n3,ilb,iub,dw,n2,n3,iv1-1,iv3-1,
     &                     a1,d2,d3,dw,n3,ix-1,ix-1,.false.)
            else if (ilen.eq.2) then
c
c Problem with two grid planes
c
               call slcp3d(n2,n3,dw(iep),a2,b2,d2,a3,b3,d3,ch,
     &              f(ll+1),ldf2,ldf3,dw(ir),n2,n3,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,heter)
               call upfr3d(n1,n2,n3,ilb,iub,dw,n2,n3,iv1-1,iv3-1,
     &                     a1,d2,d3,dw,n3,ir-1,ir+n2*n3-1,.false.)
            else
c
c Problem with three grid planes
c
               call sltr3d(n2,n3,dw(iep),a2,b2,d2,a3,b3,d3,ch,
     &              f(ll+1),ldf2,ldf3,dw(ir),n2,n3,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,heter)
               call upfr3d(n1,n2,n3,ilb,iub,dw,n2,n3,iv1-1,iv3-1,
     &                     a1,d2,d3,dw,n3,ir-1,ir+2*n2*n3-1,.false.)
            end if
c
            call upsol1(f,ldf2,ldf3,ilf,iuf,dw(ivr),dw(iv1),dw(iv3),
     &                  n1,n2,n3,ilb,iub,.false.)
         end if
      end do
c
c First stage, levels through bottom - 1 to top + 1
c
      do level=nl-1,2,-1
         call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
c
         do iloc=ilocf,ilocl
c
c Find the bounds for the strip
c
            call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
            ilen = iub - ilb - 1
            ns = min(ilen/4,3)
            if (ilen.gt.3.and.ns.gt.0) then
c
c Problem with 'ns' grid planes
c
               isp = isplit + level + (iw(ip4+level) - 1)/3
     &             + 4*(iloc - 1)
c
               call getfor(dw(ig),n2,n3,ns,f,ldf2,ldf3,ilf,iw(isp+1),
     &                     np,rank,iw(icomm+level-1),blocktype,ierr)
               if (ierr.ne.0) return
c
               if (np.le.1) then
                  ife = ilb + 1
                  ile = iub - 1
               else
                  ife = ilf
                  ile = iuf
                  if (ife.eq.ilb) ile = ile - 1
               end if
c
               do k=iv1,iv1+2*n2*n3-1
                  dw(k) = 0.d0
               end do
c
c Go through eigenvalues one by one
c
               do j=ife-ilf,ile-ilf
                  iep = ieig + 6*(j + ilenf*(level - 1))
c
                  call ftrans3(ns,n2,n3,dw(ix),dw(ig),dw(iep+2))
c
                  c = dw(iep) + ch
                  if (heter) then
                  call mf2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &                      dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                      ierr)
                  else
                  call dc2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &                      dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                      ierr)
                  end if
c
                  if (ilb.ne.0)
     &               call daxpy(n2*n3,dw(iep+1),dw(ix),1,dw(iv1),1)
                  if (iub.ne.n1+1)
     &               call daxpy(n2*n3,dw(iep+5),dw(ix),1,dw(iv3),1)
               end do
c
               call upfr3d(n1,n2,n3,ilb,iub,dw,n2,n3,iv1-1,iv3-1,
     &                     a1,d2,d3,dw,n3,iv1-1,iv3-1,.false.)
               if (np.le.1)
     &            call upsol1(f,ldf2,ldf3,ilf,iuf,dw(ivr),dw(iv1),
     &                        dw(iv3),n1,n2,n3,ilb,iub,.true.)
            end if
         end do
c
         call upsol2(f,ldf2,ldf3,ilf,dw(iv1),dw(iv3),dw(ivr),dw(ix),
     &               n1,n2,n3,np,level,size,rank,iw(icomm),iw(isplit),
     &               iw(ip4),ierr)
         if (ierr.ne.0) return
      end do
c
c Second stage, levels through top to bottom - 1
c      
      do level=1,nl-1
         call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
c
         do iloc=ilocf,ilocl
c
c Find the bounds for the strip
c
            call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
            ilen = iub - ilb - 1
            ns = min(ilen/4,3)
            if (ilen.gt.3.and.ns.gt.0) then
c
c Problem with 'ns' grid columns
c
               isp = isplit + level + (iw(ip4+level) - 1)/3
     &             + 4*(iloc - 1)
c
               call getfor(dw(ig),n2,n3,ns,f,ldf2,ldf3,ilf,iw(isp+1),
     &                     np,rank,iw(icomm+level-1),blocktype,ierr)
               if (ierr.ne.0) return
c
               if (np.le.1) then
                  ife = ilb + 1
                  ile = iub - 1
               else
                  ife = ilf
                  ile = iuf
                  if (ife.eq.ilb) ile = ile - 1
               end if
c
c Set the nonhomogenous boundary conditions
c
               call getbv(dw(iv1),dw(iv3),f,ldf2,ldf3,ilf,iuf,dw(ir),
     &                    dw(ivr),ilb,iub,n1,n2,n3,np,rank)
               call upfr3d(n1,n2,n3,ilb,iub,dw,n2,n3,iv1-1,iv3-1,
     &                     a1,d2,d3,dw,n3,iv1-1,iv3-1,.false.)
               do k=ir,ir+ns*n2*n3-1
                  dw(k) = 0.d0
               end do
c
c Go through eigenvalues one by one
c
               do j=ife-ilf,ile-ilf
                  iep = ieig + 6*(j + ilenf*(level - 1))
c
                  call ftrans3(ns,n2,n3,dw(ix),dw(ig),dw(iep+2))
c
                  if (ilb.ne.0)
     &               call daxpy(n2*n3,dw(iep+1),dw(iv1),1,dw(ix),1)
                  if (iub.ne.n1+1)
     &               call daxpy(n2*n3,dw(iep+5),dw(iv3),1,dw(ix),1)
c
                  c = dw(iep) + ch
                  if (heter) then
                  call mf2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &                      dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                      ierr)
                  else
                  call dc2d(n2,n3,dw(ix),n3,a2,b2,d2,a3,b3,d3,c,
     &                      dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                      ierr)
                  end if
c     
                  do k=0,ns-1
                     call daxpy(n2*n3,dw(iep+k+2),dw(ix),1,
     &                          dw(ir+k*n2*n3),1)
                  end do
               end do
c
               call upsol3(f,ldf2,ldf3,ilf,dw(iv1),dw(ivr),dw(ir),
     &                     dw(ig),ns,n2,n3,np,rank,iw(icomm+level-1),
     &                     iw(isp+1),ierr)
               if (ierr.ne.0) return
            end if
         end do
      end do
c
c Second stage, bottom level
c
      level = nl
      call getloc(locs,ilocf,ilocl,np,level,size,rank,iw(ip4))
c
      do iloc=ilocf,ilocl
c
c Find the bounds for the strip
c
         call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
         ilen = iub - ilb - 1
c
         if (ilen.gt.0) then
            call getbv(dw(iv1),dw(iv3),f,ldf2,ldf3,ilf,iuf,dw(ir),
     &                 dw(ivr),ilb,iub,n1,n2,n3,np,rank)
c
            k = ilb - ilf + 1
            iep = ieig + 6*(k + ilenf*(level - 1))
            ll = k*ldf2*ldf3
c
            if (ilen.eq.1) then
c
c Problem with one grid column
c
               call upfr3d(n1,n2,n3,ilb,iub,f,ldf2,ldf3,ll,ll,
     &                     a1,d2,d3,dw,n3,iv1-1,iv3-1,.true.)
               c = dw(iep+1)**2
               do i=1,n2
                  do j=1,n3
                     f(ll+(i-1)*ldf3+j) = f(ll+(i-1)*ldf3+j)*c
                  end do
               end do
               c = dw(iep) + ch
               if (heter) then
               call mf2d(n2,n3,f(ll+1),ldf3,a2,b2,d2,a3,b3,d3,c,
     &                   dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                   ierr)
               else
               call dc2d(n2,n3,f(ll+1),ldf3,a2,b2,d2,a3,b3,d3,c,
     &                   dw(idw2d),ldw2d,iw(iiw2d),liw2d,.false.,
     &                   ierr)
               end if
            else if (ilen.eq.2) then
c
c Problem with two grid columns
c
               call upfr3d(n1,n2,n3,ilb,iub,f,ldf2,ldf3,ll,ll+ldf2*ldf3,
     &                     a1,d2,d3,dw,n3,iv1-1,iv3-1,.true.)
               call slcp3d(n2,n3,dw(iep),a2,b2,d2,a3,b3,d3,ch,
     &              f(ll+1),ldf2,ldf3,f(ll+1),ldf2,ldf3,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,heter)
            else
c
c Problem with three grid columns
c
               call upfr3d(n1,n2,n3,ilb,iub,f,ldf2,ldf3,ll,
     &                     ll+2*ldf2*ldf3,a1,d2,d3,dw,n3,
     &                     iv1-1,iv3-1,.true.)
               call sltr3d(n2,n3,dw(iep),a2,b2,d2,a3,b3,d3,ch,
     &              f(ll+1),ldf2,ldf3,f(ll+1),ldf2,ldf3,
     &              dw(idw2d),ldw2d,iw(iiw2d),liw2d,.true.,heter)
            end if
         end if
      end do
c
      return
      end
c
c************************************************************************
c
c Compute the eigenvalues for the generalized eigensystem of length n
c
      subroutine peigval(n,jf,jl,a,b,d,eigen,dw,iw,rank,np,size,ierr)
      integer n, jf, jl, iw(6*n), rank, np, size, ierr
      double precision a(n), b(n), d(n), eigen(6,jl-jf+1), dw(8*n)
c
      integer i, id, ic, ie, iu, ib, is, iv, kt, m
      double precision c
c
c Pointers to the workspace
c
      id = 1
      ic = id + n
      ie = ic + n
      iu = ie + n
c
      ib = 1
      is = ib + n
      iv = is + n
c
c Eliminate the mass matrix
c
      dw(id) = b(1)/d(1)
      do i=2,n
         dw(id+i-1) = b(i)/d(i)
         dw(ic+i-2) = a(i)/sqrt(d(i-1)*d(i))
      end do
c
c      if (np.gt.1) then
c         call blacs_get(-1,0,kt)
c         m = np*(rank/np)
c         do i=0,np-1
c            iw(iv+i) = m + i
c         end do
c         call blacs_gridmap(kt,iw(iv),1,1,np)
c         call pdstebz(kt,'A','E',n,c,c,i,i,0.d0,dw(id),dw(ic),m,i,
c     &                dw(ie),iw(ib),iw(is),dw(iu),5*n,iw(iv),4*n,ierr)
c         call blacs_gridexit(kt)
c      else
         call dstebz('A','E',n,c,c,i,i,0.d0,dw(id),dw(ic),m,i,dw(ie),
     &               iw(ib),iw(is),dw(iu),iw(iv),ierr)
c      end if
      if (ierr.ne.0) then
         ierr = 4
         return
      end if
c
      do i=1,jl-jf+1
         eigen(1,i) = dw(ie+i+jf-2)
      end do
c
      return
      end
c
c************************************************************************
c
c Compute the required components of eigenvectors
c for the generalized eigensystem of length n
c
      subroutine peigvec(n,jf,jl,a,b,d,eigen,isp,dw,iw,ierr)
      integer n, jf, jl, isp(*), iw(3*n), ierr
      double precision a(n), b(n), d(n), eigen(6,jl-jf+1), dw(9*n)
c
      double precision s, dnrm2
      integer i, j, k, ipos, id, ic, ie, iz, iu, ib, is, iv
c
c Pointers to the workspace
c
      id  = 1
      ic = id + n
      ie = ic + n
      iz = ie + n
      iu = iz + n
c
      ib = 1
      is = ib + n
      iv = is + n
c
c Eliminate the mass matrix
c
      dw(id) = b(1)/d(1)
      do i=2,n
         dw(id+i-1) = b(i)/d(i)
         dw(ic+i-2) = a(i)/sqrt(d(i-1)*d(i))
      end do
c
      iw(ib) = 1
      iw(is) = n
c
      do j=1,jl-jf+1
         dw(ie) = eigen(1,j)
         call dstein(n,dw(id),dw(ic),1,dw(ie),iw(ib),iw(is),dw(iz),n,
     &               dw(iu),iw(iv),i,ierr)
         if (ierr.ne.0) then
            ierr = 5
            return
         end if
c
c Normalize the eigenvector
c
         s = 1.d0/dnrm2(n,dw(iz),1)
         do i=1,n
            dw(iz+i-1) = s*dw(iz+i-1)/sqrt(d(i))
         end do
c
c Copy the required components
c
         if (n.le.3) then
            do k=1,n
               eigen(k+1,j) = dw(iz+k-1)
            end do
         else
            eigen(2,j) = dw(iz)
            eigen(6,j) = dw(iz+n-1)
            do k=1,3
               ipos = isp(k+1) - isp(1)
               if (ipos.lt.n) then
                  eigen(k+2,j) = dw(iz+ipos-1)
               else
                  eigen(k+2,j) = 0.d0
               end if
            end do
         end if
      end do
c
      return
      end
c
c************************************************************************
c
c Do Fourier transform for 1, 2 or 3 columns
c
      subroutine ftrans3(ns,n2,n3,x,g,evec)
      integer ns, n2, n3
      double precision x(n3,n2), g(n3,n2,ns), evec(ns)
c
      integer j, k
      double precision e1, e2, e3
c
      if (ns.eq.3) then
         e1 = evec(1)
         e2 = evec(2)
         e3 = evec(3)
         do j=1,n2
            do k=1,n3
               x(k,j) = e1*g(k,j,1) + e2*g(k,j,2) + e3*g(k,j,3)
            end do
         end do
      else if (ns.eq.2) then
         e1 = evec(1)
         e2 = evec(2)
         do j=1,n2
            do k=1,n3
               x(k,j) = e1*g(k,j,1) + e2*g(k,j,2)
            end do
         end do
      else
         e1 = evec(1)
         do j=1,n2
            do k=1,n3
               x(k,j) = e1*g(k,j,1)
            end do
         end do
      end if
c
      return
      end
c
c************************************************************************
c
c Create internal communicators
c
      subroutine mkcoms(comm,size,rank,levels,inicom,ierr)
      integer comm(*), size, rank, levels, inicom, ierr
c
      include 'mpif.h'
c
      integer level, n, color, mpierr
c
      ierr = 9
c
      call MPI_COMM_SIZE(inicom,size,mpierr)
      if (mpierr.ne.MPI_SUCCESS) return
c
      if (size.eq.0) then
         ierr = 8
         return
      end if
      levels = int(log(dble(size))/log(2.d0) + 0.5d0)
      if (2**levels.ne.size) then
         ierr = 8
         return
      end if
c
      call MPI_COMM_RANK(inicom,rank,mpierr)
      if (mpierr.ne.MPI_SUCCESS) return
c
      levels = (levels + 1)/2
      comm(1) = inicom
      n = 1
c
      do level=2,levels
         n = 4*n
         color = n*rank/size
         call MPI_COMM_SPLIT(comm(level-1),color,rank,comm(level),
     &                       mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
      end do
c
      ierr = 0
c
      return
      end
c
c************************************************************************
c
c Free internal communicators
c
      subroutine frcoms(comm,levels,ierr)
      integer levels, comm(levels), ierr
c
      include 'mpif.h'
c
      integer level, mpierr
c
      ierr = 9
c
      do level=2,levels
         call MPI_COMM_FREE(comm(level),mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
      end do
c
      ierr = 0
c
      return
      end
c
c************************************************************************
c
c Gather the force vector for a partial solution problem
c
      subroutine getfor(g,n2,n3,ns,f,ldf2,ldf3,ilf,split,np,rank,
     &                  comm,blocktype,ierr)
      integer n2, n3, ns, ldf2, ldf3, ilf, split(ns)
      integer np, rank, comm, ierr, blocktype
      double precision g(n2*n3,3), f(*), dummy(n2*n3)
c
      include 'mpif.h'
c
      integer i, j, k, ll
      integer status(MPI_STATUS_SIZE), mpierr
c
      ierr = 9
c
      if (np.le.1) then
         do i=1,ns
            ll = (split(i) - ilf)*ldf2*ldf3
            do j=1,n2
               do k=1,n3
                  g((j-1)*n3+k,i) = f(ll+(j-1)*ldf3+k)
               end do
            end do
c            call dcopy(n2*n3,f(ll),1,g(1,k),1)
         end do
      else if (np.eq.2) then
         k = mod(rank,2)
         if (k.eq.0) then
c            ll = (split(1) - ilf)*ldf2*ldf3
            ll = (split(1) - ilf)*ldf2*ldf3 + 1
c            do i=1,n2
c               do j=1,n3
c                  dummy((i-1)*n3+j) = f(ll+(i-1)*ldf3+j)
c               end do
c            end do
            call MPI_SEND(f(ll),1,blocktype,1,0,comm,mpierr)
c            call MPI_SEND(dummy,n2*n3,MPI_DOUBLE_PRECISION,
c     &                    1,0,comm,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
         else 
            call MPI_RECV(g(1,1),n2*n3,MPI_DOUBLE_PRECISION,0,0,comm,
     &                    status,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
            ll = (split(2) - ilf)*ldf2*ldf3
            do i=1,n2
               do j=1,n3
                  g((i-1)*n3+j,2) = f(ll+(i-1)*ldf3+j)
               end do
            end do
c            call dcopy(n2,f(ll),1,g(1,2),1)
            ll = (split(3) - ilf)*ldf2*ldf3
            do i=1,n2
               do j=1,n3
                  g((i-1)*n3+j,3) = f(ll+(i-1)*ldf3+j)
               end do
            end do
c            call dcopy(n2,f(ll),1,g(1,3),1)
         end if
         call MPI_BCAST(g,3*n2*n3,MPI_DOUBLE_PRECISION,1,comm,mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
      else
         k = 4*mod(rank,np)
         if (k.eq.np.or.k.eq.3*np) then
            ll = (split(k/np) - ilf)*ldf2*ldf3 + 1
c            ll = (split(k/np) - ilf)*ldf2*ldf3
c            do i=1,n2
c               do j=1,n3
c                  dummy((i-1)*n3+j) = f(ll+(i-1)*ldf3+j)
c               end do
c            end do
c            call MPI_SEND(dummy,n2*n3,MPI_DOUBLE_PRECISION,
c     &                    np/2,0,comm,mpierr)
            call MPI_SEND(f(ll),1,blocktype,np/2,0,comm,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
         else if (k.eq.2*np) then
            call MPI_RECV(g(1,1),n2*n3,MPI_DOUBLE_PRECISION,np/4,0,comm,
     &                    status,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
            ll = (split(2) - ilf)*ldf2*ldf3
            do i=1,n2
               do j=1,n3
                  g((i-1)*n3+j,2) = f(ll+(i-1)*ldf3+j)
               end do
            end do
c            call dcopy(n2,f(ll),1,g(1,2),1)
            call MPI_RECV(g(1,3),n2*n3,MPI_DOUBLE_PRECISION,3*np/4,0,
     &                    comm,status,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
         end if
         call MPI_BCAST(g,3*n2*n3,MPI_DOUBLE_PRECISION,np/2,comm,mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
      end if
c
      ierr = 0
c
      return
      end
c
c************************************************************************
c
c Update the solution
c
      subroutine upsol1(f,ldf2,ldf3,ilf,iuf,vr,v1,v3,n1,n2,n3,
     &                  ilb,iub,add)
      integer ldf2, ldf3, ilf, iuf, n1, n2, n3, ilb, iub
      double precision f(*), vr(n2*n3), v1(n2*n3), v3(n2*n3)
      logical add
c
      integer ll, i, j
c
      if (ilb.ne.0) then
         ll = (ilb - ilf)*ldf2*ldf3
         do i=1,n2
            do j=1,n3
               f(ll+(i-1)*ldf3+j) = f(ll+(i-1)*ldf3+j) + v1((i-1)*n3+j)
            end do
         end do
c         call daxpy(n2*n3,1.d0,v1,1,f(ll),1)
      end if
      if (iub.ne.iuf+1) then
         ll = (iub - ilf)*ldf2*ldf3
         do i=1,n2
            do j=1,n3
               f(ll+(i-1)*ldf3+j) = f(ll+(i-1)*ldf3+j) + v3((i-1)*n3+j)
            end do
         end do
c         call daxpy(n2*n3,1.d0,v3,1,f(ll),1)
      else if (iub.ne.n1+1) then
         if (add) then
            call daxpy(n2*n3,1.d0,v3,1,vr,1)
         else
            call dcopy(n2*n3,v3,1,vr,1)
         end if
      end if
c
      return
      end
c
c************************************************************************
c
c Update the solution
c
      subroutine upsol2(f,ldf2,ldf3,ilf,v1,v3,vr,x,n1,n2,n3,np,level,
     &                  size,rank,comm,split,p4,ierr)
      integer ldf2, ldf3, ilf, n1, n2, n3, np, level, size
      integer rank, comm(level), split(*), p4(level), ierr
      double precision f(*), v1(n2*n3), v3(n2*n3), vr(n2*n3), x(n2*n3)
c
      include 'mpif.h'
c
      integer ilb, ilocf, iub, j, k, ll, m
      integer dst, src, status(MPI_STATUS_SIZE), mpierr
c
      ierr = 9
c
      m = p4(level)
c
      k = 2*size/m
      if (k.eq.1.or.k.eq.2) then
         if (rank.ne.0) then
            do k=1,n2*n3
               v1(k) = 0.d0
            end do
         end if
         if (rank.ne.size-1)
     &      call dcopy(n2*n3,vr,1,v3,1)
      else if (np.gt.1) then
         call dcopy(2*n2*n3,v1,1,x,1)
         call MPI_REDUCE(x,v1,2*n2*n3,MPI_DOUBLE_PRECISION,
     &                   MPI_SUM,0,comm(level),mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
      end if
c
      k = 2*size/m
      if (k.ge.1) then
         if (mod(2*rank,k).eq.0) then
            k = max(np,1)
            dst = rank + k
            if (dst.ge.size) dst = MPI_PROC_NULL
            src = rank - k
            if (src.lt.0) src = MPI_PROC_NULL
            call MPI_SENDRECV(v3,n2*n3,MPI_DOUBLE_PRECISION,dst,0,
     &                        x,n2*n3,MPI_DOUBLE_PRECISION,src,0,
     &                        comm(1),status,mpierr)
            if (mpierr.ne.MPI_SUCCESS) return
            if (src.ne.MPI_PROC_NULL) then
               ilocf = m*rank/size + 1
               call getbnd(level,ilocf,ilb,iub,split,p4)
               ll = (ilb - ilf)*ldf2*ldf3
               do j=1,n2
                  do k=1,n3
                     f(ll+(j-1)*ldf3+k) = f(ll+(j-1)*ldf3+k) +
     &                    x((j-1)*n3+k) + v1((j-1)*n3+k)
                  end do
               end do
            end if
         end if
      end if
c
      ierr = 0
c
      return
      end
c
c************************************************************************
c
c Update the solution
c
      subroutine upsol3(f,ldf2,ldf3,ilf,v1,vr,r,g,ns,n2,n3,np,
     &                  rank,comm,split,ierr)
      integer ldf2, ldf3, ilf, ns, n2, n3, np, rank, comm
      integer split(3), ierr
      double precision f(*), v1(n2*n3), vr(n2*n3)
      double precision r(n2*n3,3), g(n2*n3,3)
c
      include 'mpif.h'
c
      integer i, j, k, ll
      integer mpierr
c
      ierr = 9
c
      if (np.le.1) then
         do k=1,ns
            ll = (split(k) - ilf)*ldf2*ldf3
            do i=1,n2
               do j=1,n3
                  f(ll+(i-1)*ldf3+j) = r((i-1)*n3+j,k)
               end do
            end do
c            call dcopy(n2,r(1,k),1,f(ll),1)
         end do
      else
         call dcopy(3*n2*n3,r,1,g,1)
         call MPI_ALLREDUCE(g,r,3*n2*n3,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                      comm,mpierr)
         if (mpierr.ne.MPI_SUCCESS) return
c
         if (np.eq.2) then
            if (mod(rank,2).eq.0) then
               ll = (split(1) - ilf)*ldf2*ldf3
               do i=1,n2
                  do j=1,n3
                     f(ll+(i-1)*ldf3+j) = r((i-1)*n3+j,1)
                  end do
               end do
c               call dcopy(n2,r(1,1),1,f(ll),1)
               call dcopy(n2*n3,r(1,2),1,vr,1)
            else
               ll = (split(2) - ilf)*ldf2*ldf3
               do i=1,n2
                  do j=1,n3
                     f(ll+(i-1)*ldf3+j) = r((i-1)*n3+j,2)
                  end do
               end do
c               call dcopy(n2,r(1,2),1,f(ll),1)
               ll = (split(3) - ilf)*ldf2*ldf3
               do i=1,n2
                  do j=1,n3
                     f(ll+(i-1)*ldf3+j) = r((i-1)*n3+j,3)
                  end do
               end do
c               call dcopy(n2,r(1,3),1,f(ll),1)
            end if
         else
            k = 4*mod(rank,np)
            if (k.eq.np.or.k.eq.2*np.or.k.eq.3*np) then
               ll = (split(k/np) - ilf)*ldf2*ldf3
               do i=1,n2
                  do j=1,n3
                     f(ll+(i-1)*ldf3+j) = r((i-1)*n3+j,k/np)
                  end do
               end do
c               call dcopy(n2,r(1,k/np),1,f(ll),1)
            end if
            k = k/np
            if (k.ne.0)
     &         call dcopy(n2*n3,r(1,k),1,v1,1)
            if (k.ne.3)
     &         call dcopy(n2*n3,r(1,k+1),1,vr,1)
         end if
      end if
c
      ierr = 0
c
      return
      end
c
c************************************************************************
c
c Get the boundary values for a partial solution problem
c
      subroutine getbv(v1,v3,f,ldf2,ldf3,ilf,iuf,r,vr,ilb,iub,
     &                 n1,n2,n3,np,rank)
      integer ldf2, ldf3, ilf, iuf, ilb, iub, n1, n2, n3, np, rank
      double precision v1(n2*n3), v3(n2*n3), f(*), r(n2*n3,3), vr(n2*n3)
c
      integer k, ll, j
c
      if (np.le.1) then
         if (ilb.ne.0) then
            ll = (ilb - ilf)*ldf2*ldf3 
            do j=1,n2
               do k=1,n3
                  v1((j-1)*n3+k) = f(ll+(j-1)*ldf3+k)
               end do
            end do
c            call dcopy(n2,f(ll),1,v1,1)
         end if
         if (iub.ne.iuf+1) then
            ll = (iub - ilf)*ldf2*ldf3
            do j=1,n2
               do k=1,n3
                  v3((j-1)*n3+k) = f(ll+(j-1)*ldf3+k)
               end do
            end do
c            call dcopy(n2,f(ll),1,v3,1)
         else if (iub.ne.n1+1) then
            call dcopy(n2*n3,vr,1,v3,1)
         end if
      else
         k = mod(rank,4*np)/np
         if (k.ne.0)
     &      call dcopy(n2*n3,r(1,k),1,v1,1)
         if (iub.ne.n1+1) then
            if (k.ne.3) then
               call dcopy(n2*n3,r(1,k+1),1,v3,1)
            else
               call dcopy(n2*n3,vr,1,v3,1)
            end if
         end if
      end if
c
      return
      end
