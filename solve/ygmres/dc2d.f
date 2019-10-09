c************************************************************************
c
c Subroutine: dc2d
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
c   (A1(x)M2 + M1(x)A2 + ch*M1(x)M2)u = f.
c
c   The notation "(x)" denotes the tensor product of the matrices,
c   that is, A(x)B = {a_ij*B}.
c   A1 and A2 are symmetric tridiagonal matrices of dimension
c   n1 and n2, respectively. M1 and M2 are diagonal matrices of the
c   same dimension. Restriction: the matrix M1 must be positive
c   definite and the matrix A must be nonsingular.
c
c   The above system can be written in a block form
c
c   C_i u_i-1 + D_i u_i + C_i+1 u_i+1 = f_i,                      (1)
c
c   where u_i and f_i are the i:th blocks of length n2 of the vectors
c   u and f, respectively. Here C_i = a1(i)*M2, i=2,...,n1, and
c   D_i = (b1(i) + ch*d1(i))*M2 + d1(i)*A2, i=1,...,n1.
c
c
c Version: 0.2
c
c Date: 29 Jan 1997
c
c Parameters:
c
c   Input:
c          n1     - The dimension of the matrices A1 and M1
c          n2     - The dimension of the matrices A2 and M2
c          ldf    - The leading dimension of the two-dimensional 
c                   array f; ldf should be at least n2
c          a1     - The codiagonal of the matrix A1;
c                   the first components is in position a1(2)
c          b1     - The diagonal of the matrix A1
c          d1     - The diagonal of the matrix M1
c          a2     - The codiagonal of the matrix A2;
c                   the first components is in position a2(2)
c          b2     - The diagonal of the matrix A2
c          d2     - The diagonal of the matrix M2
c          ch     - The coefficient in the equation
c          ldw    - The length of double precision workspace;
c                   the minimum value is 6*nl*n1 + max(9*n1, 10*n2),
c                   where nl = 1 + max(int(log4(n1)), 0)
c          liw    - The length of integer workspace;
c                   the minimum value is
c                   (4**nl - 1)/3 + 2*nl + 5*n1 + 4,
c                   where nl is the same as previously
c          init   - Flag which indicates whether the purpose of
c                   the call is too initialize the data structures
c                   in the workspace (init = .true.) or
c                   to solve the problem (init = .false.);
c                   first the subroutine should be initialized
c                   with init = .true.;
c                   after that the subroutine can be called
c                   several times with init = .false.
c                   provided that the values of n1, n2, ldf,
c                   a1, b1, d1, a2, b2, d2, dw and iw are unchanged
c
c   Input/output:
c          f      - On entry f contains the right hand side vector;
c                   on successful exit f contains the solution;
c                   The components are stored according to the
c                   block representation (1), that is, the first
c                   n2 components of f contain the block f_1 and so on.
c                   The solution is returned in the same order.
c
c   Output:
c          ierr   - Error flag indicating failure.
c                   Possible return values:
c                   0  no error
c                   1  n1 < 1 or n2 < 1 or ldf < n2
c                   2  double precision workspace too short;
c                      the required amount of workspace can
c                      be found in iw(1) if liw > 1
c                   3  integer workspace too short;
c                      the required amount of workspace can
c                      be found in iw(2) if liw > 1
c                   4  failure in the LAPACK subroutine dstebz
c                      while solving eigenvalues;
c                      possibly some of the arrays a1, b1 or d1
c                      is incorrect
c                   5  failure in the LAPACK subroutine dstein
c                      while solving eigenvectors;
c                      possibly some of the arrays a1, b1 or d1
c                      is incorrect
c
c   Workspace:
c          dw     - Double precision workspace, length at least ldw
c          iw     - Integer workspace, length at least liw
c
c
c Subroutines called:
c   dstebz and dstein from the LAPACK library.
c   dcopy, dscal, daxpy from the BLAS1 library. These subroutines
c   can be obtained from the NETLIB archive.
c
c
c Language: FORTRAN
c
c Portability: FORTRAN-77 with do-enddo extension
c
c
c Algorithm:
c   Divide & conquer algorithm for linear systems with
c   separable block tridiagonal matrices.
c
c Complexity estimate: about 52*n1*n2*log4 ((n1 + 1)/2) flops.
c
c
c References:
c   A.A. Abakumov, A.Yu. Yeremin, Yu.A. Kuznetsov:
c   Efficient fast direct method of solving Poisson's equation on
c   a parallelepiped and its implementation in array processor.
c   Sov. J. Numer. Anal. Math. Modelling 3(1988), 1-20.
c
c   T. Rossi:
c   Fictitious domain methods with separable preconditioners.
c   Ph.D. thesis, Report 69, University of Jyvaskyla,
c   Department of Mathematics, Jyvaskyla, 1995.
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
      subroutine dc2d(n1,n2,f,ldf,a1,b1,d1,a2,b2,d2,ch,
     &                dw,ldw,iw,liw,init,ierr)
      integer n1, n2, ldf, ldw, liw, iw(liw), ierr
      double precision f(ldf*n1), a1(n1), b1(n1), d1(n1)
      double precision a2(n2), b2(n2), d2(n2), ch, dw(ldw)
      logical init
c
      integer ieig, iwev, iv1, iv3, ig, ir, ix, itri
      integer ip4, iiwev, isplit
      integer iep, ilb, ilen, iloc, isp, iub, j
      integer k, level, ll, m, nl, ns
      double precision c, eps
      parameter(eps=1.d-13)
c
      ierr = 0
c
      if (n1.lt.1.or.n2.lt.1.or.ldf.lt.n2) then
         ierr = 1
         return
      end if
      if (liw.lt.2) then
         ierr = 3
         return
      end if
c
      nl = 1 + max(int(eps + log(dble(n1))/log(4.d0)),0)
c
c Pointers to the real work space
c
      ieig  = 1
      iwev  = ieig + 6*nl*n1
      iv1   = ieig + 6*nl*n1
      iv3   = iv1  + n2
      ig    = iv3  + n2
      ir    = ig   + 3*n2
      ix    = ir   + 3*n2
      itri  = ix   + n2
      iw(1) = max(iwev+9*n1,itri+n2) - 1
c
      if (iw(1).gt.ldw) then
         ierr = 2
         return
      end if
c
c Pointers to the integer work space
c
      ip4    = 3
      iiwev  = ip4    + nl + 1
      isplit = iiwev  + 5*n1
      iw(2)  = isplit + nl + (4**nl - 1)/3 - 1
c
      if (iw(2).gt.liw) then
         ierr = 3
         return
      end if
c
      if (init) then
         iw(ip4) = 1
         do k=1,nl
            iw(ip4+k) = 4*iw(ip4+k-1)
         end do
c
c Make the division into strips
c
         call inispl(n1,iw(isplit),nl,iw(ip4))
c
c Compute the eigenvalues and eigenvectors for the partial solution problems
c
         do level=1,nl
            m = iw(ip4+level-1)
            do iloc=1,m
               call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
               ilen = iub - ilb - 1
               if (ilen.ge.1) then
                  isp = isplit + level + (iw(ip4+level) - 1)/3
     &                + 4*(iloc - 1)
                  if (level.eq.nl) isp = isplit
                  iep = ieig + 6*(ilb + n1*(level - 1))
                  call eigval(ilen,a1(ilb+1),b1(ilb+1),d1(ilb+1),
     &                        dw(iep),dw(iwev),iw(iiwev),ierr)
                  if (ierr.ne.0) return
                  call eigvec(ilen,a1(ilb+1),b1(ilb+1),d1(ilb+1),
     &                        dw(iep),iw(isp),dw(iwev),iw(iiwev),ierr)
                  if (ierr.ne.0) return
               end if
            end do
         end do
c
         return
      end if
c
c First recursion, bottom level
c
      level = nl
      m = iw(ip4+level-1)
c
      do iloc=1,m
c
c Find the bounds for the strip
c
         call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
         ilen = iub - ilb - 1
c
         if (ilen.gt.0) then
            iep = ieig + 6*(ilb + n1*(level - 1))
            ll = ilb*ldf + 1
c
            if (ilen.eq.1) then
c
c Problem with one grid column
c
               c = dw(iep+1)**2
               do k=0,n2-1
                  dw(ix+k) = c*f(ll+k)
               end do
               c = dw(iep) + ch
               call soltri(n2,a2,b2,c,d2,dw(ix),dw(itri))
               call upforc(n1,n2,ilb,iub,dw(iv1),dw(iv3),
     &                     a1,d2,dw(ix),dw(ix),.false.)
            else if (ilen.eq.2) then
c
c Problem with two grid columns
c
               call soldbl(n2,dw(iep),a2,b2,d2,ch,
     &                     f(ll),ldf,dw(ir),n2,dw(itri))
               call upforc(n1,n2,ilb,iub,dw(iv1),dw(iv3),
     &                     a1,d2,dw(ir),dw(ir+n2),.false.)
            else
c
c Problem with three grid columns
c
               call soltrb(n2,dw(iep),a2,b2,d2,ch,
     &                     f(ll),ldf,dw(ir),n2,dw(itri),.false.)
               call upforc(n1,n2,ilb,iub,dw(iv1),dw(iv3),
     &                     a1,d2,dw(ir),dw(ir+2*n2),.false.)
            end if
c
            if (ilb.ne.0) then
               ll = (ilb - 1)*ldf + 1
               call daxpy(n2,1.d0,dw(iv1),1,f(ll),1)
            end if
            if (iub.ne.n1+1) then
               ll = (iub - 1)*ldf + 1
               call daxpy(n2,1.d0,dw(iv3),1,f(ll),1)
            end if
         end if
      end do
c
c First recursion, levels through bottom - 1 to top + 1
c
      do level=nl-1,2,-1
         m = iw(ip4+level-1)
c
         do iloc=1,m
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
     &             + 4*(iloc - 1) + 1
               do k=0,ns-1
                  ll = (iw(isp+k) - 1)*ldf + 1
                  call dcopy(n2,f(ll),1,dw(ig+k*n2),1)
               end do
               do k=iv1,iv1+2*n2-1
                  dw(k) = 0.d0
               end do
c
c Go through eigenvalues one by one
c
               do j=ilb+1,iub-1
                  iep = ieig + 6*(j - 1 + n1*(level - 1))
c
                  call ftrans(ns,n2,dw(ix),dw(ig),dw(iep+2))
c
                  c = dw(iep) + ch
                  call soltri(n2,a2,b2,c,d2,dw(ix),dw(itri))
c
                  if (ilb.ne.0)
     &               call daxpy(n2,dw(iep+1),dw(ix),1,dw(iv1),1)
                  if (iub.ne.n1+1)
     &               call daxpy(n2,dw(iep+5),dw(ix),1,dw(iv3),1)
               end do
c
               call upforc(n1,n2,ilb,iub,dw(iv1),dw(iv3),
     &                     a1,d2,dw(iv1),dw(iv3),.false.)
c
               if (ilb.ne.0) then
                  ll = (ilb - 1)*ldf + 1
                  call daxpy(n2,1.d0,dw(iv1),1,f(ll),1)
               end if
               if (iub.ne.n1+1) then
                  ll = (iub - 1)*ldf + 1
                  call daxpy(n2,1.d0,dw(iv3),1,f(ll),1)
               end if
            end if
         end do
      end do
c
c Second recursion, levels through top to bottom - 1
c      
      do level=1,nl-1
         m = iw(ip4+level-1)
         do iloc=1,m
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
     &             + 4*(iloc - 1) + 1
               do k=0,ns-1
                  ll = (iw(isp+k) - 1)*ldf + 1
                  call dcopy(n2,f(ll),1,dw(ig+k*n2),1)
               end do
               if (ilb.ne.0) then
                  ll = (ilb - 1)*ldf + 1
                  call dcopy(n2,f(ll),1,dw(iv1),1)
               end if
               if (iub.ne.n1+1) then
                  ll = (iub - 1)*ldf + 1
                  call dcopy(n2,f(ll),1,dw(iv3),1)
               end if
c
c Set the nonhomogenous boundary conditions
c
               call upforc(n1,n2,ilb,iub,dw(iv1),dw(iv3),
     &                     a1,d2,dw(iv1),dw(iv3),.false.)
c
               do k=ir,ir+ns*n2-1
                  dw(k) = 0.d0
               end do
c
c Go through eigenvalues one by one
c
               do j=ilb+1,iub-1
                  iep = ieig + 6*(j - 1 + n1*(level - 1))
c
                  call ftrans(ns,n2,dw(ix),dw(ig),dw(iep+2))
c
                  if (ilb.ne.0)
     &               call daxpy(n2,dw(iep+1),dw(iv1),1,dw(ix),1)
                  if (iub.ne.n1+1)
     &               call daxpy(n2,dw(iep+5),dw(iv3),1,dw(ix),1)
c
                  c = dw(iep) + ch
                  call soltri(n2,a2,b2,c,d2,dw(ix),dw(itri))
c     
                  do k=0,ns-1
                     call daxpy(n2,dw(iep+k+2),dw(ix),1,dw(ir+k*n2),1)
                  end do
               end do
c
c Update the solution
c
               do k=0,ns-1
                  ll = (iw(isp+k) - 1)*ldf + 1
                  call dcopy(n2,dw(ir+k*n2),1,f(ll),1)
               end do
            end if
         end do
      end do
c
c Second recursion, bottom level
c
      level = nl
      m = iw(ip4+level-1)
c
      do iloc=1,m
c
c Find the bounds for the strip
c
         call getbnd(level,iloc,ilb,iub,iw(isplit),iw(ip4))
         ilen = iub - ilb - 1
c
         if (ilen.gt.0) then
            if (ilb.ne.0) then
               ll = (ilb - 1)*ldf + 1
               call dcopy(n2,f(ll),1,dw(iv1),1)
            end if
            if (iub.ne.n1+1) then
               ll = (iub - 1)*ldf + 1
               call dcopy(n2,f(ll),1,dw(iv3),1)
            end if
c
            iep = ieig + 6*(ilb + n1*(level - 1))
            ll = ilb*ldf + 1
c
            if (ilen.eq.1) then
c
c Problem with one grid column
c
               call upforc(n1,n2,ilb,iub,f(ll),f(ll),
     &                     a1,d2,dw(iv1),dw(iv3),.true.)
               c = dw(iep+1)**2
               call dscal(n2,c,f(ll),1)
               c = dw(iep) + ch
               call soltri(n2,a2,b2,c,d2,f(ll),dw(itri))
            else if (ilen.eq.2) then
c
c Problem with two grid columns
c
               call upforc(n1,n2,ilb,iub,f(ll),f(ll+ldf),
     &                     a1,d2,dw(iv1),dw(iv3),.true.)
               call soldbl(n2,dw(iep),a2,b2,d2,ch,
     &                     f(ll),ldf,f(ll),ldf,dw(itri))
            else
c
c Problem with three grid columns
c
               call upforc(n1,n2,ilb,iub,f(ll),f(ll+2*ldf),
     &                     a1,d2,dw(iv1),dw(iv3),.true.)
               call soltrb(n2,dw(iep),a2,b2,d2,ch,
     &                     f(ll),ldf,f(ll),ldf,dw(itri),.true.)
            end if
         end if
      end do
c
      return
      end
c
c************************************************************************
c
c Initialization of the data structure containing
c the division into strips
c
      subroutine inispl(n,split,level,p4)
      integer n, split(*), level, p4(*)
c
      integer i, ipp, icp, iend, ilen, id, im, j, k
c
      split(1) = 0
      split(2) = n + 1
      ipp   = 1
      icp   = 3
      level = 1
c
 100  split(icp) = split(ipp)
      icp = icp + 1
      do i=1,p4(level)
         ipp  = ipp + 1
         iend = split(ipp)
         ilen = iend - split(ipp-1)
         k = min((ilen - 1)/4 + 1, 4)
         id = ilen/k
         im = mod(ilen,k)
         do j=1,4
            k = split(icp-1) + id + min(im,1)
            split(icp) = min(k,iend)
            im = max(im-1,0)
            icp  = icp + 1
         end do
      end do
      ipp   = ipp + 1
      level = level + 1
      if (split(ipp+1)-split(ipp).gt.4) goto 100
c
      return
      end
c
c************************************************************************
c
c Find the bounds for a given strip from the data structure
c
      subroutine getbnd(level,loc,ilb,iub,split,p4)
      integer level, loc, ilb, iub, split(*), p4(*)
c
      integer i
c
      i = level + (p4(level) - 1)/3 + loc
      ilb = split(i-1)
      iub = split(i)
c
      return
      end
c
c************************************************************************
c
c Compute the eigenvalues for the generalized eigensystem of length n
c
      subroutine eigval(n,a,b,d,eigen,dw,iw,ierr)
      integer n, iw(5*n), ierr
      double precision a(n), b(n), d(n), eigen(6,n), dw(7*n)
c
      integer i, id, ic, ie, iu, ib, is, iv, m
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
      call dstebz('A','E',n,c,c,i,i,0.d0,dw(id),dw(ic),m,i,dw(ie),
     &            iw(ib),iw(is),dw(iu),iw(iv),ierr)
      if (ierr.ne.0) then
         ierr = 4
         return
      end if
c
      do i=1,n
         eigen(1,i) = dw(ie+i-1)
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
      subroutine eigvec(n,a,b,d,eigen,isp,dw,iw,ierr)
      integer n, isp(*), iw(3*n), ierr
      double precision a(n), b(n), d(n), eigen(6,*), dw(9*n)
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
      do j=1,n
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
c Solve a coupled problem with 3 columns and n rows
c using separation technique
c
      subroutine soltrb(n,eigen,a,b,d,ch,f,ldf,u,ldu,w,midcol)
      integer n, ldf, ldu
      double precision eigen(6,3), a(n), b(n), d(n)
      double precision ch, f(ldf,3), u(ldu,3), w(n)
      logical midcol
c
      integer i
      double precision ev11, ev12, ev13, ev21, ev22, ev23
      double precision ev31, ev32, ev33, u1, u2, u3, c
c
      ev11 = eigen(2,1)
      ev12 = eigen(3,1)
      ev13 = eigen(4,1)
      ev21 = eigen(2,2)
      ev22 = eigen(3,2)
      ev23 = eigen(4,2)
      ev31 = eigen(2,3)
      ev32 = eigen(3,3)
      ev33 = eigen(4,3)
c
c First Fourier transform
c
      do i=1,n
         u1     = f(i,1)
         u2     = f(i,2)
         u3     = f(i,3)
         u(i,1) = ev11*u1 + ev12*u2 + ev13*u3
         u(i,2) = ev21*u1 + ev22*u2 + ev23*u3
         u(i,3) = ev31*u1 + ev32*u2 + ev33*u3
      end do
c
c Solve the tridiagonal systems
c
      do i=1,3
         c = eigen(1,i) + ch
         call soltri(n,a,b,c,d,u(1,i),w)
      end do
c
c Second Fourier transform
c
      if (midcol) then
         do i=1,n
            u1     = u(i,1)
            u2     = u(i,2)
            u3     = u(i,3)
            u(i,1) = ev11*u1 + ev21*u2 + ev31*u3
            u(i,2) = ev12*u1 + ev22*u2 + ev32*u3
            u(i,3) = ev13*u1 + ev23*u2 + ev33*u3
         end do
      else
         do i=1,n
            u1     = u(i,1)
            u2     = u(i,2)
            u3     = u(i,3)
            u(i,1) = ev11*u1 + ev21*u2 + ev31*u3
            u(i,3) = ev13*u1 + ev23*u2 + ev33*u3
         end do
      end if
c
      return
      end
c
c************************************************************************
c
c Solve a coupled problem with 2 columns and n rows
c using separation technique
c
      subroutine soldbl(n,eigen,a,b,d,ch,f,ldf,u,ldu,w)
      integer n, ldf, ldu
      double precision eigen(6,2), a(n), b(n), d(n)
      double precision ch, f(ldf,2), u(ldu,2), w(n)
c
      integer i
      double precision ev11, ev12, ev21, ev22, u1, u2, c
c
      ev11 = eigen(2,1)
      ev12 = eigen(3,1)
      ev21 = eigen(2,2)
      ev22 = eigen(3,2)
c
c First Fourier transform
c
      do i=1,n
         u1     = f(i,1)
         u2     = f(i,2)
         u(i,1) = ev11*u1 + ev12*u2
         u(i,2) = ev21*u1 + ev22*u2
      end do
c
c Solve the tridiagonal systems
c
      do i=1,2
         c = eigen(1,i) + ch
         call soltri(n,a,b,c,d,u(1,i),w)
      end do
c
c Second Fourier transform
c
      do i=1,n
         u1     = u(i,1)
         u2     = u(i,2)
         u(i,1) = ev11*u1 + ev21*u2
         u(i,2) = ev12*u1 + ev22*u2
      end do
c
      return
      end
c
c************************************************************************
c
c Solve a tridiagonal linear system
c
      subroutine soltri(n,a,b,s,c,x,w)
      integer n
      double precision a(n), b(n), s, c(n), x(n), w(n)
c
      integer i
      double precision d, ai, an, xp, wp
c
      d = 1.d0/(b(1) + s*c(1))
      xp = x(1)*d
      x(1) = xp
      if (n.eq.1) return
      an = a(2)
      wp = -an*d
      w(1) = wp
      do i=2,n-1
         ai = an
         d = -1.d0/(b(i) + s*c(i) + ai*wp)
         xp = (ai*xp - x(i))*d
         x(i) = xp
         an = a(i+1)
         wp = an*d
         w(i) = wp
      end do
      d = -1.d0/(b(n) + s*c(n) + an*wp)
      xp = (an*xp - x(n))*d
      x(n) = xp
      do i=n-1,1,-1
         xp = x(i) + w(i)*xp
         x(i) = xp
      end do
c
      return
      end
c
c************************************************************************
c
c Do Fourier transform for 1, 2 or 3 columns
c
      subroutine ftrans(ns,n,x,g,evec)
      integer ns, n
      double precision x(n), g(n,ns), evec(ns)
c
      integer k
      double precision e1, e2, e3
c
      if (ns.eq.3) then
         e1 = evec(1)
         e2 = evec(2)
         e3 = evec(3)
         do k=1,n
            x(k) = e1*g(k,1) + e2*g(k,2) + e3*g(k,3)
         end do
      else if (ns.eq.2) then
         e1 = evec(1)
         e2 = evec(2)
         do k=1,n
            x(k) = e1*g(k,1) + e2*g(k,2)
         end do
      else
         e1 = evec(1)
         do k=1,n
            x(k) = e1*g(k,1)
         end do
      end if
c
      return
      end
c
c************************************************************************
c
c Update a force vector
c
      subroutine upforc(n1,n2,ilb,iub,fl,fu,a1,d2,vl,vu,add)
      integer n1, n2, ilb, iub
      double precision fl(n2), fu(n2), a1(n1), d2(n2), vl(n2), vu(n2)
      logical add
c
      integer k
      double precision c
c
      if (ilb.ne.0) then
         c = -a1(ilb+1)
         if (add) then
            do k=1,n2
               fl(k) = fl(k) + c*d2(k)*vl(k)
            end do
         else
            do k=1,n2
               fl(k) = c*d2(k)*vl(k)
            end do
         end if
      end if
      if (iub.ne.n1+1) then
         c = -a1(iub)
         if (add) then
            do k=1,n2
               fu(k) = fu(k) + c*d2(k)*vu(k)
            end do
         else
            do k=1,n2
               fu(k) = c*d2(k)*vu(k)
            end do
         end if
      end if
c
      return
      end
c************************************************************************
c
c Subroutine: mf2d
c
c Purpose:
c   An unsymmetric multifrontal factorization  method for 
c   solving the block tridiagonal  linear system
c 
c   Au=f,
c
c   where the matrix A is not separable, that is, the system
c   can not be expressed in the form:
c
c   (A1(x)M2 + M1(x)A2 + ch*M1(x)M2)u = f.
c
c   The notation "(x)" denotes the tensor product of the matrices,
c   that is, A(x)B = {a_ij*B}.
c   A1 and A2 are symmetric tridiagonal matrices of dimension
c   n1 and n2, respectively. M1 and M2 are diagonal matrices of the
c   same dimension. 
c
c
c Version: 0.1
c
c Date: 29 Jan 2000
c
c Parameters:
c
c   Input:
c          n1     - The dimension of the matrices A1 and M1
c          n2     - The dimension of the matrices A2 and M2
c          ldf    - The leading dimension of the two-dimensional 
c                   array f; ldf should be at least n2
c          a1     - The codiagonal of the matrix A due to connections
c                   in the first direction (n1)
c          b1     - The diagonal  of the matrix A due to connections
c                   in the first direction (n1)
c          d1     - not used
c          a2     - The codiagonal of the matrix A due to connections
c                   in the second direction (n2)
c          b2     - The diagonal  of the matrix A due to connections
c                   in the second direction (n2)
c          d2     - not used
c          lrw    - The length of real workspace; computed as
c                   include 'staticums.h'
c                   if      ( n2*n1 .le. N2Value(1,2) ) then
c                     Multiplier = N2Value(1,1)
c                   else if ( n2*n1 .le. N2Value(2,2) ) then
c                     Multiplier = N2Value(2,1)
c                   else if ( n2*n1 .le. N2Value(3,2) ) then
c                     Multiplier = N2Value(3,1)
c                   else if ( n2*n1 .le. N2Value(4,2) ) then
c                     Multiplier = N2Value(4,1)
c                   else
c                     Multiplier = N2Value(5,1)
c                   end if
c                   lrw   = 3*Multiplier*n2*n1
c          liw    - The length of integer workspace; computed as
c                   include 'staticums.h'
c                   liw  = 3*N2Index*n2*n1
c          init   - Flag which indicates whether the purpose of
c                   the call is to initialize the data structures
c                   in the workspace (init = .true.) or
c                   to solve the problem (init = .false.);
c                   first the subroutine should be initialized
c                   with init = .true.;
c                   after that the subroutine can be called
c                   several times with init = .false.
c                   provided that the values of n1, n2, ldf,
c                   a1, b1,  a2, b2, rw and iw are unchanged
c
c   Input/output:
c          f      - On entry f contains the right hand side vector;
c                   on successful exit f contains the solution;
c                   The components are stored according to the
c                   block representation (1), that is, the first
c                   n2 components of f contain the block f_1 and so on.
c                   The solution is returned in the same order.
c
c   Output:
c          ierr   - Error flag indicating failure.
c                   Possible return values:
c                   0  no error
c                   1  n1 < 1 or n2 < 1 or ldf < n2
c                   2  real workspace too short;
c                      the required amount of workspace can
c                      be found in iw(1) if liw > 1
c                   3  integer workspace too short;
c                      the required amount of workspace can
c                      be found in iw(2) if liw > 1
c                   4  other problems in UMS2FA,UMS2RF
c                   5  failure in UMS2SO
c                   6  failure convergence in slpcg4
c
c   Workspace:
c          rw     - Real workspace, length at least lrw
c          iw     - Integer workspace, length at least liw
c
c
c Subroutines called:
c   UMS2IN, UMS2FA, UMS2RF, UMS2SO from the UMFPACK library.
c   (file ums.f here). These subroutines
c   can be obtained from the NETLIB archive.
c
c
c Language: FORTRAN
c
c Portability: FORTRAN-77 with do-enddo extension
c
c
c Algorithm:
c   Unsymmetric-pattern MultiFrontal Package (UMFPACK).
c
c Complexity estimate:  asymptotically  
c   O[ (n1*n2)**alpha ] flops, 
c   1.6 <= alpha <= 1.7 on most rectangular meshes
c   ( very seldom alpha may be up to 1.8 )
c   
c
c
c References:
c T. A. Davis and I. S. Duff, "An
c unsymmetric-pattern multifrontal method for sparse LU factorization",
c SIAM J. Matrix Analysis and Applications 
c // Technical report TR-94-038, CISE Dept., Univ. of Florida,
c P.O. Box 116120, Gainesville, FL 32611-6120, USA.  
c
c The method used here is a modification of that method, 
c described in 
c T. A. Davis, "A combined unifrontal/multifrontal method 
c for unsymmetric sparse matrices," TR-94-005, 
c
c and in 
c T. A. Davis and I. S. Duff, (same title), TR-95-020.  
c (Technical reports are available via WWW at http://www.cis.ufl.edu/).
c
c The (unsymmetric) approximate degree update
c algorithm used here has been incorporated into a symmetric approximate
c minimum degree ordering algorithm, described in 
c
c P. Amestoy, T. A. Davis, and I. S. Duff, 
c "An approximate minimum degree ordering algorithm",
c SIAM Journal on Matrix Analysis and Applications // TR-94-039.
c
c       Tim Davis:  http://www.cis.ufl.edu/~davis
c                   (also email: davis@cis.ufl.edu).
c       Iain Duff:  http://www.cis.rl.ac.uk/people/isd/contact.html
c
c
c Author: Yuri Vassilevski   (vasilevs@ticam.utexas.edu),
c
c Address: The University of Texas at Austin
c          TICAM
c
c************************************************************************
      subroutine mf2d(n1,n2,f,ldf,a1,b1,d1,a2,b2,d2,ch,
     &                rw,lrw,iw,liw,init,ierr)
      integer n1, n2, ldf, lrw, liw, iw(liw), ierr
      double precision f(ldf*n1), a1(*), b1(*), d1(*)
      double precision a2(*), b2(*), d2(*), ch
      real             rw(lrw)
      logical init
c
      integer pf,pw,px,pe
      integer i,j,ic,nel

      real Cntl(10,3), Rinfo(20,3)
      integer Icntl(20,3), Keep(20,3), InfoUMS(40,3),norder
      common/ums/ Cntl,Rinfo,Icntl,Keep,InfoUMS,norder
      save/ums/




      integer iorder,lrwRest(4),liwRest(4),ITER
      real RESID
      double precision chord

  
      logical ONCEONLY,         refactorize
      common/oncemf2d/ ONCEONLY,refactorize
      save /oncemf2d/
      data ONCEONLY /.true./
      external multpcg, precpcg
c
      ierr = 0
c
      if (n1.lt.1.or.n2.lt.1.or.ldf.lt.n2) then
         ierr = 1
         return
      end if
      if (liw.lt.2) then
         ierr = 3
         write(*,*) "liw<2"
         return
      end if
c

      call fillmatrix(n1,n2,b1,b2,a1,a2,ch,rw,iw,nel)

         
      IF (init) THEN
        refactorize = .true.
        if (ONCEONLY) then
         ONCEONLY = .false.
         lrwRest(1) = lrw - 8*n1*n2
         liwRest(1) = liw
         norder = n1*n2
         do iorder = 1,3
          chord = 1d1**(iorder-1)
          call fillmatrix(n1,n2,b1,b2,a1,a2,chord,rw,iw,nel)
          call UMS2IN(Icntl(1,iorder), Cntl(1,iorder), Keep(1,iorder))
C-- Preserve symmetry
          Icntl(6,iorder) = 1
C-- Silence
          Icntl(3,iorder) = 0
C-- No iterative refinement
          Icntl(8,iorder) = 0

c         call UMS2FA(n1*n2,nel,0,.FALSE.,lrw,liw,rw,iw,
          call UMS2FA(n1*n2,nel,0,lrwRest(iorder),liwRest(iorder),rw,iw,
     &               Keep(1,iorder),Cntl(1,iorder),Icntl(1,iorder),
     &               InfoUMS(1,iorder),Rinfo(1,iorder) )
          iw(1) = InfoUMS(20,iorder)+8*n1*n2
          iw(2) = InfoUMS(18,iorder)
          lrwRest(iorder+1) = lrwRest(iorder) - (InfoUMS(23,iorder)+1)
          liwRest(iorder+1) = liwRest(iorder) - (InfoUMS(22,iorder)+1)
          if (InfoUMS(1,iorder).lt.0) then
            ierr = 4
            if (InfoUMS(1,iorder).eq.-3) ierr = 3
            if (InfoUMS(1,iorder).eq.-4) ierr = 2
            iw(1) = InfoUMS(21,iorder)+8*n1*n2
            iw(2) = InfoUMS(19,iorder)
            return
          end if
         end do
        end if
      ELSE
        if (refactorize) then
         lrwRest(1) = lrw - 8*n1*n2
         liwRest(1) = liw
         do iorder = 1,3
          chord = 1d1**iorder/3d0
          call fillmatrix(n1,n2,b1,b2,a1,a2,chord,rw,iw,nel)
c         call UMS2RF(n1*n2,nel,0,.FALSE.,lrw,liw,rw,iw,
          call UMS2RF(n1*n2,nel,0,lrwRest(iorder),liwRest(iorder),rw,iw,
     &               Keep(1,iorder),Cntl(1,iorder),Icntl(1,iorder),
     &               InfoUMS(1,iorder),Rinfo(1,iorder) )
          lrwRest(iorder+1) = lrwRest(iorder) - (InfoUMS(23,iorder)+1)
          liwRest(iorder+1) = liwRest(iorder) - (InfoUMS(22,iorder)+1)
          if (InfoUMS(1,iorder).lt.0) then
            ierr = 4
            if (InfoUMS(1,iorder).eq.-3) ierr = 3
            if (InfoUMS(1,iorder).eq.-4) ierr = 2
            return
          end if
         end do
         refactorize = .false.
        end if

        pf = lrw - n1*n2+1
        px = pf - n1*n2
        pw = px - 4*n1*n2
        ic = 0
        do i = 1, n1
         do j = 1, n2
          rw(pf+ic) = f((i-1)*ldf+j)
          rw(px+ic) = 0.
          ic = ic+1
         end do
        end do

        ITER = 100
        RESID = 0.1*sqrt(sdot( n1*n2, rw(pf), 1, rw(pf), 1 ))
        if (ch.le.10) then
           iorder = 1
        else if (ch.le.100) then
           iorder = 2
        else if (ch.le.2000) then
           iorder = 3
        else
           iorder = 4
        end if

        call slpcg4( precpcg, iorder, rw,iw,
     >               multpcg, n1,n2,b1,b2,a1,a2,ch,
     >               rw(pw), n1*n2, 4,
     >               n1*n2, rw(pf), rw(px), 
     >               ITER, RESID,  ierr, 0 )
c       write(*,*)iorder,ITER
        if (ierr.ne.0) then
           ierr = 6
           return
        end if

        ic = 0
        do i = 1, n1
         do j = 1, n2
          f((i-1)*ldf+j) = rw(px+ic)
          ic = ic+1
         end do
        end do
      END IF


      return
      end

      subroutine fillmatrix(n1,n2,b1,b2,a1,a2,ch,rw,iw,nel)
      implicit none
      integer n1,n2
      double precision  a1(*),a2(*),b1(*),b2(*), ch

      integer iw(*), nel
      real    rw(*)


      integer i,j,ic


      nel = 5*n1*n2
      ic = 1
      do i = 1, n1
      do j = 1, n2
         rw(ic) = b1((i-1)*n2+j) + b2((i-1)*n2+j) + ch
         iw(ic) = (i-1)*n2+j
         iw(ic+nel) = (i-1)*n2+j
         ic = ic+1
      end do
      end do
      do i = 1, n1
      do j = 1, n2-1
         rw(ic) = a2((i-1)*n2+j+1)
         iw(ic) = (i-1)*n2+j
         iw(ic+nel) = (i-1)*n2+j+1
         ic = ic+1
      end do
      end do
      do i = 1, n1
      do j = 2, n2
         rw(ic) = a2((i-1)*n2+j)
         iw(ic) = (i-1)*n2+j
         iw(ic+nel) = (i-1)*n2+j-1
         ic = ic+1
      end do
      end do
      do i = 1, n1-1
      do j = 1, n2
         rw(ic) = a1((i-0)*n2+j)
         iw(ic) = (i-1)*n2+j
         iw(ic+nel) = (i-0)*n2+j
         ic = ic+1
      end do
      end do
      do i = 2, n1
      do j = 1, n2
         rw(ic) = a1((i-1)*n2+j)
         iw(ic) = (i-1)*n2+j
         iw(ic+nel) = (i-2)*n2+j
         ic = ic+1
      end do
      end do
      do i = 1,ic-1
       iw(ic-1+i) = iw(nel+i)
      end do
      nel = ic-1

      return
      end

      subroutine multpcg( A, X, Y,
     &           n1,n2,b1,b2,a1,a2,ch)


      implicit none
      real    X(*),Y(*),A

      integer n1,n2
      double precision  a1(*),a2(*),b1(*),b2(*), ch

      integer i,j
      double precision     c

      do i = 1, n1
      do j = 1, n2
         c = b1((i-1)*n2+j) + b2((i-1)*n2+j) + ch
         Y( (i-1)*n2+j ) = c*X( (i-1)*n2+j )
      end do
      end do
      do i = 1, n1
      do j = 1, n2-1
         c = a2((i-1)*n2+j+1)
         Y( (i-1)*n2+j ) = Y( (i-1)*n2+j ) + c*X( (i-1)*n2+j+1 )
      end do
      end do
      do i = 1, n1
      do j = 2, n2
         c = a2((i-1)*n2+j)
         Y( (i-1)*n2+j ) = Y( (i-1)*n2+j ) + c*X( (i-1)*n2+j-1 )
      end do
      end do
      do i = 1, n1-1
      do j = 1, n2
         c = a1((i-0)*n2+j)
         Y( (i-1)*n2+j ) = Y( (i-1)*n2+j ) + c*X( (i-0)*n2+j )
      end do
      end do
      do i = 2, n1
      do j = 1, n2
         c = a1((i-1)*n2+j)
         Y( (i-1)*n2+j ) = Y( (i-1)*n2+j ) + c*X( (i-2)*n2+j )
      end do
      end do

      do i = 1, n1
      do j = 1, n2
         Y( (i-1)*n2+j ) =  A*Y( (i-1)*n2+j )
      end do
      end do

      return
      end

      subroutine precpcg( iorder, rw,iw, F, X )
      implicit none

      real rw(*), X(*), F(*)
      integer iw(*), iorder

      real Cntl(10,3), Rinfo(20,3)
      integer Icntl(20,3), Keep(20,3), InfoUMS(40,3),norder
      common/ums/ Cntl,Rinfo,Icntl,Keep,InfoUMS,norder
      save/ums/


      integer lrw,liw,pws

      if (iorder.eq.4) then
         call scopy( norder, F, 1, X, 1 )
         return
      end if

      lrw = Keep(2,iorder)
      liw = Keep(5,iorder)
      pws = Keep(2,1)+1
     

 
      call UMS2SO(norder,0,lrw,liw,rw,iw,Keep(1,iorder),
     &            F,X,rw(pws),Cntl(1,iorder),Icntl(1,iorder),
     &            InfoUMS(1,iorder),Rinfo(1,iorder))


      return
      end 

c---->------------------------------------------------------------------<
c  Preconditioned conjugate gradient method
c  Templates for the solution if linear Systems...
c  http://www.netlib.org
c---->------------------------------------------------------------------<
      SUBROUTINE slpcg4(
     >  prevec, IPREVEC,rw,iw,
     >  matvec, n1,n2,b1,b2,a1,a2,ch,
     >  WORK, MW, NW,
     >  N, RHS, SOL,
     >  ITER, RESID,
     >  INFO, NUNIT )
c---->
      IMPLICIT NONE
c---->------------------------------------------------------------------<
c  Argument types:
c
      EXTERNAL  matvec, prevec
      INTEGER   IPREVEC(*)
      INTEGER   N, MW, NW, ITER, INFO, NUNIT
      REAL      RESID
      REAL      RHS(*), SOL(*), WORK(MW,NW)
c---->
c  Argument Descriptions:
c
c  prevec   : extern : Precondition-vector routine
c  IPREVEC  : input  : Configuration data for 'prevec'
c  matvec   : extern : Matrix-vector multiply routine
      integer n1,n2
      double precision  a1(*),a2(*),b1(*),b2(*), ch
c
c  WORK     : work   : Workspace (MW,NW)
c  MW       : input  : leading  dimension of workspace >= N
c  NW       : input  : trailing dimension of workspace >= 4
c
c  N        : input  : Length of vectors
c  RHS      : input  : RHS vector
c  SOL      : in/out : Initial guess / iterated solution
c  ITER     : in/out : Maximum iterations / actual iterations
c  RESID    : in/out : Convergence target / Norm of final residual
c  INFO     : output : = 0, converged
c                    : > 0, did not converge
c                    : < 0, error with input
c---->
c  External routine specifications:
c
c    matvec( A, X, Y )  <=>  Y = A * Mat * X
c    prevec( IPREVEC, rw,iw, X, Y )  <=>  Y = (MatP_{i})^{-1} * X
      REAL rw(*)
      INTEGER iw(*)
c      where MatP is the approximation of Mat
c---->------------------------------------------------------------------<
c  Local Parameters
c
      REAL    ZERO,ONE
      PARAMETER ( ZERO = 0.0 , ONE = 1.0 )
c---->------------------------------------------------------------------<
c  Local Variables:
c
      INTEGER MAXIT
      INTEGER JR, JP, JQ, JZ
      REAL  RHO, RHOPREV, ALPHA, BETA, RNORM, TOL, TMP, TMP2
c
c---->------------------------------------------------------------------<
c  External BLAS, etc.:
c
      EXTERNAL  sdot,saxpy,scopy,sscal
      REAL    sdot
      INTRINSIC sqrt, min, abs
c---->------------------------------------------------------------------<
c
c    Test the input parameters.
c
      INFO = 0
c
      if ( N .eq. 0 ) then
         return
      else if ( N .lt. 0 ) then
         INFO = -10
      else if ( MW .lt. N ) then
         INFO = -20
      else if ( NW .lt. 4 ) then
         INFO = -30
      else if ( ITER .le. 0 ) then
         INFO = -40
      endif
c
      if ( INFO .ne. 0 ) return
c---->------------------------------------------------------------------<
c  Save input iteration limit and convergence tolerance
c
      MAXIT = ITER
      TOL   = RESID
c---->
c  Alias workspace columns.
c
      JR  = 1
      JP  = JR + 1
      JQ  = JP + 1
      JZ  = JQ + 1
c---->
c  Set initial residual
c
      call scopy( N, RHS, 1, WORK(1,JR), 1 )
c
      TMP2 = sdot( N, SOL, 1, SOL, 1 )
      if ( TMP2 .ne. ZERO ) then
        call matvec( -ONE, SOL, WORK(1,JR) ,
     &                n1,n2,b1,b2,a1,a2,ch)
        call saxpy( N, ONE, RHS, 1, WORK(1,JR), 1 )
      endif
c---->
      TMP2 = sdot( N, WORK(1,JR), 1, WORK(1,JR), 1 )
      RESID = sqrt( TMP2 )
c---->
      ITER = 0
      if ( RESID .le. TOL ) GOTO 20
c---->------------------------------------------------------------------<
c  PCG  iteration point
c---->--
   10   continue
c
          ITER = ITER + 1
c---->----
          call prevec( IPREVEC, rw,iw, WORK(1,JR), WORK(1,JZ) )

          RHOPREV = RHO
          RHO = sdot( N, WORK(1,JR), 1, WORK(1,JZ), 1 )

          IF (ITER.eq.1) THEN
             call scopy( N, WORK(1,JZ), 1, WORK(1,JP), 1 )
          ELSE
             BETA = RHO / RHOPREV
             call sscal( N, BETA, WORK(1,JP), 1 )
             call saxpy( N, ONE, WORK(1,JZ), 1, WORK(1,JP), 1 )
          END IF

          call matvec( ONE, WORK(1,JP), WORK(1,JQ),
     &                n1,n2,b1,b2,a1,a2,ch)

          TMP2 = sdot( N, WORK(1,JP), 1, WORK(1,JQ), 1 )
          ALPHA = RHO / TMP2

          call saxpy( N, ALPHA, WORK(1,JP), 1, SOL, 1 )
          call saxpy( N,-ALPHA, WORK(1,JQ), 1, WORK(1,JR), 1 )
c---->----
c  Check convergence
          TMP2 = sdot( N, WORK(1,JR), 1, WORK(1,JR), 1 )
          RESID = sqrt( TMP2 )
c---->------------------------------------------------------------------<
c  Continue PCG loop while:
c    1)  Less than maximum iteration
c    2)  Have not converged
c         print*,'pcg: ',ITER, RESID
c
          if ( ITER .lt. MAXIT .and. RESID .ge. TOL ) go to 10
c---->--
c
c  Convergence failure?
c
        if ( ITER .ge. MAXIT .and. RESID .ge. TOL ) INFO = 1
c---->------------------------------------------------------------------<
c  Output
c
  20    continue
        TMP2 = sdot( N , SOL, 1, SOL, 1 )
        TMP2 = sqrt( TMP2 )
        if ( NUNIT .gt. 0 ) then
          WRITE(NUNIT,9000) ITER,RESID,TMP2
c    &    , '^[[A'
 9000     FORMAT(3x,'SLPCG ',I4,' : ',E10.3,' (SOL ',E10.3,')')
c9000     FORMAT(3x,'SLPCG ',I4,' : ',E16.10,' (SOL ',E16.10,')')
        end if
c---->------------------------------------------------------------------<
      return
      end
c---->------------------------------------------------------------------<
