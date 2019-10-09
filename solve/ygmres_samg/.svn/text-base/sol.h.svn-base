cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c file:  sol.h
c  
c contains parameters for PCG convergence
c lsol_itmax, 
c lsol_absflag = 
c    it specifies if absolute or relative tol. is to be used
c 	if absflag =0 ONLY relative tolerance is checked (original version)
c       if absflag =1 ONLY absolute tolerance is checked
c       if absflag =2 absolute tolerance OR relative must be satisfied
c       if absflag =3 absolute tolerance AND relative  must be satisfied 
c lsol_tol  = parameter for relative convergence check
c lsol_atol = parameter for absolute convergence check 
c n_gs_step = parameter for number of times the block Gauss-Seidel 
c             "inverse pressure" routine is called (for GMRES solver)
c prec      = choice of preconditioner for  gmres package
c forcing   = stwitch for using the forcing term (0 - off(default), 
c             a>0 - on, classic forcing term being muliplied by a)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer lsol_itmax, lsol_absflag,n_gs_step,prec
      logical samg 
      real*4 lsol_tol,lsol_atol,forcing
      common/lsolc/lsol_itmax, lsol_absflag,lsol_tol, lsol_atol,
     &     n_gs_step,prec,forcing,samg
      save/lsolc/

c ----------------------------------------------------------------



