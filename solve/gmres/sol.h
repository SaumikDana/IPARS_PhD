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
c pcg_prec  = choice of preconditioner for pcg package
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer lsol_itmax, lsol_absflag,n_gs_step,prec
      real*4 lsol_tol,lsol_atol
      common/lsolc/lsol_itmax, lsol_absflag,lsol_tol, lsol_atol,
     &     n_gs_step,prec

c ----------------------------------------------------------------



