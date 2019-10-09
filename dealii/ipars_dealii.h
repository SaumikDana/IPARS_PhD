// --------------------------------------------------------------------
// ipars_dealii.h
// Header file for C++ interface to IPARS library subroutines,
// used for IPARS-deal.II coupling.
// Ben Ganis
// 2/10/17
// --------------------------------------------------------------------

extern "C"
{
  void ipars_start_(int *nerr);
  void ipars_finish_(int *nerr);
  void ipars_sendgrid_(double *xLow, double *xHigh, int *ndim, int *Ncoarse,
    int *Nlev, int *Nele, int *grid, int *adaptivity, int *nerr);
  void ipars_init_(int *nerr);
  void ipars_set_array_pointers_(double *perm, double *pres, int *nerr);
  void ipars_sendperm_(int *nerr);
  void ipars_sendpres_(int *nerr);
  void ipars_recvpres_(int *nerr);
  void ipars_solve_(int *nstep, double *tim, double *deltim, int *nerr);
  void ipars_visualize_(int *nerr);
  void ipars_reinit_(int *Nele, int *grid, int *nerr);
}

