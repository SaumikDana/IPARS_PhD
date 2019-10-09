//****************************************************************************************
// file: samg.h
// automatically created by tool make_amgparms_user
// created on (yyyy-mm-dd): 2005-06-10, time: 13:40
//****************************************************************************************

// INTERFACE TO FORTRAN90 SAMG : amg subroutines have
// to be called as SAMG_XXXX when this interface is used
// author: Tanja Clees
// last modified: 2003/07/31 TC
//                2003/11/07 TC (SAMG_RESET_SECONDARY... added;
//                               handling of FILNAM_DUMP added)
//                2003/11/26 TC (SAMG_USER_COO removed)
//                2004/03/04 TC (SAMG_C_STDCALL renamed to SAMG_C_CALLCONV,
//                               SAMG_CVF and Intel support added)
//                2004/04/15 TC (handling of FILNAM_DUMP modified)
//                2005/01/12 TC (samg.h now created automatically, see above!)

////////////
/* Macros */
////////////

/* subroutine naming conventions.
for Unix / Linux: depending on the names of the Fortran symbols in libamg.a,
   choose one of the following macros via a -D option for the C++ compiler
   in your make.inc (as has already been done in the include files provided!):
      SAMG_LCASE
      SAMG_LCASE_USCORE   (see below what they do)
   if none of the above matches the naming conventions for your system,
   define appropriate macros yourself!
*/

/* SAMG_C_CALLCONV: macro (for Windows!) defining the standard calling convention.
   for Unix / Linux: set -DSAMG_UNIX_LINUX as an option for the C++ compiler
   in your make.inc (as has already been done in the include files provided!) */
#if SAMG_UNIX_LINUX
#define SAMG_C_CALLCONV void
#elif SAMG_CVF
// for Compaq Visual Fortran (tested with 6.6 X) :
#define SAMG_C_CALLCONV void __stdcall
#else
// for Intel Visual Fortran (tested with 8.X) :
#define SAMG_C_CALLCONV void __cdecl
#endif


/////////////////////////////////////
/* directives for subroutine names */
/////////////////////////////////////

#ifdef SAMG_LCASE
#define SAMG samg
#define SAMG_CTIME samg_ctime
#define SAMG_SIMPLE samg_simple
#define SAMG_MAIN samg_main
#define SAMG_LEAVE samg_leave
#define SAMG_REFRESH samg_refresh
#define SAMG_CLEANUP samg_cleanup
#define SAMG_RESET_SECONDARY samg_reset_secondary
#define SAMG_RESET_HIDDEN samg_reset_hidden
#define SAMG_GET_LEVELS_CREATED samg_get_levels_created
#define SAMG_GET_MEM_ACTIVE samg_get_mem_active
#define SAMG_GET_TIME_AND_MEM samg_get_time_and_mem
#define SAMG_CVEC_ALLOC samg_cvec_alloc
#define SAMG_CVEC_DEALLOC samg_cvec_dealloc
#define SAMG_CVEC_SET samg_cvec_set
#define SAMG_CVEC_GET samg_cvec_get
#define SAMG_OMEGA_JACOBI_ALLOC samg_omega_jacobi_alloc
#define SAMG_OMEGA_JACOBI_DEALLOC samg_omega_jacobi_dealloc
#define SAMG_OMEGA_JACOBI_SET samg_omega_jacobi_set
#define SAMG_CHECK_LICENSE samg_check_license
#define SAMG_CURRENT_RESIDUAL samg_current_residual
#define SAMG_USER_COO samg_user_coo
#define SAMG_GET_NTHREAD_MAX samg_get_nthread_max
#define SAMG_SET_NTHREAD_MAX samg_set_nthread_max
#define SAMG_GET_NMAX_MATRIX samg_get_nmax_matrix
#define SAMG_SET_NMAX_MATRIX samg_set_nmax_matrix
#define SAMG_GET_NMAX_MATRIX_RESC samg_get_nmax_matrix_resc
#define SAMG_SET_NMAX_MATRIX_RESC samg_set_nmax_matrix_resc
#define SAMG_GET_NMAX_VECTOR samg_get_nmax_vector
#define SAMG_SET_NMAX_VECTOR samg_set_nmax_matrix
#define SAMG_GET_IRESTRICTION_OPENMP samg_get_irestriction_openmp
#define SAMG_SET_IRESTRICTION_OPENMP samg_set_irestriction_openmp
#define SAMG_SET_A_CMPLX_AGG_DEFAULT samg_set_a_cmplx_agg_default
#define SAMG_GET_A_CMPLX_AGG_DEFAULT samg_get_a_cmplx_agg_default
#define SAMG_SET_A_CMPLX_DEFAULT samg_set_a_cmplx_default
#define SAMG_GET_A_CMPLX_DEFAULT samg_get_a_cmplx_default
#define SAMG_SET_ALLOW_ELIM samg_set_allow_elim
#define SAMG_GET_ALLOW_ELIM samg_get_allow_elim
#define SAMG_SET_ALLUNS_AT_ALLPNTS samg_set_alluns_at_allpnts
#define SAMG_GET_ALLUNS_AT_ALLPNTS samg_get_alluns_at_allpnts
#define SAMG_SET_B_CMPLX samg_set_b_cmplx
#define SAMG_GET_B_CMPLX samg_get_b_cmplx
#define SAMG_SET_B_CMPLX_AGG_DEFAULT samg_set_b_cmplx_agg_default
#define SAMG_GET_B_CMPLX_AGG_DEFAULT samg_get_b_cmplx_agg_default
#define SAMG_SET_B_CMPLX_DEFAULT samg_set_b_cmplx_default
#define SAMG_GET_B_CMPLX_DEFAULT samg_get_b_cmplx_default
#define SAMG_CNTRL_SET_BACKUP samg_cntrl_set_backup
#define SAMG_CNTRL_GET_BACKUP samg_cntrl_get_backup
#define SAMG_SET_BLK_FILLEXP samg_set_blk_fillexp
#define SAMG_GET_BLK_FILLEXP samg_get_blk_fillexp
#define SAMG_SET_BLK_STAB samg_set_blk_stab
#define SAMG_GET_BLK_STAB samg_get_blk_stab
#define SAMG_SET_CHECK_ALLPNTS samg_set_check_allpnts
#define SAMG_GET_CHECK_ALLPNTS samg_get_check_allpnts
#define SAMG_SET_CHECK_ORDER samg_set_check_order
#define SAMG_GET_CHECK_ORDER samg_get_check_order
#define SAMG_SET_CONV_STOP_DEFAULT samg_set_conv_stop_default
#define SAMG_GET_CONV_STOP_DEFAULT samg_get_conv_stop_default
#define SAMG_SET_CSET_LASTPNT samg_set_cset_lastpnt
#define SAMG_GET_CSET_LASTPNT samg_get_cset_lastpnt
#define SAMG_SET_CSET_LESSVARS samg_set_cset_lessvars
#define SAMG_GET_CSET_LESSVARS samg_get_cset_lessvars
#define SAMG_SET_CSET_LONGROW samg_set_cset_longrow
#define SAMG_GET_CSET_LONGROW samg_get_cset_longrow
#define SAMG_ISET_CSET_READ samg_iset_cset_read
#define SAMG_IGET_CSET_READ samg_iget_cset_read
#define SAMG_SET_CSET_ZERODIAG samg_set_cset_zerodiag
#define SAMG_GET_CSET_ZERODIAG samg_get_cset_zerodiag
#define SAMG_SET_DELTA_MILU samg_set_delta_milu
#define SAMG_GET_DELTA_MILU samg_get_delta_milu
#define SAMG_SET_DENSX samg_set_densx
#define SAMG_GET_DENSX samg_get_densx
#define SAMG_CNTRL_SET_DIVERGENCE samg_cntrl_set_divergence
#define SAMG_CNTRL_GET_DIVERGENCE samg_cntrl_get_divergence
#define SAMG_SET_DROPTOL samg_set_droptol
#define SAMG_GET_DROPTOL samg_get_droptol
#define SAMG_SET_DROPTOL_CL samg_set_droptol_cl
#define SAMG_GET_DROPTOL_CL samg_get_droptol_cl
#define SAMG_SET_DROPTOL_SMO samg_set_droptol_smo
#define SAMG_GET_DROPTOL_SMO samg_get_droptol_smo
#define SAMG_SET_DUMP_CORRECTW samg_set_dump_correctw
#define SAMG_GET_DUMP_CORRECTW samg_get_dump_correctw
#define SAMG_SET_ECG samg_set_ecg
#define SAMG_GET_ECG samg_get_ecg
#define SAMG_SET_ECG_DEFAULT samg_set_ecg_default
#define SAMG_GET_ECG_DEFAULT samg_get_ecg_default
#define SAMG_SET_EPS_ABS samg_set_eps_abs
#define SAMG_GET_EPS_ABS samg_get_eps_abs
#define SAMG_SET_EPS_DD samg_set_eps_dd
#define SAMG_GET_EPS_DD samg_get_eps_dd
#define SAMG_SET_EPS_DIAG samg_set_eps_diag
#define SAMG_GET_EPS_DIAG samg_get_eps_diag
#define SAMG_SET_EPS_LSQ samg_set_eps_lsq
#define SAMG_GET_EPS_LSQ samg_get_eps_lsq
#define SAMG_SET_ETR samg_set_etr
#define SAMG_GET_ETR samg_get_etr
#define SAMG_SET_ETR_DEFAULT samg_set_etr_default
#define SAMG_GET_ETR_DEFAULT samg_get_etr_default
#define SAMG_SET_EWT samg_set_ewt
#define SAMG_GET_EWT samg_get_ewt
#define SAMG_SET_EWT_DEFAULT samg_set_ewt_default
#define SAMG_GET_EWT_DEFAULT samg_get_ewt_default
#define SAMG_SET_FACTOR_APP_VAR samg_set_factor_app_var
#define SAMG_GET_FACTOR_APP_VAR samg_get_factor_app_var
#define SAMG_SET_FACTOR_QUASI_RES samg_set_factor_quasi_res
#define SAMG_GET_FACTOR_QUASI_RES samg_get_factor_quasi_res
#define SAMG_SET_FACTOR_RES_VAR samg_set_factor_res_var
#define SAMG_GET_FACTOR_RES_VAR samg_get_factor_res_var
#define SAMG_ISET_FILNAM samg_iset_filnam
#define SAMG_IGET_FILNAM samg_iget_filnam
#define SAMG_ISET_FILNAM_DUMP samg_iset_filnam_dump
#define SAMG_IGET_FILNAM_DUMP samg_iget_filnam_dump
#define SAMG_SET_FULL_PIVOTING samg_set_full_pivoting
#define SAMG_GET_FULL_PIVOTING samg_get_full_pivoting
#define SAMG_CNTRL_SET_FULL_SETUP samg_cntrl_set_full_setup
#define SAMG_CNTRL_GET_FULL_SETUP samg_cntrl_get_full_setup
#define SAMG_SET_G_CMPLX_AGG_DEFAULT samg_set_g_cmplx_agg_default
#define SAMG_GET_G_CMPLX_AGG_DEFAULT samg_get_g_cmplx_agg_default
#define SAMG_SET_G_CMPLX_DEFAULT samg_set_g_cmplx_default
#define SAMG_GET_G_CMPLX_DEFAULT samg_get_g_cmplx_default
#define SAMG_SET_GMAX_MULTIPASS samg_set_gmax_multipass
#define SAMG_GET_GMAX_MULTIPASS samg_get_gmax_multipass
#define SAMG_SET_IAUTO_STOP samg_set_iauto_stop
#define SAMG_GET_IAUTO_STOP samg_get_iauto_stop
#define SAMG_SET_IB_CMPLX samg_set_ib_cmplx
#define SAMG_GET_IB_CMPLX samg_get_ib_cmplx
#define SAMG_SET_IB_CMPLX_AGG_DEFAULT samg_set_ib_cmplx_agg_default
#define SAMG_GET_IB_CMPLX_AGG_DEFAULT samg_get_ib_cmplx_agg_default
#define SAMG_SET_IB_CMPLX_DEFAULT samg_set_ib_cmplx_default
#define SAMG_GET_IB_CMPLX_DEFAULT samg_get_ib_cmplx_default
#define SAMG_SET_IBGS_PIVOT samg_set_ibgs_pivot
#define SAMG_GET_IBGS_PIVOT samg_get_ibgs_pivot
#define SAMG_SET_ICRITS samg_set_icrits
#define SAMG_GET_ICRITS samg_get_icrits
#define SAMG_SET_ILU_SPEED samg_set_ilu_speed
#define SAMG_GET_ILU_SPEED samg_get_ilu_speed
#define SAMG_SET_IODUMP samg_set_iodump
#define SAMG_GET_IODUMP samg_get_iodump
#define SAMG_ISET_IOFILE_OPTA samg_iset_iofile_opta
#define SAMG_IGET_IOFILE_OPTA samg_iget_iofile_opta
#define SAMG_ISET_IOFORM samg_iset_ioform
#define SAMG_IGET_IOFORM samg_iget_ioform
#define SAMG_SET_IOGRID samg_set_iogrid
#define SAMG_GET_IOGRID samg_get_iogrid
#define SAMG_SET_IOMOVIE samg_set_iomovie
#define SAMG_GET_IOMOVIE samg_get_iomovie
#define SAMG_SET_IOSCRATCH_DEFAULT samg_set_ioscratch_default
#define SAMG_GET_IOSCRATCH_DEFAULT samg_get_ioscratch_default
#define SAMG_SET_IOUNIT_OPTA samg_set_iounit_opta
#define SAMG_GET_IOUNIT_OPTA samg_get_iounit_opta
#define SAMG_SET_IPASS_MAX_SET samg_set_ipass_max_set
#define SAMG_GET_IPASS_MAX_SET samg_get_ipass_max_set
#define SAMG_SET_IRESTRICTION_OPENMP samg_set_irestriction_openmp
#define SAMG_GET_IRESTRICTION_OPENMP samg_get_irestriction_openmp
#define SAMG_SET_ISET_VIO_DD samg_set_iset_vio_dd
#define SAMG_GET_ISET_VIO_DD samg_get_iset_vio_dd
#define SAMG_SET_ITER_CHECK samg_set_iter_check
#define SAMG_GET_ITER_CHECK samg_get_iter_check
#define SAMG_SET_ITER_PRE samg_set_iter_pre
#define SAMG_GET_ITER_PRE samg_get_iter_pre
#define SAMG_SET_ITMAX_CONV_DEFAULT samg_set_itmax_conv_default
#define SAMG_GET_ITMAX_CONV_DEFAULT samg_get_itmax_conv_default
#define SAMG_SET_LASTGRID samg_set_lastgrid
#define SAMG_GET_LASTGRID samg_get_lastgrid
#define SAMG_SET_LEVELX samg_set_levelx
#define SAMG_GET_LEVELX samg_get_levelx
#define SAMG_SET_LFIL_CL_DEFAULT samg_set_lfil_cl_default
#define SAMG_GET_LFIL_CL_DEFAULT samg_get_lfil_cl_default
#define SAMG_SET_LFIL_SMO samg_set_lfil_smo
#define SAMG_GET_LFIL_SMO samg_get_lfil_smo
#define SAMG_ISET_LOGFILE samg_iset_logfile
#define SAMG_IGET_LOGFILE samg_iget_logfile
#define SAMG_SET_LOGIO samg_set_logio
#define SAMG_GET_LOGIO samg_get_logio
#define SAMG_CNTRL_SET_MAX_CALLS samg_cntrl_set_max_calls
#define SAMG_CNTRL_GET_MAX_CALLS samg_cntrl_get_max_calls
#define SAMG_SET_MAX_LEVEL samg_set_max_level
#define SAMG_GET_MAX_LEVEL samg_get_max_level
#define SAMG_SET_MAXOP_RESTART samg_set_maxop_restart
#define SAMG_GET_MAXOP_RESTART samg_get_maxop_restart
#define SAMG_SET_MILU samg_set_milu
#define SAMG_GET_MILU samg_get_milu
#define SAMG_CNTRL_SET_MODE_CNTRL samg_cntrl_set_mode_cntrl
#define SAMG_CNTRL_GET_MODE_CNTRL samg_cntrl_get_mode_cntrl
#define SAMG_SET_MODE_DEBUG samg_set_mode_debug
#define SAMG_GET_MODE_DEBUG samg_get_mode_debug
#define SAMG_SET_MODE_MESS samg_set_mode_mess
#define SAMG_GET_MODE_MESS samg_get_mode_mess
#define SAMG_SET_MODIFY_MAT samg_set_modify_mat
#define SAMG_GET_MODIFY_MAT samg_get_modify_mat
#define SAMG_SET_MULTIPASS_ALLCOUP samg_set_multipass_allcoup
#define SAMG_GET_MULTIPASS_ALLCOUP samg_get_multipass_allcoup
#define SAMG_SET_NBLK_DEBUG samg_set_nblk_debug
#define SAMG_GET_NBLK_DEBUG samg_get_nblk_debug
#define SAMG_SET_NBLK_MAX samg_set_nblk_max
#define SAMG_GET_NBLK_MAX samg_get_nblk_max
#define SAMG_SET_NBLK_OVERLAP samg_set_nblk_overlap
#define SAMG_GET_NBLK_OVERLAP samg_get_nblk_overlap
#define SAMG_SET_NBLK_RESID samg_set_nblk_resid
#define SAMG_GET_NBLK_RESID samg_get_nblk_resid
#define SAMG_SET_NBLK_SOLVE samg_set_nblk_solve
#define SAMG_GET_NBLK_SOLVE samg_get_nblk_solve
#define SAMG_SET_NBLK_SOLVER samg_set_nblk_solver
#define SAMG_GET_NBLK_SOLVER samg_get_nblk_solver
#define SAMG_SET_NCFRAMES samg_set_ncframes
#define SAMG_GET_NCFRAMES samg_get_ncframes
#define SAMG_SET_NCG samg_set_ncg
#define SAMG_GET_NCG samg_get_ncg
#define SAMG_SET_NCGRAD_DEFAULT samg_set_ncgrad_default
#define SAMG_GET_NCGRAD_DEFAULT samg_get_ncgrad_default
#define SAMG_SET_NCYC_DEFAULT samg_set_ncyc_default
#define SAMG_GET_NCYC_DEFAULT samg_get_ncyc_default
#define SAMG_SET_NCYC_MIN samg_set_ncyc_min
#define SAMG_GET_NCYC_MIN samg_get_ncyc_min
#define SAMG_SET_NCYC_START samg_set_ncyc_start
#define SAMG_GET_NCYC_START samg_get_ncyc_start
#define SAMG_SET_NEG_DIAG samg_set_neg_diag
#define SAMG_GET_NEG_DIAG samg_get_neg_diag
#define SAMG_SET_NEG_DIAG_BRUTE samg_set_neg_diag_brute
#define SAMG_GET_NEG_DIAG_BRUTE samg_get_neg_diag_brute
#define SAMG_SET_NINT_ROWSUM1 samg_set_nint_rowsum1
#define SAMG_GET_NINT_ROWSUM1 samg_get_nint_rowsum1
#define SAMG_SET_NKDIM_DEFAULT samg_set_nkdim_default
#define SAMG_GET_NKDIM_DEFAULT samg_get_nkdim_default
#define SAMG_SET_NMIN_MATRIX samg_set_nmin_matrix
#define SAMG_GET_NMIN_MATRIX samg_get_nmin_matrix
#define SAMG_SET_NMIN_MATRIX_RESC samg_set_nmin_matrix_resc
#define SAMG_GET_NMIN_MATRIX_RESC samg_get_nmin_matrix_resc
#define SAMG_SET_NMIN_VECTOR samg_set_nmin_vector
#define SAMG_GET_NMIN_VECTOR samg_get_nmin_vector
#define SAMG_SET_NOTALLUNS_CHEAP samg_set_notalluns_cheap
#define SAMG_GET_NOTALLUNS_CHEAP samg_get_notalluns_cheap
#define SAMG_SET_NP_MOD1 samg_set_np_mod1
#define SAMG_GET_NP_MOD1 samg_get_np_mod1
#define SAMG_SET_NP_MOD2 samg_set_np_mod2
#define SAMG_GET_NP_MOD2 samg_get_np_mod2
#define SAMG_SET_NP_OPT samg_set_np_opt
#define SAMG_GET_NP_OPT samg_get_np_opt
#define SAMG_SET_NPRIM_AT_ALLPNTS samg_set_nprim_at_allpnts
#define SAMG_GET_NPRIM_AT_ALLPNTS samg_get_nprim_at_allpnts
#define SAMG_SET_NPTMAX samg_set_nptmax
#define SAMG_GET_NPTMAX samg_get_nptmax
#define SAMG_SET_NPTMN samg_set_nptmn
#define SAMG_GET_NPTMN samg_get_nptmn
#define SAMG_SET_NRC samg_set_nrc
#define SAMG_GET_NRC samg_get_nrc
#define SAMG_SET_NRC_DEFAULT samg_set_nrc_default
#define SAMG_GET_NRC_DEFAULT samg_get_nrc_default
#define SAMG_SET_NRC_EMERGENCY samg_set_nrc_emergency
#define SAMG_GET_NRC_EMERGENCY samg_get_nrc_emergency
#define SAMG_SET_NRD samg_set_nrd
#define SAMG_GET_NRD samg_get_nrd
#define SAMG_SET_NRU samg_set_nru
#define SAMG_GET_NRU samg_get_nru
#define SAMG_SET_NSIMPLE_EMERGENCY samg_set_nsimple_emergency
#define SAMG_GET_NSIMPLE_EMERGENCY samg_get_nsimple_emergency
#define SAMG_SET_NSOLVE_DEFAULT samg_set_nsolve_default
#define SAMG_GET_NSOLVE_DEFAULT samg_get_nsolve_default
#define SAMG_SET_NTAKE_RES_IN samg_set_ntake_res_in
#define SAMG_GET_NTAKE_RES_IN samg_get_ntake_res_in
#define SAMG_SET_NTH_RES_SCRATCH samg_set_nth_res_scratch
#define SAMG_GET_NTH_RES_SCRATCH samg_get_nth_res_scratch
#define SAMG_SET_NTR samg_set_ntr
#define SAMG_GET_NTR samg_get_ntr
#define SAMG_SET_NTR_PRIM samg_set_ntr_prim
#define SAMG_GET_NTR_PRIM samg_get_ntr_prim
#define SAMG_SET_NTYP_ACCEL samg_set_ntyp_accel
#define SAMG_GET_NTYP_ACCEL samg_get_ntyp_accel
#define SAMG_SET_NUMTRY_MAX_SET samg_set_numtry_max_set
#define SAMG_GET_NUMTRY_MAX_SET samg_get_numtry_max_set
#define SAMG_SET_NWT samg_set_nwt
#define SAMG_GET_NWT samg_get_nwt
#define SAMG_SET_OMEGA_JACOBI_DEFAULT samg_set_omega_jacobi_default
#define SAMG_GET_OMEGA_JACOBI_DEFAULT samg_get_omega_jacobi_default
#define SAMG_SET_P_CMPLX_AGG_DEFAULT samg_set_p_cmplx_agg_default
#define SAMG_GET_P_CMPLX_AGG_DEFAULT samg_get_p_cmplx_agg_default
#define SAMG_SET_P_CMPLX_DEFAULT samg_set_p_cmplx_default
#define SAMG_GET_P_CMPLX_DEFAULT samg_get_p_cmplx_default
#define SAMG_CNTRL_SET_PARTIAL_FRAC samg_cntrl_set_partial_frac
#define SAMG_CNTRL_GET_PARTIAL_FRAC samg_cntrl_get_partial_frac
#define SAMG_SET_PRIM_NORM samg_set_prim_norm
#define SAMG_GET_PRIM_NORM samg_get_prim_norm
#define SAMG_SET_PRIM_PRINT samg_set_prim_print
#define SAMG_GET_PRIM_PRINT samg_get_prim_print
#define SAMG_SET_RCONDX samg_set_rcondx
#define SAMG_GET_RCONDX samg_get_rcondx
#define SAMG_CNTRL_SET_REFRESH_CALLED samg_cntrl_set_refresh_called
#define SAMG_CNTRL_GET_REFRESH_CALLED samg_cntrl_get_refresh_called
#define SAMG_CNTRL_SET_RHO_MIN samg_cntrl_set_rho_min
#define SAMG_CNTRL_GET_RHO_MIN samg_cntrl_get_rho_min
#define SAMG_CNTRL_SET_RHO_OK samg_cntrl_set_rho_ok
#define SAMG_CNTRL_GET_RHO_OK samg_cntrl_get_rho_ok
#define SAMG_SET_SHOW_UN_RES samg_set_show_un_res
#define SAMG_GET_SHOW_UN_RES samg_get_show_un_res
#define SAMG_SET_SLOW_COARSENING samg_set_slow_coarsening
#define SAMG_GET_SLOW_COARSENING samg_get_slow_coarsening
#define SAMG_SET_STABILITY samg_set_stability
#define SAMG_GET_STABILITY samg_get_stability
#define SAMG_SET_TERM_COARSENING samg_set_term_coarsening
#define SAMG_GET_TERM_COARSENING samg_get_term_coarsening
#define SAMG_SET_USE_IC samg_set_use_ic
#define SAMG_GET_USE_IC samg_get_use_ic
#define SAMG_SET_VIO_DD samg_set_vio_dd
#define SAMG_GET_VIO_DD samg_get_vio_dd
#define SAMG_SET_W_AVRGE_AGG_DEFAULT samg_set_w_avrge_agg_default
#define SAMG_GET_W_AVRGE_AGG_DEFAULT samg_get_w_avrge_agg_default
#define SAMG_SET_W_AVRGE_DEFAULT samg_set_w_avrge_default
#define SAMG_GET_W_AVRGE_DEFAULT samg_get_w_avrge_default
#endif

#ifdef SAMG_LCASE_USCORE
#define SAMG samg_
#define SAMG_CTIME samg_ctime_
#define SAMG_SIMPLE samg_simple_
#define SAMG_MAIN samg_main_
#define SAMG_LEAVE samg_leave_
#define SAMG_REFRESH samg_refresh_
#define SAMG_CLEANUP samg_cleanup_
#define SAMG_RESET_SECONDARY samg_reset_secondary_
#define SAMG_RESET_HIDDEN samg_reset_hidden_
#define SAMG_GET_LEVELS_CREATED samg_get_levels_created_
#define SAMG_GET_MEM_ACTIVE samg_get_mem_active_
#define SAMG_GET_TIME_AND_MEM samg_get_time_and_mem_
#define SAMG_CVEC_ALLOC samg_cvec_alloc_
#define SAMG_CVEC_DEALLOC samg_cvec_dealloc_
#define SAMG_CVEC_SET samg_cvec_set_
#define SAMG_CVEC_GET samg_cvec_get_
#define SAMG_OMEGA_JACOBI_ALLOC samg_omega_jacobi_alloc_
#define SAMG_OMEGA_JACOBI_DEALLOC samg_omega_jacobi_dealloc_
#define SAMG_OMEGA_JACOBI_SET samg_omega_jacobi_set_
#define SAMG_CHECK_LICENSE samg_check_license_
#define SAMG_CURRENT_RESIDUAL samg_current_residual_
#define SAMG_USER_COO samg_user_coo_
#define SAMG_GET_NTHREAD_MAX samg_get_nthread_max_
#define SAMG_SET_NTHREAD_MAX samg_set_nthread_max_
#define SAMG_GET_NMAX_MATRIX samg_get_nmax_matrix_
#define SAMG_SET_NMAX_MATRIX samg_set_nmax_matrix_
#define SAMG_GET_NMAX_MATRIX_RESC samg_get_nmax_matrix_resc_
#define SAMG_SET_NMAX_MATRIX_RESC samg_set_nmax_matrix_resc_
#define SAMG_GET_NMAX_VECTOR samg_get_nmax_vector_
#define SAMG_SET_NMAX_VECTOR samg_set_nmax_matrix_
#define SAMG_GET_IRESTRICTION_OPENMP samg_get_irestriction_openmp_
#define SAMG_SET_IRESTRICTION_OPENMP samg_set_irestriction_openmp_
#define SAMG_SET_A_CMPLX_AGG_DEFAULT samg_set_a_cmplx_agg_default_
#define SAMG_GET_A_CMPLX_AGG_DEFAULT samg_get_a_cmplx_agg_default_
#define SAMG_SET_A_CMPLX_DEFAULT samg_set_a_cmplx_default_
#define SAMG_GET_A_CMPLX_DEFAULT samg_get_a_cmplx_default_
#define SAMG_SET_ALLOW_ELIM samg_set_allow_elim_
#define SAMG_GET_ALLOW_ELIM samg_get_allow_elim_
#define SAMG_SET_ALLUNS_AT_ALLPNTS samg_set_alluns_at_allpnts_
#define SAMG_GET_ALLUNS_AT_ALLPNTS samg_get_alluns_at_allpnts_
#define SAMG_SET_B_CMPLX samg_set_b_cmplx_
#define SAMG_GET_B_CMPLX samg_get_b_cmplx_
#define SAMG_SET_B_CMPLX_AGG_DEFAULT samg_set_b_cmplx_agg_default_
#define SAMG_GET_B_CMPLX_AGG_DEFAULT samg_get_b_cmplx_agg_default_
#define SAMG_SET_B_CMPLX_DEFAULT samg_set_b_cmplx_default_
#define SAMG_GET_B_CMPLX_DEFAULT samg_get_b_cmplx_default_
#define SAMG_CNTRL_SET_BACKUP samg_cntrl_set_backup_
#define SAMG_CNTRL_GET_BACKUP samg_cntrl_get_backup_
#define SAMG_SET_BLK_FILLEXP samg_set_blk_fillexp_
#define SAMG_GET_BLK_FILLEXP samg_get_blk_fillexp_
#define SAMG_SET_BLK_STAB samg_set_blk_stab_
#define SAMG_GET_BLK_STAB samg_get_blk_stab_
#define SAMG_SET_CHECK_ALLPNTS samg_set_check_allpnts_
#define SAMG_GET_CHECK_ALLPNTS samg_get_check_allpnts_
#define SAMG_SET_CHECK_ORDER samg_set_check_order_
#define SAMG_GET_CHECK_ORDER samg_get_check_order_
#define SAMG_SET_CONV_STOP_DEFAULT samg_set_conv_stop_default_
#define SAMG_GET_CONV_STOP_DEFAULT samg_get_conv_stop_default_
#define SAMG_SET_CSET_LASTPNT samg_set_cset_lastpnt_
#define SAMG_GET_CSET_LASTPNT samg_get_cset_lastpnt_
#define SAMG_SET_CSET_LESSVARS samg_set_cset_lessvars_
#define SAMG_GET_CSET_LESSVARS samg_get_cset_lessvars_
#define SAMG_SET_CSET_LONGROW samg_set_cset_longrow_
#define SAMG_GET_CSET_LONGROW samg_get_cset_longrow_
#define SAMG_ISET_CSET_READ samg_iset_cset_read_
#define SAMG_IGET_CSET_READ samg_iget_cset_read_
#define SAMG_SET_CSET_ZERODIAG samg_set_cset_zerodiag_
#define SAMG_GET_CSET_ZERODIAG samg_get_cset_zerodiag_
#define SAMG_SET_DELTA_MILU samg_set_delta_milu_
#define SAMG_GET_DELTA_MILU samg_get_delta_milu_
#define SAMG_SET_DENSX samg_set_densx_
#define SAMG_GET_DENSX samg_get_densx_
#define SAMG_CNTRL_SET_DIVERGENCE samg_cntrl_set_divergence_
#define SAMG_CNTRL_GET_DIVERGENCE samg_cntrl_get_divergence_
#define SAMG_SET_DROPTOL samg_set_droptol_
#define SAMG_GET_DROPTOL samg_get_droptol_
#define SAMG_SET_DROPTOL_CL samg_set_droptol_cl_
#define SAMG_GET_DROPTOL_CL samg_get_droptol_cl_
#define SAMG_SET_DROPTOL_SMO samg_set_droptol_smo_
#define SAMG_GET_DROPTOL_SMO samg_get_droptol_smo_
#define SAMG_SET_DUMP_CORRECTW samg_set_dump_correctw_
#define SAMG_GET_DUMP_CORRECTW samg_get_dump_correctw_
#define SAMG_SET_ECG samg_set_ecg_
#define SAMG_GET_ECG samg_get_ecg_
#define SAMG_SET_ECG_DEFAULT samg_set_ecg_default_
#define SAMG_GET_ECG_DEFAULT samg_get_ecg_default_
#define SAMG_SET_EPS_ABS samg_set_eps_abs_
#define SAMG_GET_EPS_ABS samg_get_eps_abs_
#define SAMG_SET_EPS_DD samg_set_eps_dd_
#define SAMG_GET_EPS_DD samg_get_eps_dd_
#define SAMG_SET_EPS_DIAG samg_set_eps_diag_
#define SAMG_GET_EPS_DIAG samg_get_eps_diag_
#define SAMG_SET_EPS_LSQ samg_set_eps_lsq_
#define SAMG_GET_EPS_LSQ samg_get_eps_lsq_
#define SAMG_SET_ETR samg_set_etr_
#define SAMG_GET_ETR samg_get_etr_
#define SAMG_SET_ETR_DEFAULT samg_set_etr_default_
#define SAMG_GET_ETR_DEFAULT samg_get_etr_default_
#define SAMG_SET_EWT samg_set_ewt_
#define SAMG_GET_EWT samg_get_ewt_
#define SAMG_SET_EWT_DEFAULT samg_set_ewt_default_
#define SAMG_GET_EWT_DEFAULT samg_get_ewt_default_
#define SAMG_SET_FACTOR_APP_VAR samg_set_factor_app_var_
#define SAMG_GET_FACTOR_APP_VAR samg_get_factor_app_var_
#define SAMG_SET_FACTOR_QUASI_RES samg_set_factor_quasi_res_
#define SAMG_GET_FACTOR_QUASI_RES samg_get_factor_quasi_res_
#define SAMG_SET_FACTOR_RES_VAR samg_set_factor_res_var_
#define SAMG_GET_FACTOR_RES_VAR samg_get_factor_res_var_
#define SAMG_ISET_FILNAM samg_iset_filnam_
#define SAMG_IGET_FILNAM samg_iget_filnam_
#define SAMG_ISET_FILNAM_DUMP samg_iset_filnam_dump_
#define SAMG_IGET_FILNAM_DUMP samg_iget_filnam_dump_
#define SAMG_SET_FULL_PIVOTING samg_set_full_pivoting_
#define SAMG_GET_FULL_PIVOTING samg_get_full_pivoting_
#define SAMG_CNTRL_SET_FULL_SETUP samg_cntrl_set_full_setup_
#define SAMG_CNTRL_GET_FULL_SETUP samg_cntrl_get_full_setup_
#define SAMG_SET_G_CMPLX_AGG_DEFAULT samg_set_g_cmplx_agg_default_
#define SAMG_GET_G_CMPLX_AGG_DEFAULT samg_get_g_cmplx_agg_default_
#define SAMG_SET_G_CMPLX_DEFAULT samg_set_g_cmplx_default_
#define SAMG_GET_G_CMPLX_DEFAULT samg_get_g_cmplx_default_
#define SAMG_SET_GMAX_MULTIPASS samg_set_gmax_multipass_
#define SAMG_GET_GMAX_MULTIPASS samg_get_gmax_multipass_
#define SAMG_SET_IAUTO_STOP samg_set_iauto_stop_
#define SAMG_GET_IAUTO_STOP samg_get_iauto_stop_
#define SAMG_SET_IB_CMPLX samg_set_ib_cmplx_
#define SAMG_GET_IB_CMPLX samg_get_ib_cmplx_
#define SAMG_SET_IB_CMPLX_AGG_DEFAULT samg_set_ib_cmplx_agg_default_
#define SAMG_GET_IB_CMPLX_AGG_DEFAULT samg_get_ib_cmplx_agg_default_
#define SAMG_SET_IB_CMPLX_DEFAULT samg_set_ib_cmplx_default_
#define SAMG_GET_IB_CMPLX_DEFAULT samg_get_ib_cmplx_default_
#define SAMG_SET_IBGS_PIVOT samg_set_ibgs_pivot_
#define SAMG_GET_IBGS_PIVOT samg_get_ibgs_pivot_
#define SAMG_SET_ICRITS samg_set_icrits_
#define SAMG_GET_ICRITS samg_get_icrits_
#define SAMG_SET_ILU_SPEED samg_set_ilu_speed_
#define SAMG_GET_ILU_SPEED samg_get_ilu_speed_
#define SAMG_SET_IODUMP samg_set_iodump_
#define SAMG_GET_IODUMP samg_get_iodump_
#define SAMG_ISET_IOFILE_OPTA samg_iset_iofile_opta_
#define SAMG_IGET_IOFILE_OPTA samg_iget_iofile_opta_
#define SAMG_ISET_IOFORM samg_iset_ioform_
#define SAMG_IGET_IOFORM samg_iget_ioform_
#define SAMG_SET_IOGRID samg_set_iogrid_
#define SAMG_GET_IOGRID samg_get_iogrid_
#define SAMG_SET_IOMOVIE samg_set_iomovie_
#define SAMG_GET_IOMOVIE samg_get_iomovie_
#define SAMG_SET_IOSCRATCH_DEFAULT samg_set_ioscratch_default_
#define SAMG_GET_IOSCRATCH_DEFAULT samg_get_ioscratch_default_
#define SAMG_SET_IOUNIT_OPTA samg_set_iounit_opta_
#define SAMG_GET_IOUNIT_OPTA samg_get_iounit_opta_
#define SAMG_SET_IPASS_MAX_SET samg_set_ipass_max_set_
#define SAMG_GET_IPASS_MAX_SET samg_get_ipass_max_set_
#define SAMG_SET_IRESTRICTION_OPENMP samg_set_irestriction_openmp_
#define SAMG_GET_IRESTRICTION_OPENMP samg_get_irestriction_openmp_
#define SAMG_SET_ISET_VIO_DD samg_set_iset_vio_dd_
#define SAMG_GET_ISET_VIO_DD samg_get_iset_vio_dd_
#define SAMG_SET_ITER_CHECK samg_set_iter_check_
#define SAMG_GET_ITER_CHECK samg_get_iter_check_
#define SAMG_SET_ITER_PRE samg_set_iter_pre_
#define SAMG_GET_ITER_PRE samg_get_iter_pre_
#define SAMG_SET_ITMAX_CONV_DEFAULT samg_set_itmax_conv_default_
#define SAMG_GET_ITMAX_CONV_DEFAULT samg_get_itmax_conv_default_
#define SAMG_SET_LASTGRID samg_set_lastgrid_
#define SAMG_GET_LASTGRID samg_get_lastgrid_
#define SAMG_SET_LEVELX samg_set_levelx_
#define SAMG_GET_LEVELX samg_get_levelx_
#define SAMG_SET_LFIL_CL_DEFAULT samg_set_lfil_cl_default_
#define SAMG_GET_LFIL_CL_DEFAULT samg_get_lfil_cl_default_
#define SAMG_SET_LFIL_SMO samg_set_lfil_smo_
#define SAMG_GET_LFIL_SMO samg_get_lfil_smo_
#define SAMG_ISET_LOGFILE samg_iset_logfile_
#define SAMG_IGET_LOGFILE samg_iget_logfile_
#define SAMG_SET_LOGIO samg_set_logio_
#define SAMG_GET_LOGIO samg_get_logio_
#define SAMG_CNTRL_SET_MAX_CALLS samg_cntrl_set_max_calls_
#define SAMG_CNTRL_GET_MAX_CALLS samg_cntrl_get_max_calls_
#define SAMG_SET_MAX_LEVEL samg_set_max_level_
#define SAMG_GET_MAX_LEVEL samg_get_max_level_
#define SAMG_SET_MAXOP_RESTART samg_set_maxop_restart_
#define SAMG_GET_MAXOP_RESTART samg_get_maxop_restart_
#define SAMG_SET_MILU samg_set_milu_
#define SAMG_GET_MILU samg_get_milu_
#define SAMG_CNTRL_SET_MODE_CNTRL samg_cntrl_set_mode_cntrl_
#define SAMG_CNTRL_GET_MODE_CNTRL samg_cntrl_get_mode_cntrl_
#define SAMG_SET_MODE_DEBUG samg_set_mode_debug_
#define SAMG_GET_MODE_DEBUG samg_get_mode_debug_
#define SAMG_SET_MODE_MESS samg_set_mode_mess_
#define SAMG_GET_MODE_MESS samg_get_mode_mess_
#define SAMG_SET_MODIFY_MAT samg_set_modify_mat_
#define SAMG_GET_MODIFY_MAT samg_get_modify_mat_
#define SAMG_SET_MULTIPASS_ALLCOUP samg_set_multipass_allcoup_
#define SAMG_GET_MULTIPASS_ALLCOUP samg_get_multipass_allcoup_
#define SAMG_SET_NBLK_DEBUG samg_set_nblk_debug_
#define SAMG_GET_NBLK_DEBUG samg_get_nblk_debug_
#define SAMG_SET_NBLK_MAX samg_set_nblk_max_
#define SAMG_GET_NBLK_MAX samg_get_nblk_max_
#define SAMG_SET_NBLK_OVERLAP samg_set_nblk_overlap_
#define SAMG_GET_NBLK_OVERLAP samg_get_nblk_overlap_
#define SAMG_SET_NBLK_RESID samg_set_nblk_resid_
#define SAMG_GET_NBLK_RESID samg_get_nblk_resid_
#define SAMG_SET_NBLK_SOLVE samg_set_nblk_solve_
#define SAMG_GET_NBLK_SOLVE samg_get_nblk_solve_
#define SAMG_SET_NBLK_SOLVER samg_set_nblk_solver_
#define SAMG_GET_NBLK_SOLVER samg_get_nblk_solver_
#define SAMG_SET_NCFRAMES samg_set_ncframes_
#define SAMG_GET_NCFRAMES samg_get_ncframes_
#define SAMG_SET_NCG samg_set_ncg_
#define SAMG_GET_NCG samg_get_ncg_
#define SAMG_SET_NCGRAD_DEFAULT samg_set_ncgrad_default_
#define SAMG_GET_NCGRAD_DEFAULT samg_get_ncgrad_default_
#define SAMG_SET_NCYC_DEFAULT samg_set_ncyc_default_
#define SAMG_GET_NCYC_DEFAULT samg_get_ncyc_default_
#define SAMG_SET_NCYC_MIN samg_set_ncyc_min_
#define SAMG_GET_NCYC_MIN samg_get_ncyc_min_
#define SAMG_SET_NCYC_START samg_set_ncyc_start_
#define SAMG_GET_NCYC_START samg_get_ncyc_start_
#define SAMG_SET_NEG_DIAG samg_set_neg_diag_
#define SAMG_GET_NEG_DIAG samg_get_neg_diag_
#define SAMG_SET_NEG_DIAG_BRUTE samg_set_neg_diag_brute_
#define SAMG_GET_NEG_DIAG_BRUTE samg_get_neg_diag_brute_
#define SAMG_SET_NINT_ROWSUM1 samg_set_nint_rowsum1_
#define SAMG_GET_NINT_ROWSUM1 samg_get_nint_rowsum1_
#define SAMG_SET_NKDIM_DEFAULT samg_set_nkdim_default_
#define SAMG_GET_NKDIM_DEFAULT samg_get_nkdim_default_
#define SAMG_SET_NMIN_MATRIX samg_set_nmin_matrix_
#define SAMG_GET_NMIN_MATRIX samg_get_nmin_matrix_
#define SAMG_SET_NMIN_MATRIX_RESC samg_set_nmin_matrix_resc_
#define SAMG_GET_NMIN_MATRIX_RESC samg_get_nmin_matrix_resc_
#define SAMG_SET_NMIN_VECTOR samg_set_nmin_vector_
#define SAMG_GET_NMIN_VECTOR samg_get_nmin_vector_
#define SAMG_SET_NOTALLUNS_CHEAP samg_set_notalluns_cheap_
#define SAMG_GET_NOTALLUNS_CHEAP samg_get_notalluns_cheap_
#define SAMG_SET_NP_MOD1 samg_set_np_mod1_
#define SAMG_GET_NP_MOD1 samg_get_np_mod1_
#define SAMG_SET_NP_MOD2 samg_set_np_mod2_
#define SAMG_GET_NP_MOD2 samg_get_np_mod2_
#define SAMG_SET_NP_OPT samg_set_np_opt_
#define SAMG_GET_NP_OPT samg_get_np_opt_
#define SAMG_SET_NPRIM_AT_ALLPNTS samg_set_nprim_at_allpnts_
#define SAMG_GET_NPRIM_AT_ALLPNTS samg_get_nprim_at_allpnts_
#define SAMG_SET_NPTMAX samg_set_nptmax_
#define SAMG_GET_NPTMAX samg_get_nptmax_
#define SAMG_SET_NPTMN samg_set_nptmn_
#define SAMG_GET_NPTMN samg_get_nptmn_
#define SAMG_SET_NRC samg_set_nrc_
#define SAMG_GET_NRC samg_get_nrc_
#define SAMG_SET_NRC_DEFAULT samg_set_nrc_default_
#define SAMG_GET_NRC_DEFAULT samg_get_nrc_default_
#define SAMG_SET_NRC_EMERGENCY samg_set_nrc_emergency_
#define SAMG_GET_NRC_EMERGENCY samg_get_nrc_emergency_
#define SAMG_SET_NRD samg_set_nrd_
#define SAMG_GET_NRD samg_get_nrd_
#define SAMG_SET_NRU samg_set_nru_
#define SAMG_GET_NRU samg_get_nru_
#define SAMG_SET_NSIMPLE_EMERGENCY samg_set_nsimple_emergency_
#define SAMG_GET_NSIMPLE_EMERGENCY samg_get_nsimple_emergency_
#define SAMG_SET_NSOLVE_DEFAULT samg_set_nsolve_default_
#define SAMG_GET_NSOLVE_DEFAULT samg_get_nsolve_default_
#define SAMG_SET_NTAKE_RES_IN samg_set_ntake_res_in_
#define SAMG_GET_NTAKE_RES_IN samg_get_ntake_res_in_
#define SAMG_SET_NTH_RES_SCRATCH samg_set_nth_res_scratch_
#define SAMG_GET_NTH_RES_SCRATCH samg_get_nth_res_scratch_
#define SAMG_SET_NTR samg_set_ntr_
#define SAMG_GET_NTR samg_get_ntr_
#define SAMG_SET_NTR_PRIM samg_set_ntr_prim_
#define SAMG_GET_NTR_PRIM samg_get_ntr_prim_
#define SAMG_SET_NTYP_ACCEL samg_set_ntyp_accel_
#define SAMG_GET_NTYP_ACCEL samg_get_ntyp_accel_
#define SAMG_SET_NUMTRY_MAX_SET samg_set_numtry_max_set_
#define SAMG_GET_NUMTRY_MAX_SET samg_get_numtry_max_set_
#define SAMG_SET_NWT samg_set_nwt_
#define SAMG_GET_NWT samg_get_nwt_
#define SAMG_SET_OMEGA_JACOBI_DEFAULT samg_set_omega_jacobi_default_
#define SAMG_GET_OMEGA_JACOBI_DEFAULT samg_get_omega_jacobi_default_
#define SAMG_SET_P_CMPLX_AGG_DEFAULT samg_set_p_cmplx_agg_default_
#define SAMG_GET_P_CMPLX_AGG_DEFAULT samg_get_p_cmplx_agg_default_
#define SAMG_SET_P_CMPLX_DEFAULT samg_set_p_cmplx_default_
#define SAMG_GET_P_CMPLX_DEFAULT samg_get_p_cmplx_default_
#define SAMG_CNTRL_SET_PARTIAL_FRAC samg_cntrl_set_partial_frac_
#define SAMG_CNTRL_GET_PARTIAL_FRAC samg_cntrl_get_partial_frac_
#define SAMG_SET_PRIM_NORM samg_set_prim_norm_
#define SAMG_GET_PRIM_NORM samg_get_prim_norm_
#define SAMG_SET_PRIM_PRINT samg_set_prim_print_
#define SAMG_GET_PRIM_PRINT samg_get_prim_print_
#define SAMG_SET_RCONDX samg_set_rcondx_
#define SAMG_GET_RCONDX samg_get_rcondx_
#define SAMG_CNTRL_SET_REFRESH_CALLED samg_cntrl_set_refresh_called_
#define SAMG_CNTRL_GET_REFRESH_CALLED samg_cntrl_get_refresh_called_
#define SAMG_CNTRL_SET_RHO_MIN samg_cntrl_set_rho_min_
#define SAMG_CNTRL_GET_RHO_MIN samg_cntrl_get_rho_min_
#define SAMG_CNTRL_SET_RHO_OK samg_cntrl_set_rho_ok_
#define SAMG_CNTRL_GET_RHO_OK samg_cntrl_get_rho_ok_
#define SAMG_SET_SHOW_UN_RES samg_set_show_un_res_
#define SAMG_GET_SHOW_UN_RES samg_get_show_un_res_
#define SAMG_SET_SLOW_COARSENING samg_set_slow_coarsening_
#define SAMG_GET_SLOW_COARSENING samg_get_slow_coarsening_
#define SAMG_SET_STABILITY samg_set_stability_
#define SAMG_GET_STABILITY samg_get_stability_
#define SAMG_SET_TERM_COARSENING samg_set_term_coarsening_
#define SAMG_GET_TERM_COARSENING samg_get_term_coarsening_
#define SAMG_SET_USE_IC samg_set_use_ic_
#define SAMG_GET_USE_IC samg_get_use_ic_
#define SAMG_SET_VIO_DD samg_set_vio_dd_
#define SAMG_GET_VIO_DD samg_get_vio_dd_
#define SAMG_SET_W_AVRGE_AGG_DEFAULT samg_set_w_avrge_agg_default_
#define SAMG_GET_W_AVRGE_AGG_DEFAULT samg_get_w_avrge_agg_default_
#define SAMG_SET_W_AVRGE_DEFAULT samg_set_w_avrge_default_
#define SAMG_GET_W_AVRGE_DEFAULT samg_get_w_avrge_default_
#endif

////////////////
/* interfaces */
////////////////

// for resetting secondary / all hidden parameters
// extern "C" {
SAMG_C_CALLCONV SAMG_RESET_SECONDARY(void);
SAMG_C_CALLCONV SAMG_RESET_HIDDEN(void);
// }

// samg's internal timing routine
// extern "C"
SAMG_C_CALLCONV SAMG_CTIME(float * time);

// samg and samg_main routines
// extern "C"
SAMG_C_CALLCONV SAMG(int * nnu, int * nna, int * nsys,
          int * ia, int * ja, double * a, double * f, double * u,
          int * iu, int * ndiu, int * ip, int * ndip,
          int * matrix, int * iscale,
          double * res_in, double * res_out, int * ncyc_done, int * ierr,
          int * nsolve, int * ifirst, double * eps, int * ncyc,
          int * iswtch,
          double * a_cmplx, double * g_cmplx, double * p_cmplx, double * w_avrge,
          double * chktol, int * idump, int * iout);
SAMG_C_CALLCONV SAMG_MAIN(int * nnu, int * nna, int * nsys,
          int * ia, int * ja, double * a, double * f, double * u,
          int * iu, int * ndiu, int * ip, int * ndip,
          int * matrix, int * iscale,
          double * res_in, double * res_out, int * ncyc_done, int * ierr,
          int * nsolve, int * ifirst, double * eps, int * ncyc,
          int * iswtch,
          double * a_cmplx, double * g_cmplx, double * p_cmplx, double * w_avrge,
          double * chktol, int * idump, int * iout);

// samg's simple interface
// extern "C"
SAMG_C_CALLCONV SAMG_SIMPLE(int * iounit, int * nnu, int * nna, int * nsys,
          int * ia, int * ja, double * a, double * f, double * u,
          int * iu, int * ndiu, int * ip, int * ndip, int * matrix, int * iscale,
          double * res_in, double * res_out, int * ncyc_done, int * ierr);

// certain other routines
// extern "C"
// {
SAMG_C_CALLCONV SAMG_LEAVE(int * val);
SAMG_C_CALLCONV SAMG_REFRESH(int * val);
SAMG_C_CALLCONV SAMG_CLEANUP(void);
SAMG_C_CALLCONV SAMG_GET_LEVELS_CREATED(int *ival);
SAMG_C_CALLCONV SAMG_GET_MEM_ACTIVE(int *ival);
SAMG_C_CALLCONV SAMG_GET_TIME_AND_MEM(int *istat, double *valtime, double *valmem);
SAMG_C_CALLCONV SAMG_CVEC_ALLOC(int *val1, int *val2);
SAMG_C_CALLCONV SAMG_CVEC_DEALLOC(int *val);
SAMG_C_CALLCONV SAMG_CVEC_SET(int *val1, int *val2, int *val3);
SAMG_C_CALLCONV SAMG_CVEC_GET(int *val1, int *val2, int *val3);
SAMG_C_CALLCONV SAMG_OMEGA_JACOBI_ALLOC(int * nsys, int * ierr);
SAMG_C_CALLCONV SAMG_OMEGA_JACOBI_SET(int * npos, double * val, int * ierr);
SAMG_C_CALLCONV SAMG_OMEGA_JACOBI_DEALLOC(int * ierr);
SAMG_C_CALLCONV SAMG_CHECK_LICENSE(int *ival);
SAMG_C_CALLCONV SAMG_CURRENT_RESIDUAL(int *ncycle, double *residual);
SAMG_C_CALLCONV SAMG_USER_COO(int *i, int *ndim, double *x, double *y, double *z);
SAMG_C_CALLCONV SAMG_GET_NTHREAD_MAX(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTHREAD_MAX(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMAX_MATRIX(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMAX_MATRIX(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMAX_MATRIX_RESC(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMAX_MATRIX_RESC(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMAX_VECTOR(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMAX_VECTOR(int *ival);
SAMG_C_CALLCONV SAMG_GET_IRESTRICTION_OPENMP(int *ival);
SAMG_C_CALLCONV SAMG_SET_IRESTRICTION_OPENMP(int *ival);
// }

// routines for setting single parameters
// setting character strings might be a problem for some compilers
// the below variant with two arguments works, e.g., for Intel Fortran for Windows 8.x, ...
// for other compilers, add an int *length2 ...
// extern "C"
// {
SAMG_C_CALLCONV SAMG_SET_A_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_A_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_A_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_A_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_ALLOW_ELIM(int *ival);
SAMG_C_CALLCONV SAMG_GET_ALLOW_ELIM(int *ival);
SAMG_C_CALLCONV SAMG_SET_ALLUNS_AT_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_GET_ALLUNS_AT_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_SET_B_CMPLX(double *dval);
SAMG_C_CALLCONV SAMG_GET_B_CMPLX(double *dval);
SAMG_C_CALLCONV SAMG_SET_B_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_B_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_B_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_B_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_SET_BACKUP(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_GET_BACKUP(int *ival);
SAMG_C_CALLCONV SAMG_SET_BLK_FILLEXP(double *dval);
SAMG_C_CALLCONV SAMG_GET_BLK_FILLEXP(double *dval);
SAMG_C_CALLCONV SAMG_SET_BLK_STAB(double *dval);
SAMG_C_CALLCONV SAMG_GET_BLK_STAB(double *dval);
SAMG_C_CALLCONV SAMG_SET_CHECK_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_GET_CHECK_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_SET_CHECK_ORDER(int *ival);
SAMG_C_CALLCONV SAMG_GET_CHECK_ORDER(int *ival);
SAMG_C_CALLCONV SAMG_SET_CONV_STOP_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_CONV_STOP_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_CSET_LASTPNT(int *ival);
SAMG_C_CALLCONV SAMG_GET_CSET_LASTPNT(int *ival);
SAMG_C_CALLCONV SAMG_SET_CSET_LESSVARS(int *ival);
SAMG_C_CALLCONV SAMG_GET_CSET_LESSVARS(int *ival);
SAMG_C_CALLCONV SAMG_SET_CSET_LONGROW(int *ival);
SAMG_C_CALLCONV SAMG_GET_CSET_LONGROW(int *ival);
SAMG_C_CALLCONV SAMG_ISET_CSET_READ(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_CSET_READ(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_SET_CSET_ZERODIAG(int *ival);
SAMG_C_CALLCONV SAMG_GET_CSET_ZERODIAG(int *ival);
SAMG_C_CALLCONV SAMG_SET_DELTA_MILU(double *dval);
SAMG_C_CALLCONV SAMG_GET_DELTA_MILU(double *dval);
SAMG_C_CALLCONV SAMG_SET_DENSX(double *dval);
SAMG_C_CALLCONV SAMG_GET_DENSX(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_SET_DIVERGENCE(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_GET_DIVERGENCE(double *dval);
SAMG_C_CALLCONV SAMG_SET_DROPTOL(double *dval);
SAMG_C_CALLCONV SAMG_GET_DROPTOL(double *dval);
SAMG_C_CALLCONV SAMG_SET_DROPTOL_CL(double *dval);
SAMG_C_CALLCONV SAMG_GET_DROPTOL_CL(double *dval);
SAMG_C_CALLCONV SAMG_SET_DROPTOL_SMO(double *dval);
SAMG_C_CALLCONV SAMG_GET_DROPTOL_SMO(double *dval);
SAMG_C_CALLCONV SAMG_SET_DUMP_CORRECTW(int *ival);
SAMG_C_CALLCONV SAMG_GET_DUMP_CORRECTW(int *ival);
SAMG_C_CALLCONV SAMG_SET_ECG(double *dval);
SAMG_C_CALLCONV SAMG_GET_ECG(double *dval);
SAMG_C_CALLCONV SAMG_SET_ECG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_ECG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_EPS_ABS(double *dval);
SAMG_C_CALLCONV SAMG_GET_EPS_ABS(double *dval);
SAMG_C_CALLCONV SAMG_SET_EPS_DD(double *dval);
SAMG_C_CALLCONV SAMG_GET_EPS_DD(double *dval);
SAMG_C_CALLCONV SAMG_SET_EPS_DIAG(double *dval);
SAMG_C_CALLCONV SAMG_GET_EPS_DIAG(double *dval);
SAMG_C_CALLCONV SAMG_SET_EPS_LSQ(double *dval);
SAMG_C_CALLCONV SAMG_GET_EPS_LSQ(double *dval);
SAMG_C_CALLCONV SAMG_SET_ETR(double *dval);
SAMG_C_CALLCONV SAMG_GET_ETR(double *dval);
SAMG_C_CALLCONV SAMG_SET_ETR_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_ETR_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_EWT(double *dval);
SAMG_C_CALLCONV SAMG_GET_EWT(double *dval);
SAMG_C_CALLCONV SAMG_SET_EWT_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_EWT_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_FACTOR_APP_VAR(double *dval);
SAMG_C_CALLCONV SAMG_GET_FACTOR_APP_VAR(double *dval);
SAMG_C_CALLCONV SAMG_SET_FACTOR_QUASI_RES(double *dval);
SAMG_C_CALLCONV SAMG_GET_FACTOR_QUASI_RES(double *dval);
SAMG_C_CALLCONV SAMG_SET_FACTOR_RES_VAR(double *dval);
SAMG_C_CALLCONV SAMG_GET_FACTOR_RES_VAR(double *dval);
SAMG_C_CALLCONV SAMG_ISET_FILNAM(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_FILNAM(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_ISET_FILNAM_DUMP(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_FILNAM_DUMP(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_SET_FULL_PIVOTING(int *ival);
SAMG_C_CALLCONV SAMG_GET_FULL_PIVOTING(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_SET_FULL_SETUP(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_GET_FULL_SETUP(double *dval);
SAMG_C_CALLCONV SAMG_SET_G_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_G_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_G_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_G_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_GMAX_MULTIPASS(double *dval);
SAMG_C_CALLCONV SAMG_GET_GMAX_MULTIPASS(double *dval);
SAMG_C_CALLCONV SAMG_SET_IAUTO_STOP(int *ival);
SAMG_C_CALLCONV SAMG_GET_IAUTO_STOP(int *ival);
SAMG_C_CALLCONV SAMG_SET_IB_CMPLX(double *dval);
SAMG_C_CALLCONV SAMG_GET_IB_CMPLX(double *dval);
SAMG_C_CALLCONV SAMG_SET_IB_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_IB_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_IB_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_IB_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_IBGS_PIVOT(int *ival);
SAMG_C_CALLCONV SAMG_GET_IBGS_PIVOT(int *ival);
SAMG_C_CALLCONV SAMG_SET_ICRITS(int *ival);
SAMG_C_CALLCONV SAMG_GET_ICRITS(int *ival);
SAMG_C_CALLCONV SAMG_SET_ILU_SPEED(int *ival);
SAMG_C_CALLCONV SAMG_GET_ILU_SPEED(int *ival);
SAMG_C_CALLCONV SAMG_SET_IODUMP(int *ival);
SAMG_C_CALLCONV SAMG_GET_IODUMP(int *ival);
SAMG_C_CALLCONV SAMG_ISET_IOFILE_OPTA(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_IOFILE_OPTA(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_ISET_IOFORM(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_IOFORM(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_SET_IOGRID(int *ival);
SAMG_C_CALLCONV SAMG_GET_IOGRID(int *ival);
SAMG_C_CALLCONV SAMG_SET_IOMOVIE(int *ival);
SAMG_C_CALLCONV SAMG_GET_IOMOVIE(int *ival);
SAMG_C_CALLCONV SAMG_SET_IOSCRATCH_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_IOSCRATCH_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_IOUNIT_OPTA(int *ival);
SAMG_C_CALLCONV SAMG_GET_IOUNIT_OPTA(int *ival);
SAMG_C_CALLCONV SAMG_SET_IPASS_MAX_SET(int *ival);
SAMG_C_CALLCONV SAMG_GET_IPASS_MAX_SET(int *ival);
SAMG_C_CALLCONV SAMG_SET_IRESTRICTION_OPENMP(int *ival);
SAMG_C_CALLCONV SAMG_GET_IRESTRICTION_OPENMP(int *ival);
SAMG_C_CALLCONV SAMG_SET_ISET_VIO_DD(int *ival);
SAMG_C_CALLCONV SAMG_GET_ISET_VIO_DD(int *ival);
SAMG_C_CALLCONV SAMG_SET_ITER_CHECK(int *ival);
SAMG_C_CALLCONV SAMG_GET_ITER_CHECK(int *ival);
SAMG_C_CALLCONV SAMG_SET_ITER_PRE(int *ival);
SAMG_C_CALLCONV SAMG_GET_ITER_PRE(int *ival);
SAMG_C_CALLCONV SAMG_SET_ITMAX_CONV_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_ITMAX_CONV_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_LASTGRID(int *ival);
SAMG_C_CALLCONV SAMG_GET_LASTGRID(int *ival);
SAMG_C_CALLCONV SAMG_SET_LEVELX(int *ival);
SAMG_C_CALLCONV SAMG_GET_LEVELX(int *ival);
SAMG_C_CALLCONV SAMG_SET_LFIL_CL_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_LFIL_CL_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_LFIL_SMO(int *ival);
SAMG_C_CALLCONV SAMG_GET_LFIL_SMO(int *ival);
SAMG_C_CALLCONV SAMG_ISET_LOGFILE(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_IGET_LOGFILE(int *istring, int *length1);
SAMG_C_CALLCONV SAMG_SET_LOGIO(int *ival);
SAMG_C_CALLCONV SAMG_GET_LOGIO(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_SET_MAX_CALLS(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_GET_MAX_CALLS(int *ival);
SAMG_C_CALLCONV SAMG_SET_MAX_LEVEL(int *ival);
SAMG_C_CALLCONV SAMG_GET_MAX_LEVEL(int *ival);
SAMG_C_CALLCONV SAMG_SET_MAXOP_RESTART(int *ival);
SAMG_C_CALLCONV SAMG_GET_MAXOP_RESTART(int *ival);
SAMG_C_CALLCONV SAMG_SET_MILU(int *ival);
SAMG_C_CALLCONV SAMG_GET_MILU(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_SET_MODE_CNTRL(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_GET_MODE_CNTRL(int *ival);
SAMG_C_CALLCONV SAMG_SET_MODE_DEBUG(int *ival);
SAMG_C_CALLCONV SAMG_GET_MODE_DEBUG(int *ival);
SAMG_C_CALLCONV SAMG_SET_MODE_MESS(int *ival);
SAMG_C_CALLCONV SAMG_GET_MODE_MESS(int *ival);
SAMG_C_CALLCONV SAMG_SET_MODIFY_MAT(int *ival);
SAMG_C_CALLCONV SAMG_GET_MODIFY_MAT(int *ival);
SAMG_C_CALLCONV SAMG_SET_MULTIPASS_ALLCOUP(int *ival);
SAMG_C_CALLCONV SAMG_GET_MULTIPASS_ALLCOUP(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_DEBUG(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_DEBUG(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_MAX(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_MAX(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_OVERLAP(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_OVERLAP(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_RESID(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_RESID(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_SOLVE(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_SOLVE(int *ival);
SAMG_C_CALLCONV SAMG_SET_NBLK_SOLVER(int *ival);
SAMG_C_CALLCONV SAMG_GET_NBLK_SOLVER(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCFRAMES(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCFRAMES(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCG(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCG(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCGRAD_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCGRAD_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCYC_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCYC_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCYC_MIN(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCYC_MIN(int *ival);
SAMG_C_CALLCONV SAMG_SET_NCYC_START(int *ival);
SAMG_C_CALLCONV SAMG_GET_NCYC_START(int *ival);
SAMG_C_CALLCONV SAMG_SET_NEG_DIAG(int *ival);
SAMG_C_CALLCONV SAMG_GET_NEG_DIAG(int *ival);
SAMG_C_CALLCONV SAMG_SET_NEG_DIAG_BRUTE(int *ival);
SAMG_C_CALLCONV SAMG_GET_NEG_DIAG_BRUTE(int *ival);
SAMG_C_CALLCONV SAMG_SET_NINT_ROWSUM1(int *ival);
SAMG_C_CALLCONV SAMG_GET_NINT_ROWSUM1(int *ival);
SAMG_C_CALLCONV SAMG_SET_NKDIM_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NKDIM_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMIN_MATRIX(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMIN_MATRIX(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMIN_MATRIX_RESC(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMIN_MATRIX_RESC(int *ival);
SAMG_C_CALLCONV SAMG_SET_NMIN_VECTOR(int *ival);
SAMG_C_CALLCONV SAMG_GET_NMIN_VECTOR(int *ival);
SAMG_C_CALLCONV SAMG_SET_NOTALLUNS_CHEAP(int *ival);
SAMG_C_CALLCONV SAMG_GET_NOTALLUNS_CHEAP(int *ival);
SAMG_C_CALLCONV SAMG_SET_NP_MOD1(int *ival);
SAMG_C_CALLCONV SAMG_GET_NP_MOD1(int *ival);
SAMG_C_CALLCONV SAMG_SET_NP_MOD2(int *ival);
SAMG_C_CALLCONV SAMG_GET_NP_MOD2(int *ival);
SAMG_C_CALLCONV SAMG_SET_NP_OPT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NP_OPT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NPRIM_AT_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_GET_NPRIM_AT_ALLPNTS(int *ival);
SAMG_C_CALLCONV SAMG_SET_NPTMAX(int *ival);
SAMG_C_CALLCONV SAMG_GET_NPTMAX(int *ival);
SAMG_C_CALLCONV SAMG_SET_NPTMN(int *ival);
SAMG_C_CALLCONV SAMG_GET_NPTMN(int *ival);
SAMG_C_CALLCONV SAMG_SET_NRC(int *ival);
SAMG_C_CALLCONV SAMG_GET_NRC(int *ival);
SAMG_C_CALLCONV SAMG_SET_NRC_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NRC_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NRC_EMERGENCY(int *ival);
SAMG_C_CALLCONV SAMG_GET_NRC_EMERGENCY(int *ival);
SAMG_C_CALLCONV SAMG_SET_NRD(int *ival);
SAMG_C_CALLCONV SAMG_GET_NRD(int *ival);
SAMG_C_CALLCONV SAMG_SET_NRU(int *ival);
SAMG_C_CALLCONV SAMG_GET_NRU(int *ival);
SAMG_C_CALLCONV SAMG_SET_NSIMPLE_EMERGENCY(int *ival);
SAMG_C_CALLCONV SAMG_GET_NSIMPLE_EMERGENCY(int *ival);
SAMG_C_CALLCONV SAMG_SET_NSOLVE_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NSOLVE_DEFAULT(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTAKE_RES_IN(int *ival);
SAMG_C_CALLCONV SAMG_GET_NTAKE_RES_IN(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTH_RES_SCRATCH(int *ival);
SAMG_C_CALLCONV SAMG_GET_NTH_RES_SCRATCH(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTR(int *ival);
SAMG_C_CALLCONV SAMG_GET_NTR(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTR_PRIM(int *ival);
SAMG_C_CALLCONV SAMG_GET_NTR_PRIM(int *ival);
SAMG_C_CALLCONV SAMG_SET_NTYP_ACCEL(int *ival);
SAMG_C_CALLCONV SAMG_GET_NTYP_ACCEL(int *ival);
SAMG_C_CALLCONV SAMG_SET_NUMTRY_MAX_SET(int *ival);
SAMG_C_CALLCONV SAMG_GET_NUMTRY_MAX_SET(int *ival);
SAMG_C_CALLCONV SAMG_SET_NWT(int *ival);
SAMG_C_CALLCONV SAMG_GET_NWT(int *ival);
SAMG_C_CALLCONV SAMG_SET_OMEGA_JACOBI_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_OMEGA_JACOBI_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_P_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_P_CMPLX_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_P_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_P_CMPLX_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_SET_PARTIAL_FRAC(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_GET_PARTIAL_FRAC(double *dval);
SAMG_C_CALLCONV SAMG_SET_PRIM_NORM(int *ival);
SAMG_C_CALLCONV SAMG_GET_PRIM_NORM(int *ival);
SAMG_C_CALLCONV SAMG_SET_PRIM_PRINT(int *ival);
SAMG_C_CALLCONV SAMG_GET_PRIM_PRINT(int *ival);
SAMG_C_CALLCONV SAMG_SET_RCONDX(double *dval);
SAMG_C_CALLCONV SAMG_GET_RCONDX(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_SET_REFRESH_CALLED(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_GET_REFRESH_CALLED(int *ival);
SAMG_C_CALLCONV SAMG_CNTRL_SET_RHO_MIN(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_GET_RHO_MIN(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_SET_RHO_OK(double *dval);
SAMG_C_CALLCONV SAMG_CNTRL_GET_RHO_OK(double *dval);
SAMG_C_CALLCONV SAMG_SET_SHOW_UN_RES(int *ival);
SAMG_C_CALLCONV SAMG_GET_SHOW_UN_RES(int *ival);
SAMG_C_CALLCONV SAMG_SET_SLOW_COARSENING(double *dval);
SAMG_C_CALLCONV SAMG_GET_SLOW_COARSENING(double *dval);
SAMG_C_CALLCONV SAMG_SET_STABILITY(double *dval);
SAMG_C_CALLCONV SAMG_GET_STABILITY(double *dval);
SAMG_C_CALLCONV SAMG_SET_TERM_COARSENING(double *dval);
SAMG_C_CALLCONV SAMG_GET_TERM_COARSENING(double *dval);
SAMG_C_CALLCONV SAMG_SET_USE_IC(int *ival);
SAMG_C_CALLCONV SAMG_GET_USE_IC(int *ival);
SAMG_C_CALLCONV SAMG_SET_VIO_DD(int *ival);
SAMG_C_CALLCONV SAMG_GET_VIO_DD(int *ival);
SAMG_C_CALLCONV SAMG_SET_W_AVRGE_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_W_AVRGE_AGG_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_SET_W_AVRGE_DEFAULT(double *dval);
SAMG_C_CALLCONV SAMG_GET_W_AVRGE_DEFAULT(double *dval);
// }

