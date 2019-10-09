// -----------------------------------------------------------------
// file: visualc.h
//
// visualization output in the Tecplot format for IPARS framework
// header file for vis files
//
// MPeszynska, 3/19/98
// moved from visual.dc (mpesz) 11/20/98,
// see CVS log files for description of updates
//
// RMartino, 3/2/02
// changed to include routines for binary output
//   and moved shared typedefs, function declarations
//
// XGAI,  08/10/03
// Make changes for corner point variables
// Modify struct vis_inform{} to add a flag indicating corner point variables
// Add in _vis_vnodal_set() for corner point variables
//
// Gergina Pencheva 10/14/15   Added flag for binary vs ascii vis vtk output
//-----------------------------------------------------------------

// gp
#include <stdlib.h>
#include <string.h>

// ------------------------------ definitions and declarations
//
//  INTERFACE variables & functions from this module
//                to be called from IPARS Fortran routines
//
#define _vis_tecoutput   _F_NAME_(VIS_TECOUTPUT,vis_tecoutput)
#define _vis_vtkoutput   _F_NAME_(VIS_VTKOUTPUT,vis_vtkoutput)
#define _vis_vtkoutput_mpfa   _F_NAME_(VIS_VTKOUTPUT_MPFA,vis_vtkoutput_mpfa)
#define _vis_off_set     _F_NAME_(VIS_OFF_SET,vis_off_set)
#define _vis_vnodal_set  _F_NAME_(VIS_VNODAL_SET,vis_vnodal_set)
#define _vis_name_set    _F_NAME_(VIS_NAME_SET,vis_name_set)
#define _vis_fname_set   _F_NAME_(VIS_FNAME_SET,vis_fname_set)
#define _vis_info_set    _F_NAME_(VIS_INFO_SET,vis_info_set)
#define _vis_tstep_set   _F_NAME_(VIS_TSTEP_SET,vis_tstep_set)
#define _vis_pvtu_set    _F_NAME_(VIS_PVTU_SET,vis_pvtu_set)
#define _vis_pvtu_quit   _F_NAME_(VIS_PVTU_QUIT,vis_pvtu_quit)
#define _visual_init     _F_NAME_(VISUAL_INIT,visual_init)
#define _vis_setvec_nam  _F_NAME_(VIS_SETVEC_NAM,vis_setvec_nam)

//
// IPARS function in C to be called from this file
//
#define myblkoff    _F_NAME_(MYBLKOFF,myblkoff)
_F_EXTERN_(void) myblkoff(const _F_INTEGER*, _F_INTEGER*,
                          _F_INTEGER*, _F_INTEGER*, _F_INTEGER*);

#define _ismyblk      _F_NAME_(IS_MYBLK,is_myblk)
_F_EXTERN_(_F_INTEGER) _ismyblk(_F_INTEGER *numblk);

#define _ismyslice      _F_NAME_(IS_MYSLICE,is_myslice)
_F_EXTERN_(_F_INTEGER) _ismyslice(_F_INTEGER *numslice);

//------------------------------  private variables and functions
// constants : preprocessed by IPARS preprocessor

#define MAXVISVARS 	30
#define MAXSTRLEN 	50
#define MAXFNAMELEN	50
#define MAXBLK   	10
#define MAXSLI          3	

// global variable that counts how many times we called our vis routine
//
extern int _Vis_cnt[MAXBLK];  // counts if init file for the block exists
extern char* _Vis_vecnam[MAXVISVARS];  // vector names in input file
extern int _Vis_flag; // C-global copy of the visual flag
extern int _Vis_init; // counts if some initialization has been made
extern int _Vis_newtimestep;

//
//_Vis_pars keeps the offsets and names of the visualization variables,
// it is set by vis_off_set and vis_name_set
//
typedef struct vis_inform {int ldim; char *name; int vnodal;} VIS_INFO ;
extern VIS_INFO _Vis_pars[MAXVISVARS];

//
// The variables below are auxiliary: help in creating filenames etc.
//
extern int _Ngblk[MAXBLK]; // global cnter of # of gridbcks for the block
extern int _Myprc; // processor number, set by vis_fname_set
extern int _Filestyle; // style of the file name, see vis_fname_set
extern char ROOTname [MAXFNAMELEN] ; // root name for the vis output files
                                     // set by vis_fname_set
                                     // or by visualization_init (default)
extern char INFOname [MAXFNAMELEN] ; // root name for the info vis file
                                     // set by vis_fname_set
                                     // or by visualization_init (default)
// bag8, gp
extern char DIRname [MAXFNAMELEN] ;  // directory name for vis

extern _F_INTEGER _Nstep; // time step setup externally from Fortran
extern _F_REAL_8   _Time; // time step value set externally from Fortran

extern _F_INTEGER _Mxrecxp,_Myrecyp,_Mzreczp,_Mblk,_Nprocs;

// function declarations shared by visual3.dc and visual7.dc

static _F_REAL_8 sint3Dnonuniform_with_ghosts(
                 int c111, int c211, int c121, int c221,
                 int c112, int c212, int c122, int c222,
                 int k111, int k211, int k121, int k221,
                 int k112, int k212, int k122, int k222,
                 _F_REAL_8 p111,_F_REAL_8 p211,_F_REAL_8 p121,_F_REAL_8 p221,
                 _F_REAL_8 p112,_F_REAL_8 p212,_F_REAL_8 p122,_F_REAL_8 p222,
                 _F_REAL_8 dx1,_F_REAL_8 dx2,
                 _F_REAL_8 dy1,_F_REAL_8 dy2,
                 _F_REAL_8 dz1,_F_REAL_8 dz2,
                 int *flag);

static _F_REAL_8 vint3D_nonuniform(
                 int s11,int s21,int s12,int s22,
                 int k111, int k211, int k121, int k221, int k112,
                 int k212,int k122, int k222,
                 _F_REAL_8 v11,_F_REAL_8 v21,_F_REAL_8 v12,_F_REAL_8 v22,
                 _F_REAL_8 dy1,_F_REAL_8 dy2,
                 _F_REAL_8 dz1,_F_REAL_8 dz2,
                 int *flag);

static void int1D_edge(int c1, int c2,
		       _F_REAL_8 p1, _F_REAL_8 p2, _F_REAL_8 d1,_F_REAL_8 d2,
		       int *count, _F_REAL_8 *val);

// sgt  ---------------------------------------------------------
static void _update_Visinf();
static int _Ifnew=1;
static void trimblanks( char *source );

//---------------------------------------------------------------
inline static void _update_Visinf()
{
  if(_Myprc == 0)
    if (_Vis_newtimestep==1) {

      char VISINFname[30];
      sprintf(VISINFname,"%sVis.inf",DIRname);
      FILE *fp = fopen(VISINFname,"a");
      if(fp == NULL) {
	fprintf(stderr,"Error opening main file Vis.inf");
        exit(-1);
      }

      _Vis_newtimestep =0;

      fprintf(fp,"Nstep    = %d\n",_Nstep);
      fprintf(fp,"Time     = %g\n",_Time);
      fclose(fp);
    }
}

// ---------------------------------------------------------------
inline static void trimblanks( char *source )
{
char *s;
s = strchr(source,' ');
if (s != NULL) *s='\0';
}

inline static _F_REAL_8 sint3Dnonuniform_with_ghosts(			
		 int c111, int c211, int c121, int c221,
		 int c112, int c212, int c122, int c222,

		 int k111, int k211, int k121, int k221,
		 int k112, int k212, int k122, int k222,

		 _F_REAL_8 p111,_F_REAL_8 p211,_F_REAL_8 p121,_F_REAL_8 p221,
		 _F_REAL_8 p112,_F_REAL_8 p212,_F_REAL_8 p122,_F_REAL_8 p222,

		 _F_REAL_8 dx1,_F_REAL_8 dx2,
		 _F_REAL_8 dy1,_F_REAL_8 dy2,
		 _F_REAL_8 dz1,_F_REAL_8 dz2,

		 int *flag
		 )
{
  const int kk111=abs(k111), kk211=abs(k211), kk121=abs(k121),
    kk221=abs(k221), kk112=abs(k112), kk212=abs(k212), kk122=abs(k122),
    kk222=abs(k222);

  int c11,c12,c21,c22,c1,c2,count;
  _F_REAL_8 v11,v21,v12,v22,v1,v2,val=0.0;

  // for each gridblock check if it belongs to the processor subdomain
  // then the value is well defined, otherwise do not do anything
  //
  // if it is a ghost cell (possibly a corner)
  // take the gridblock value into account

  // --------------- interpolate x values first
  // FRONT LOWER
  int1D_edge(c111*kk111,c211*kk211,p111,p211,dx1,dx2,
	     &c11, &v11);

  // FRONT UPPER
  int1D_edge(c121*kk121,c221*kk221,p121,p221,dx1,dx2,
	     &c21, &v21);

  // BACK LOWER
  int1D_edge(c112*kk112,c212*kk212,p112,p212,dx1,dx2,
	     &c12, &v12);

  // BACK UPPER
  int1D_edge(c122*kk122,c222*kk222,p122,p222,dx1,dx2,
	     &c22, &v22);

  if(c11+c12+c21+c22==0)  {
    *flag = 0;
    return 0.0;
    printf("\n ISOLATED NODE on edge in sint3Dnonuniform_with_ghosts.\n");
  }
  // ------------------------ interpolate y values next

  int1D_edge(c11,c21,v11,v21,dy1,dy2,&c1,&v1);
  int1D_edge(c12,c22,v12,v22,dy1,dy2,&c2,&v2);

  if(c1+c2==0)  {
    *flag = -1;
    return 0.0;
    printf("\n ISOLATED NODE on face in sint3Dnonuniform_with_ghosts.\n");
  }

  int1D_edge(c1,c2,v1,v2,dz1,dz2,&count,&val);

  *flag=count;
  return val;

}

inline _F_REAL_8 vint3D_nonuniform(
		 int s11,int s21,int s12,int s22,
		 int k111, int k211, int k121, int k221, int k112,
		 int k212,int k122, int k222,
		 _F_REAL_8 v11,_F_REAL_8 v21,_F_REAL_8 v12,_F_REAL_8 v22,

		 _F_REAL_8 dy1,_F_REAL_8 dy2,
		 _F_REAL_8 dz1,_F_REAL_8 dz2,

		 int *flag
		 )
{
  _F_REAL_8 val=0.0,v1,v2; int k11,k21,k12,k22;
  int count=0,c1,c2;

  // === none of the dy, dz re allowed to be zero:
  if (
      ( fabs(dy1) + fabs(dy2) < 1e-8 ) ||
      ( fabs(dz1) + fabs(dz2) < 1e-8 )
      )
    {
      fprintf(stderr,
             "\n Vint 3D returns because of zero values of %g %g %g %g\n",
             dy1,dy2,dz1,dz2);
      return 0.0        ;
  }


  //==== The comments below are for the x-face interpretation
  // for velocities it is enough for a face to be included that at least one
  // of the gridblocks forming that face is in the processor subdomain
  // the others can be ghost cells or outer bdary faces


  if((k111==1) || (k211==1) ) k11 = 1;else k11 = 0;
  if((k121==1) || (k221==1) ) k21 = 1;else k21 = 0;
  if((k112==1) || (k212==1) ) k12 = 1;else k12 = 0;
  if((k122==1) || (k222==1) ) k22 = 1;else k22 = 0;

  // FRONT LOWER: 11
  // check lower left face=lower left front corner and
  // lower right front corner
  // ... and
  // FRONT UPPER: 21
  // check upper left face=upper left front corner and upper right front corner

  int1D_edge(k11*s11,k21*s21,v11,v21,dy1,dy2,&c1,&v1);

  // BACK LOWER: 12
  // check lower right face: lower left back corner and lower right back corner
  /// ... and
  // BACK UPPER: 22
  // check upper right face: upper left back corner and lower left back corner

  int1D_edge(k12*s12,k22*s22,v12,v22,dy1,dy2,&c2,&v2);

  // ------------------------

  if(c1+c2==0)  {
    return 0.0;
    printf("\n ISOLATED NODE on face in vint3Dnonuniform_with_ghosts.\n");
  }

  int1D_edge(c1,c2,v1,v2,dz1,dz2,&count,&val);

  *flag=count;
  return val;

}

inline static _F_REAL_8 sint3D_with_ghosts(			
		 int c111, int c211, int c121, int c221,
		 int c112, int c212, int c122, int c222,

		 int k111, int k211, int k121, int k221,
		 int k112, int k212, int k122, int k222,
		 _F_REAL_8 p111,_F_REAL_8 p211,_F_REAL_8 p121,_F_REAL_8 p221,
		 _F_REAL_8 p112,_F_REAL_8 p212,_F_REAL_8 p122,_F_REAL_8 p222,
		 int *flag
		 )
{
  _F_REAL_8 val=0.0;
  int count=0;

  const int kk111=abs(k111), kk211=abs(k211), kk121=abs(k121),
    kk221=abs(k221), kk112=abs(k112), kk212=abs(k212), kk122=abs(k122),
    kk222=abs(k222);
	
  // for each gridblock check if it belongs to the processor subdomain
  // then the value is well defined, otherwise do not do anything
  //
  // if it is a ghost cell (possibly a corner)
  // take the gridblock value into account

  // FRONT
  // check lower left front corner
  if( (c111*kk111==1) ) {
    val+=p111;count++;
  }

  // check lower right front corner
  if( (c211*kk211==1) ){
    val+=p211;count++;
  }

  // check upper left front corner
  if( (c121*kk121==1) ){
    val+=p121;count++;
  }

  // check upper right front corner
  if( (c221*kk221==1) ){
    val+=p221;count++;
  }

  // BACK
  // checkk lower left back corner
  if( (c112*kk112==1) ){
    val+=p112;count++;
  }

  // check lower right back corner
  if( (c212*kk212==1) ){
    val+=p212;count++;
  }

  // check upper left back corner
  if( (c122*kk122==1)){
    val+=p122;count++;
  }

  // check upper right back corner
  if( (c222*kk222==1) ){
    val+=p222;count++;
  }

  if(count==0)  {
    return 0.0;
    printf("\n ISOLATED NODE in sint3D_with_ghosts.\n");
  } else {
    *flag=count;
    return val/ ( (_F_REAL_8) count);
  }

}

inline static void int1D_edge(int c1, int c2,
     _F_REAL_8 p1, _F_REAL_8 p2, _F_REAL_8 d1,_F_REAL_8 d2,
      int *count, _F_REAL_8 *val)
{
  if (d1 + d2 < 1e-8) {
    fprintf(stderr," VIS. int1D_edge error: d1+d2 close to zero.");
    *val =0.0;*count = -1;
    return;
  }

  *count=c1+c2;
  if( (c1>0) && (c2>0) )
    *val = (p1*d2 +p2*d1) / (d1+ d2);
  else if (c1>0) *val = p1;
  else if (c2>0) *val = p2;
  else *val=0.0;
}

// -------------------------------------------------------------------x

// ----------------------------------------------------------------



// ----------------------------------------------------------------

inline _F_REAL_8 vint3D(
		 int s11,int s21,int s12,int s22,
		 int k111, int k211, int k121, int k221, int k112,
		 int k212,int k122, int k222,
		 _F_REAL_8 v11,_F_REAL_8 v21,_F_REAL_8 v12,_F_REAL_8 v22,
		 int flag
		 )
{
  _F_REAL_8 val=0.0;
  int count=0;

  //==== The comments below are for the x-face interpretation
  // for velocities it is enough for a face to be included that at least one
  // of the gridblocks forming that face is in the processor subdomain
  // the others can be ghost cells or outer bdary faces

  // FRONT LOWER: 11
  // check lower left face=lower left front corner and
  // lower right front corner

  if(s11==1)
  if((k111==1) || (k211==1) ){
    val+=v11;    count+=1;
  }

  // FRONT UPPER: 21
  // check upper left face=upper left front corner and upper right front corner

  if(s21==1)
  if((k121==1) || (k221==1) ){
    val+=v21;    count+=1;
      }

  // BACK LOWER: 12
  // check lower right face: lower left back corner and lower right back corner

  if(s12==1)
  if((k112==1) || (k212==1) ){
    val+=v12;      count+=1;
  }

  // BACK UPPER: 22
  // check upper right face: upper left back corner and lower left back corner

  if(s22==1)
  if((k122==1) ||  (k222==1) ){
    val+=v22;count+=1;
  }

  if(count==0)  {
    printf("\n ISOLATED NODE in vint3D.\n");
    return 0.0;
  } else {
    return val/ ( (_F_REAL_8) count);
  }

}

// -------------------------------------------------------------------
inline static _F_REAL_8 sint3D_without_ghosts(
		 int k111, int k211, int k121, int k221,
		 int k112, int k212, int k122, int k222,
		 _F_REAL_8 p111,_F_REAL_8 p211,_F_REAL_8 p121,_F_REAL_8 p221,
		 _F_REAL_8 p112,_F_REAL_8 p212,_F_REAL_8 p122,_F_REAL_8 p222,
		 int *flag
		 )
{
  _F_REAL_8 val=0.0;
  int count=0;

  // for each gridblock check if it belongs to the processor subdomain
  // then the value is well defined, otherwise do not do anything
  //

  // FRONT
  // check lower left front corner
  if( (k111==1) ) {
    val+=p111;count++;
  }

  // check lower right front corner
  if( (k211==1) ){
    val+=p211;count++;
  }

  // check upper left front corner
  if( (k121==1) ){
    val+=p121;count++;
  }

  // check upper right front corner
  if( (k221==1) ){
    val+=p221;count++;
  }

  // BACK
  // check lower left back corner
  if( (k112==1) ){
    val+=p112;count++;
  }

  // check lower right back corner
  if( (k212==1) ){
    val+=p212;count++;
  }

  // check upper left back corner
  if( (k122==1)){
    val+=p122;count++;
  }

  // check lower left back corner
  if( (k222==1) ){
    val+=p222;count++;
  }

  if(count==0)  {
    return 0.0;
    printf("\n ISOLATED NODE in sint3D.\n");
  } else {
    *flag=count;
    return val/ ( (_F_REAL_8) count);
  }

}

// sgt --------------------------------------------------------

//-------------------------------------------------------------
//
// the structure Nnodes is essential for unstructured grids
// _Nnodes keeps the nodes of the grid for the unstructured output
//
typedef struct node { int i,j,k; } NODE;
extern NODE * _Nodes[MAXBLK]; // structure holding info about the nodes
extern int _Nnod[MAXBLK];     // global counter of the number of nodes
			      // for the block


// ------------------------------------ routines
void _vis_struct(
 const _F_INTEGER * const IDIM,
 const _F_INTEGER * const JDIM,
 const _F_INTEGER * const KDIM,
 const _F_INTEGER * const LDIM,
 const _F_INTEGER * const IL1,
 const _F_INTEGER * const IL2,
 const _F_INTEGER * const JLV1,
 const _F_INTEGER * const JLV2,
 const _F_INTEGER * const KL1,
 const _F_INTEGER * const KL2,
 const _F_INTEGER * const KEYOUT,
 const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
 );

void _vis_struct_bin(
 const _F_INTEGER * const IDIM,
 const _F_INTEGER * const JDIM,
 const _F_INTEGER * const KDIM,
 const _F_INTEGER * const LDIM,
 const _F_INTEGER * const IL1,
 const _F_INTEGER * const IL2,
 const _F_INTEGER * const JLV1,
 const _F_INTEGER * const JLV2,
 const _F_INTEGER * const KL1,
 const _F_INTEGER * const KL2,
 const _F_INTEGER * const KEYOUT,
 const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
 );

void _vis_const_unstruct(
 const _F_INTEGER * const IDIM,
 const _F_INTEGER * const JDIM,
 const _F_INTEGER * const KDIM,
 const _F_INTEGER * const LDIM,
 const _F_INTEGER * const IL1,
 const _F_INTEGER * const IL2,
 const _F_INTEGER * const JLV1,
 const _F_INTEGER * const JLV2,
 const _F_INTEGER * const KL1,
 const _F_INTEGER * const KL2,
 const _F_INTEGER * const KEYOUT,
 const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
);

void _vis_const_unstruct(
 const _F_INTEGER * const IDIM,
 const _F_INTEGER * const JDIM,
 const _F_INTEGER * const KDIM,
 const _F_INTEGER * const LDIM,
 const _F_INTEGER * const IL1,
 const _F_INTEGER * const IL2,
 const _F_INTEGER * const JLV1,
 const _F_INTEGER * const JLV2,
 const _F_INTEGER * const KL1,
 const _F_INTEGER * const KL2,
 const _F_INTEGER * const KEYOUT,
 const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8  * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4  * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 * * r8_scl_list,
 _F_REAL_8 * * r8_vec_list
);

void _vis_full_unstruct(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
);

void _vis_full_unstruct_bin(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
);

void _vis_corner_point(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8  * const xc,
 const _F_REAL_8 * const yc,
 const _F_REAL_8 * const zc,
 const int nscl, const int nvec,
 _F_REAL_8 * * r8_scl_list,
 _F_REAL_8 * * r8_vec_list
);

// vtk
void _vis_vtk_unstruct(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 // gp: added flag for binary vs ascii vtk output
 const int vis_binary,
 const _F_REAL_8 * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4 * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 ** r8_scl_list,
 _F_REAL_8 ** r8_vec_list
);

void _vis_vtkcorner_point
(
  const _F_INTEGER* const IDIM,
  const _F_INTEGER* const JDIM,
  const _F_INTEGER* const KDIM,
  const _F_INTEGER* const LDIM,
  const _F_INTEGER* const IL1,
  const _F_INTEGER* const IL2,
  const _F_INTEGER* const JLV1,
  const _F_INTEGER* const JLV2,
  const _F_INTEGER* const KL1,
  const _F_INTEGER* const KL2,
  const _F_INTEGER* const KEYOUT,
  const _F_INTEGER* const NBLK,
 // fixed parameters
 const int flag,
 // gp: added flag for binary vs ascii vtk output
 const int vis_binary,
 const _F_REAL_8 * const xc,
 const _F_REAL_8 * const yc,
 const _F_REAL_8 * const zc,
 const int nscl, const int nvec,
 _F_REAL_8** r8_scl_list,
 _F_REAL_8** r8_vec_list
);
// typedefs, etc shared by visual4.dc, visual8.dc

typedef struct visjcells { int i,index;float visdx,visdy;} VISJC ;
typedef struct visicells { int j,k,visJ,visjmin,visjmax; VISJC *vjp;} VISIC;
typedef struct rectcells { int i,j,k,index;} RECT ;
typedef struct rectlist  {
  int n,visI,visjmin,visjmax; RECT *cells;} RECTL;

static int Numslices=0;

typedef struct point {_F_REAL_8 x,y,z; int nb, i,j,k;} POINTS;
extern POINTS  GPoint1[MAXSLI],GPoint2[MAXSLI];

typedef struct slicelist  {
  int nb,n,idim,jdim,ncurr;
  RECT *pts ;
} SLICE;

extern SLICE   Slice [MAXSLI];

int find_ind(_F_REAL_8 y,
	     const _F_REAL_8 *yrec,
	     const _F_REAL_4 *dyrec,
	     int jbeg,int jend);

int get_line(_F_REAL_8 x1,_F_REAL_8 y1,_F_REAL_8 x2,_F_REAL_8 y2,
	      double *a,double *b,double *c);

void points2grid(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8  * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4  * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec
 );

#define biglist Slice[slice].pts

void _vis_rectangle(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8  * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4  * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 * * r8_scl_list,
 _F_REAL_8 * * r8_vec_list
);

void _vis_rectangle_bin(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK,
 // fixed parameters
 const int flag,
 const _F_REAL_8  * const vis_xrec,
 const _F_REAL_8 * const vis_yrec,
 const _F_REAL_8 * const vis_zrec,
 const _F_REAL_4  * const vis_dxrec,
 const _F_REAL_4 * const vis_dyrec,
 const _F_REAL_4 * const vis_dzrec,
 const int nscl, const int nvec,
 _F_REAL_8 * * r8_scl_list,
 _F_REAL_8 * * r8_vec_list
);

void _cr_INIT_fname(char *filename,int flag,int myblk);
void _cr_ZONE_fname(char *filename,int flag,int myblk);
void _cr_ZONE_fname_bin(char *filename,int flag,int myblk,int lvar);
void _INFO_print(int info_flag,char *filename,int myblk);

// ------------------------------------------------------ DEBUG
//#define DEBUG
extern FILE *fvdebug;
//------------------------------------------------------------
