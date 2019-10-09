// -----------------------------------------------------------------
// file: visvtk.c
//
// visualization output in the vtk format for use in software such
// as paraview, visit, etc.
//
// SGT, 09/20/09   Initial version, driver for vtk visualization
// Gergina Pencheva 10/14/15   Added flag for binary vs ascii vis vtk output
//-----------------------------------------------------------------

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
// gp
#include <stdbool.h>

#include "cfsimple.h"
#include "visualc.h"

// --------------------------------------------------------------
// output in a vtk unstructured form. This is a work function

void _vis_vtkoutput (

// first 12 IPARS parameters passed to callwork

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

  // visualization parameters: flag, x,y,z and number of variables
  // to be output : first number of scalars, next number of vectors

  const _F_INTEGER * ivisflag,
// gp : added flag for binary/ascii vtk output
  const bool * ivis_binary,
  const _F_REAL_8  * const vis_xrec,
  const _F_REAL_8  * const vis_yrec,
  const _F_REAL_8  * const vis_zrec,
  const _F_REAL_4  * const vis_dxrec,
  const _F_REAL_4  * const vis_dyrec,
  const _F_REAL_4  * const vis_dzrec,
  const _F_INTEGER * numscalar,
  const _F_INTEGER * numvector,
  // list of all the UNNAMED variables: first scalar, next vector
  ...
  )
{

// read all the common parameters into global variables of this file

       _Vis_flag=*ivisflag;
  const int flag=*ivisflag;
  const bool vis_binary=*ivis_binary;
  const int nvec=*numvector;
  const int nscl=*numscalar;
  const int narg= nvec+nscl;


  // gp  extend support for dirname to vtk output
  // gp  add flag for binary/ascii output file
  if(_Myprc == 0) {
     printf("%s vtk output (%s%s) in block=%d flag=%d, nvisvars=%d, ",
       vis_binary ? "Binary" : "Ascii",DIRname,ROOTname,*NBLK-1,flag,narg);
     printf("step=%d, tim=%g\n", _Nstep, _Time);
  }

  _update_Visinf();

  if ( ((flag != 7) && (flag != 10)) || (narg <=0) || (narg>MAXVISVARS) ) {
     if(_Myprc == 0)
        fprintf(stderr,"Invalid flag or narg. Leaving vis_vtkoutput.\n");
     return;
  }
  else {
    //  ------------------------ read the argument list
    // read parameters passed in the argument list  : vectors and scalars

    va_list ap;
    int i;

    _F_REAL_8 ** r8_scl_list
      =(_F_REAL_8 **) malloc((nscl)*(sizeof(_F_REAL_8*)));
    _F_REAL_8 ** r8_vec_list
      =(_F_REAL_8 **) malloc((nvec)*(sizeof(_F_REAL_8*)));

    if( (nscl>0) && (r8_scl_list == NULL) )
      {fprintf(stderr,"\n No memory :-(\n");exit(-1);}

    if( (nvec>0) && (r8_vec_list == NULL) )
      {fprintf(stderr,"\n No memory :-(\n");exit(-1);}

    va_start(ap, numvector); // numvector=last NAMED parameter

    for(i=0;i< (nscl);i++) {
      r8_scl_list[i]=  va_arg(ap, _F_REAL_8 *) ;
    }

    for(i=0;i< (nvec);i++) {
      r8_vec_list[i]=  va_arg(ap, _F_REAL_8 *) ;
    }

    va_end(ap);

    _vis_vtk_unstruct(
       IDIM, JDIM,   KDIM,   LDIM,   IL1, IL2,
       JLV1,   JLV2,   KL1, KL2,   KEYOUT,   NBLK,
       flag, vis_binary,
       vis_xrec, vis_yrec, vis_zrec,
       vis_dxrec, vis_dyrec, vis_dzrec,
       nscl, nvec, r8_scl_list, r8_vec_list);

    free(r8_scl_list);
    free(r8_vec_list);
  }
}

// --------------------------------------------------------------
// output in a vtk unstructured form. This is a work function

void _vis_vtkoutput_mpfa (

// first 12 IPARS parameters passed to callwork

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

  // visualization parameters: flag, x,y,z and number of variables
  // to be output : first number of scalars, next number of vectors

  const _F_INTEGER * ivisflag,
// gp : added flag for binary/ascii vtk output
  const bool * ivis_binary,
  const _F_REAL_8  * const xc,
  const _F_REAL_8 * const yc,
  const _F_REAL_8 * const zc,
  const _F_INTEGER * numscalar,
  const _F_INTEGER * numvector,
  // list of all the UNNAMED variables: first scalar, next vector
  ...
  )
{

// read all the common parameters into global variables of this file

       _Vis_flag=*ivisflag;
  const int flag=*ivisflag;
  const int vis_binary=*ivis_binary;
  const int nvec=*numvector;
  const int nscl=*numscalar;
  const int narg= nvec+nscl;

  // gp  extend support for dirname to vtk output
  // gp  add flag for binary/ascii output file
  if(_Myprc == 0) {
    printf("%s vtk output (%s%s) in block=%d flag=%d, nvisvars=%d, ",
      vis_binary ? "Binary" : "Ascii",DIRname,ROOTname,*NBLK-1,flag,narg);
    printf("step=%d, tim=%g\n", _Nstep, _Time);
  }
	
  _update_Visinf();

  if ( ((flag != 8) && (flag != 10)) || (narg <=0) || (narg>MAXVISVARS) ) {
     if(_Myprc == 0)
        fprintf(stderr,"Invalid flag or narg. Leaving vis_vtkoutput2.\n");
     return;
  }
  else {
    //  ------------------------ read the argument list
    // read parameters passed in the argument list  : vectors and scalars

    va_list ap;
    int i;

    _F_REAL_8 ** r8_scl_list
      =(_F_REAL_8 **) malloc((nscl)*(sizeof(_F_REAL_8*)));
    _F_REAL_8 ** r8_vec_list
      =(_F_REAL_8 **) malloc((nvec)*(sizeof(_F_REAL_8*)));

    if( (nscl>0) && (r8_scl_list == NULL) )
      {fprintf(stderr,"\n No memory :-(\n");exit(-1);}

    if( (nvec>0) && (r8_vec_list == NULL) )
      {fprintf(stderr,"\n No memory :-(\n");exit(-1);}

    va_start(ap, numvector); // numvector=last NAMED parameter

    for(i=0;i< (nscl);i++) {
      r8_scl_list[i]=  va_arg(ap, _F_REAL_8 *) ;
    }

    for(i=0;i< (nvec);i++) {
      r8_vec_list[i]=  va_arg(ap, _F_REAL_8 *) ;
    }

    va_end(ap);

    _vis_vtkcorner_point(
       IDIM, JDIM,   KDIM,   LDIM,   IL1, IL2,
       JLV1,   JLV2,   KL1, KL2,   KEYOUT,   NBLK,
       flag, vis_binary, xc, yc, zc, nscl, nvec, r8_scl_list,
       r8_vec_list);

    free(r8_scl_list);
    free(r8_vec_list);
  }
}

// ------------------------------------------------------
// open pvtu file for current time step and write header
// information
void _vis_pvtu_set(const int* nscal, const int* nvecs)
{

  char pvtufilename[MAXSTRLEN];
  char sclnam[MAXSTRLEN];
  char vecnam[MAXSTRLEN];
  FILE *pvtuPtr;
  int iarg, nscl, nvec;
  nscl=*nscal;
  nvec=*nvecs;

  if(_Myprc == 0)
  {
    // gp  - extend support for dirname to vtk output
    sprintf(pvtufilename,"%s%s_nstep_%d.pvtu",DIRname,ROOTname,_Nstep);
    pvtuPtr=fopen(pvtufilename,"w");
    if(pvtuPtr==NULL)
    {
       fprintf(stderr,"Error opening file %s\n",pvtufilename);
       return;
    }

    fprintf(pvtuPtr,"<VTKFile type=\"PUnstructuredGrid\" version=\"0.1\" \
 byte_order=\"LittleEndian\">\n");
    fprintf(pvtuPtr,"  <PUnstructuredGrid GhostLevel=\"0\">\n");
    fprintf(pvtuPtr,"    <PPointData>\n");

    // write PDataArray for each variable
    for(iarg=0; iarg<nscl; iarg++)
    {
       if(_Vis_pars[iarg].name!=NULL)
          fprintf(pvtuPtr, "      <PDataArray type=\"Float32\" Name=\"%s\"\
 NumberOfComponents=\"1\" format=\"binary\"/>\n",_Vis_pars[iarg].name);
       else
       {
          sprintf(sclnam,"v%d",iarg+1);
          fprintf(pvtuPtr, "      <PDataArray type=\"Float32\" Name=\"%s\"\
 NumberOfComponents=\"1\" format=\"binary\"/>\n",sclnam);
       }
    }

    for(iarg=0; iarg<(nvec/3); iarg++)
    {
       if(_Vis_vecnam[iarg]!=NULL)
          fprintf(pvtuPtr, "      <PDataArray type=\"Float32\" Name=\"%s\"\
 NumberOfComponents=\"3\" format=\"binary\"/>\n",_Vis_vecnam[iarg]);
       else
       {
          sprintf(vecnam,"v%d",iarg+1);
          fprintf(pvtuPtr, "      <PDataArray type=\"Float32\" Name=\"%s\"\
 NumberOfComponents=\"3\" format=\"binary\"/>\n",vecnam);
       }
    }

    fprintf(pvtuPtr, "    </PPointData>\n");
    fprintf(pvtuPtr, "    <PPoints>\n");
    fprintf(pvtuPtr, "      <PDataArray type=\"Float32\" Name=\"Points\" \
 NumberOfComponents=\"3\" format=\"binary\"/>\n");
    fprintf(pvtuPtr, "    </PPoints>\n");
    fclose(pvtuPtr);
  }

}


// ------------------------------------------------------
// open pvtu file for current time step and write footer
// information
void _vis_pvtu_quit()
{

  char pvtufilename[MAXSTRLEN];
  FILE *pvtuPtr;

  if(_Myprc == 0)
  {
    // gp  - extend support for dirname to vtk output
    sprintf(pvtufilename,"%s%s_nstep_%d.pvtu",DIRname,ROOTname,_Nstep);
    pvtuPtr=fopen(pvtufilename,"a");
    if(pvtuPtr==NULL)
    {
       fprintf(stderr,"Error opening file %s\n",pvtufilename);
       return;
    }
    fprintf(pvtuPtr, "  </PUnstructuredGrid>\n");
    fprintf(pvtuPtr, "</VTKFile>\n");
    fclose(pvtuPtr);
  }

}
