// -----------------------------------------------------------------
// file: vistec.c
//
// visualization output in the Tecplot format for IPARS framework
// driver and utility routines
//
// MPeszynska, 3/19/98
// last mod. (mpesz) 11/20/98, see CVS log files for description of updates
// moved routines  from visual.dc to sveral smaller files on 11/20/98
//
// RMartino, 3/2/02
// added necessary functions for binary output
//
// Ben Ganis, 6/21/16
// Modern tecplot format without postprocessing or interpolation
// for both bricks and hexahedral.
//-----------------------------------------------------------------

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "cfsimple.h"
#include "visualc.h"

// --------------------------------------------------------------
// output in a fully unstructured form
// it is an IPARS work routine
//
void _vis_tecoutput (

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
  const _F_REAL_8  * const vis_xrec,
  const _F_REAL_8  * const vis_yrec,
  const _F_REAL_8  * const vis_zrec,
  const _F_REAL_4  * const vis_dxrec,
  const _F_REAL_4  * const vis_dyrec,
  const _F_REAL_4  * const vis_dzrec,
  const _F_REAL_8  * const xc,
  const _F_REAL_8  * const yc,
  const _F_REAL_8  * const zc,
  const _F_INTEGER * numscalar,
  const _F_INTEGER * numvector,

  // list of all the UNNAMED variables: first scalar, next vector
  ...
  )
{

// read all the common parameters into global variables of this file

  const int flag=*ivisflag;
  const int nvec=*numvector;
  const int nscl=*numscalar;
  const int narg= nvec+nscl;

  // DEBUG
  if (_Myprc ==0)
    printf("Vis output (%s%s) in block=%d flag=%d, nvisvars=%d\n",
      DIRname,ROOTname,*NBLK-1,flag,narg);
	
  _update_Visinf();

  if( (narg <=0) || (narg>MAXVISVARS) ) {
    if (_Myprc == 0)
      fprintf(stderr,"Invalid args to vis_tecoutput.\n");
    return;
  }
  else {
    //  ------------------------ read the argument list
    // read parameters passed in the argument list  : vectors and scalars

    va_list ap;
    int i,ctr;

    _F_REAL_8 * * r8_scl_list
      =(_F_REAL_8 * *) malloc((nscl)*(sizeof(_F_REAL_8 *)));
    _F_REAL_8 * * r8_vec_list
      =(_F_REAL_8 * *) malloc((nvec)*(sizeof(_F_REAL_8 *)));

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

    //
    // if last time vis routine was called with different flag, reset Vis_cnt
    //

    //
    // call vis routine appropriate to flag
    //

    switch(flag) {
    case 1: _vis_struct
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag,
	       vis_xrec, vis_yrec, vis_zrec,
	       vis_dxrec, vis_dyrec, vis_dzrec,
	       nscl, nvec, r8_scl_list, r8_vec_list
	       ); break;			
    case 2: _vis_const_unstruct
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag,
	       vis_xrec, vis_yrec, vis_zrec,
	       vis_dxrec, vis_dyrec, vis_dzrec,
	       nscl, nvec, r8_scl_list, r8_vec_list
	       ); break;			
    case 3: _vis_full_unstruct
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag,
	       vis_xrec, vis_yrec, vis_zrec,
	       vis_dxrec, vis_dyrec, vis_dzrec,
	       nscl, nvec, r8_scl_list, r8_vec_list
	       ); break;			
    case 4: _vis_rectangle
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag,
	       vis_xrec, vis_yrec, vis_zrec,
	       vis_dxrec, vis_dyrec, vis_dzrec,
	       nscl, nvec, r8_scl_list, r8_vec_list
	       ); break;			
    case 5: _vis_full_unstruct_bin
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag,
	       vis_xrec, vis_yrec, vis_zrec,
	       vis_dxrec, vis_dyrec, vis_dzrec,
	       nscl, nvec, r8_scl_list, r8_vec_list
	       ); break;
    case 6: _vis_corner_point
	      (
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
	       flag, xc, yc, zc, nscl, nvec, r8_scl_list,
               r8_vec_list
	       ); break;
    case 9:
    case 10:
             vis_tec_grid1_(
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK,
               xc, yc, zc, &narg);
             ctr=0;
             for(i=0;i<nscl;i++) { ctr++;
               int ioff=_Vis_pars[i].ldim;
               char *name=_Vis_pars[i].name;
               if ((strcmp(name,"XPERM")==0) ||
                   (strcmp(name,"YPERM")==0) ||
                   (strcmp(name,"ZPERM")==0) ||
                   (strcmp(name,"POR")==0)) {
                 vis_tec_array_r4_(
                   IDIM, JDIM, KDIM, LDIM, IL1, IL2,
                   JLV1, JLV2, KL1, KL2, KEYOUT, NBLK, &ctr,
                   r8_scl_list[i],&ioff);

               }
               else {
                 vis_tec_array_(
                   IDIM, JDIM, KDIM, LDIM, IL1, IL2,
                   JLV1, JLV2, KL1, KL2, KEYOUT, NBLK, &ctr,
                   r8_scl_list[i],&ioff);
               }
             }

             for(i=0;i<nvec;i++) { ctr++;
               int ioff=1;
               vis_tec_array_(
                 IDIM, JDIM, KDIM, LDIM, IL1, IL2,
                 JLV1, JLV2, KL1, KL2, KEYOUT, NBLK, &ctr,
                 r8_vec_list[i],&ioff);
             }
             vis_tec_grid2_(
	       IDIM, JDIM, KDIM, LDIM, IL1, IL2,
	       JLV1, JLV2, KL1, KL2, KEYOUT, NBLK, &narg);
            break;
    default:
      if (_Myprc == 0)
        fprintf(stderr,"Invalid flag in vis_tecoutput.\n");
      return;
    }
    // DEBUG
    /**
      for(i=0;i<narg;i++)
      printf("\nVAR %d has offset=%d name=[%s] of len=%d\n",
      i,_Vis_pars[i].ldim,_Vis_pars[i].name,
      strlen(_Vis_pars[i].name));
      **/

    free(r8_scl_list);
    free(r8_vec_list);
  }
}

// ----------------------------------------------------------------

void _cr_INIT_fname(char *filename,int flag,int myblk){
  switch(_Filestyle) {
  case 2:  // IBM PC style
           sprintf(filename,"%s%s_%d_%d.ini",
             DIRname,ROOTname,myblk,_Myprc); break;
  default: // NO_PC
           sprintf(filename,"%s%s.%d.%d.%d.init",
             DIRname,ROOTname,myblk,_Myprc,flag); break;
  }
}

void _cr_ZONE_fname(char *filename,int flag,int myblk){
  switch(_Filestyle) {
  case 2:  // IBM PC style
           sprintf(filename,"%s%s_%d_%d.%d",
             DIRname,ROOTname,myblk,_Myprc,_Nstep); break;
  default: // NO_PC
           sprintf(filename,"%s%s.%d.%d.%d.%d",
             DIRname,ROOTname,myblk,_Myprc,flag,_Nstep); break;
  }
}

void _cr_ZONE_fname_bin(char *filename,int flag,int myblk,int lvar){

  switch(_Filestyle) {
  case 2:  // IBM PC style
           sprintf(filename,"%s%s_%d_%d.%d",
             DIRname,ROOTname,myblk,_Myprc,_Nstep); break;
  default: // NO_PC
           sprintf(filename,"%s%s.%d.%d.%d.%d",
             DIRname,ROOTname,myblk,_Myprc,flag,lvar); break;
  }
}

void  _INFO_print(int info_flag,char *filename,int myblk)
{
  //
  // print the information to INFO file (for given proc/fblock)
  // and send the information about which INFO file has been
  // produced to processor 0, processor 0 is supposed to
  //
  char INFOfilename[50];FILE *finfo;

  switch(_Filestyle) {
  case 2: // IBM PC
	       sprintf(INFOfilename,"%s%s_%d.%d",DIRname,INFOname,myblk,_Myprc);
               break;
  default: // no PC
		sprintf(INFOfilename,"%s%s.%d.%d",DIRname,INFOname,myblk,_Myprc);
                break;
  }

  if(info_flag==0)
    finfo=fopen(INFOfilename,"w");
  else
    finfo=fopen(INFOfilename,"a");
  if(finfo!=NULL) {
    if(info_flag==0)
      fprintf(finfo,
	      "<Tecplot vis. output for f.block=%d proc=%d. Files:>\n",
	      myblk,_Myprc);
    fprintf(finfo,"%s\n",filename);
    fclose(finfo);
  } else {
    fprintf(stderr,"\nProblems with file:%s\n",INFOfilename);
  }

}

// ----------------------------------------------------------------
// set the root name for the filename and some aux. names for zones
//
void _vis_fname_set(
		    _F_INTEGER *style_flag,
		    _F_INTEGER *par_visflag,
                    char * rootname, char * dirname)
{
  const int visflag = *par_visflag;
  char auxname[MAXFNAMELEN];
  char auxname2[MAXFNAMELEN];

  trimblanks(rootname);
  trimblanks(dirname);

// bag8, gp - added DIRname
  auxname[0]='\0';
  auxname2[0]='\0';

  //printf("dirname=%s\n",dirname);
  //printf("strlen(dirname)=%d\n",strlen(dirname));

    if (strlen(dirname) > 0) {
      strcat(auxname, dirname);
      trimblanks(auxname);
      strcat(auxname, "/");
    }

  sprintf(DIRname,"%s",auxname);
  strcat(auxname2, rootname);
  strcat(auxname, rootname);
  trimblanks(auxname2);
  trimblanks(auxname);
  sprintf(ROOTname,"%s",auxname2);
  sprintf(INFOname,"%s_info",auxname2);
  _Filestyle = *style_flag;

  // create a file for each processor that will contain the
  // numbers of faultblocks active on this processor

  if (*par_visflag !=4 ) {
    char PROCname[30];int i; FILE *fp;

    sprintf(PROCname,"%s_blks.%d",auxname,_Myprc);
    fp=fopen(PROCname, "w");
    if(fp == NULL) {
      fprintf(stderr,"Error opening %s",PROCname);exit(-1);
    } else {
      for(i=0;i<_Mblk;i++) {
	const int r= _ismyblk(&i);
	if (r)
	  fprintf(fp,"Block %d\n",i);
      }
      fclose(fp);
    }
  } else {
    char PROCname[30];int i; FILE *fp;

    sprintf(PROCname,"%s_slices.%d",auxname,_Myprc);
    fp=fopen(PROCname, "w");
    if(fp == NULL) {
      fprintf(stderr,"Error opening %s",PROCname);exit(-1);
    } else {
      for(i=0;i<Numslices;i++) {
	const int r= _ismyslice(&i);
	if (r)
	  fprintf(fp,"Slice %d\n",i);
      }
      fclose(fp);
    }
  }

  // create a file for processor

  if (_Myprc ==0) {
    FILE *fp;
    char VISINFname[30];
    sprintf(VISINFname,"%sVis.inf",DIRname);

    if(_Ifnew) { fp = fopen(VISINFname, "w");_Ifnew=0;}
    else fp = fopen(VISINFname,"a");

    if(fp == NULL) {
      fprintf(stderr,"Error opening main file Vis.inf");exit(-1);
    } else {
    //DEBUG
      // printf("Vis.inf open.\n");

      if(visflag!=4)
	fprintf(fp,"Rootname = %s\n",ROOTname);
      else
	fprintf(fp,"Rootname = %s\t Numslices = %d\n",ROOTname,Numslices);

      fprintf(fp,"Nprocs   = %d\n",_Nprocs);
      fprintf(fp,"Style    = %d\n",_Filestyle);
      fprintf(fp,"Flag     = %d\n",visflag);

      // request that the current time step be printed when the output is made
      _Vis_newtimestep =1;
      fclose(fp);
    }
  }

}
