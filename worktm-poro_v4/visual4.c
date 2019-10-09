// -----------------------------------------------------------------
// file: visual4.c
//
// rectangle  visualization output for IPARS framework
//
// MPeszynska, 12/15/99
//-----------------------------------------------------------------

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "cfsimple.h"
#include "visualc.h"

extern int Numslices;
POINTS  GPoint1[MAXSLI],GPoint2[MAXSLI];
SLICE   Slice [MAXSLI];

// ----------------------------------------------------------------
// the actual unstructured grid routine
//
void _vis_rectangle
(
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
 )
{
  const int narg=nscl+nvec;
  const int maxblk = (*IDIM)*(*JDIM)*(*KDIM);
  const int maxpls = (*IDIM +1)*(*JDIM +1)*(*KDIM +1);
  const int   kinc = (*IDIM) * (*JDIM) ;
  const int   jinc = (*IDIM) ;
  const int   kbeg = (*KL1) ;
  const int   kend = (*KL2) ;
  const int   ibeg =  *IL1 ;
  const int   iend =  *IL2 ;
  const int   nblk = *NBLK-1;

  //--------------------------
  //int visI,visJ,visJmin,visJmax,visJnum;
  int slice;

  FILE *fp; // general file handle, first used for INITfile, next for ZONEfile
  _F_INTEGER igoff,jgoff,kgoff,nofferr;
	
  if(flag != 4) return ;

  myblkoff(NBLK,&igoff,&jgoff,&kgoff,&nofferr);

  /**
  printf("\nVIS_RECTANGLE %d  at vis_cnt=%d \n",_Nstep, _Vis_cnt[nblk] );
  for(slice=0;slice<Numslices;slice++)
    printf("\nSlice %d of i=%d j=%d n=%d ncurr=%d\n",slice,
	   Slice[slice].idim,Slice[slice].jdim,Slice[slice].n,
	   Slice[slice].ncurr);
  **/

  // -------------------------------- rectangle grid setup or output


  if (_Vis_cnt[nblk]++ ==0) {						
    									
    points2grid(							
		IDIM, JDIM,   KDIM,   LDIM,   IL1, IL2,   		
		JLV1,   JLV2,   KL1, KL2,   KEYOUT,   NBLK,		
		flag,							
		vis_xrec, vis_yrec, vis_zrec,				
		vis_dxrec, vis_dyrec, vis_dzrec);			
    									
    for(slice=0;slice<Numslices;slice++){ // loop over slices		
									
#define Point1 GPoint1[slice]						
#define Point2 GPoint2[slice]						
									
      int i1 = Point1.i; int i2 = Point2.i;				
									
      int j1 = Point1.j; int j2 = Point2.j; 				
      int k1 = Point1.k; int k2 = Point2.k;

      int ji = (j1 < j2 ? 1 : (j1 == j2 ? 0: -1));
      int ki = (k1 < k2 ? 1 : (k1 == k2 ? 0: -1));
 				
      int jj1 = (j1 < j2 ? j1 : j2); 				
      int jj2 = (j1 < j2 ? j2 : j1); 				
      int kk1 = (k1 < k2 ? k1 : k2); 				
      int kk2 = (k1 < k2 ? k2 : k1); 				

      int jd = jj2-jj1+1;						
      int kd = kk2-kk1+1;					
		
      int j=0;								
      int sdiff = kd-jd;						
      int diff = (sdiff > 0 ? sdiff : -sdiff);				
      int diff2 = diff / 2;						

      int mindir = (jd < kd ? jd : kd);					
      int maxdir = (jd < kd ? kd : jd);					

      RECT *list;							
      int jc, kc, d, i;							
      int sliblk=Point1.nb;						
									
      /*								
      printf("\n Found %d %d %d %d %d %d NBL = %dn",			
	     jd,kd,sdiff,diff,diff2,mdiff,				
	     Point1.nb);						
      */								

				
      list = (RECT *) malloc (sizeof(RECT) * maxdir);			
									
      Slice[slice].nb=sliblk;						
   									
      if ( (sliblk != Point2.nb) || (sliblk != nblk)) return;		
#undef Point1 								
#undef Point2 								
									
      //------------------------------------------------------------------
      // finds list of indices between (j1,k1) and (j2,k2) (proc. independ.)
      									
      // nodes before diagonal						
      jc=j1;kc=k1;							
      									
      // diagonal							
      for(d=0;d<mindir;d++){						
	list[j].j=jc;list[j].k=kc;if(d<mindir-1){jc+=ji;kc+=ki;} j++;	
      }									
      									
      // nodes after diagonal						
      for(d=0;d<diff;d++){						
	if(sdiff > 0) kc+=ki;else jc+=ji;list[j].j=jc;list[j].k=kc;j++;	
      }									

      printf("\nFound %d versus maxdir=%d mindir =%d other dim=%dn",	
	     j,maxdir,mindir,						
	     i2-i1+1);							
							
	printf("\nSlice : %d \n",slice);
      // -------------------------------------------------------	
      // record (global) coordinates of all points for this slice	
      // proc 0 writes it to output file				
									
      Slice[slice].idim=i2-i1+1;					
      Slice[slice].jdim=maxdir;						
      Slice[slice].n=Slice[slice].idim * Slice[slice].jdim ;		
      biglist = (RECT *) malloc( sizeof(RECT) * Slice[slice].n);	
      									
      if(biglist == NULL) return;					
									

      if (_Myprc == 0) 							
	printf("\nSlice %d %d %d %d\n",slice,sliblk,			
	Slice[slice].idim, Slice[slice].jdim);				

									
      d=0;      							
      for(j=0;j<maxdir;j++) {						
	for(i=i1;i<=i2;i++) { 	

	  if (_Myprc == 0)
	    printf("\n%d  ---> %d %d %d",d,
		   i,list[j].j,list[j].k);

	  biglist[d].j=list[j].j; biglist[d].k=list[j].k; biglist[d].i=i;
	  d++; 	  	
	} // end loop over i
      }  // end loop over jk

      free (list);

      // ===========
      //	if(slice==2) exit(0);					

      // -------------------------------------------------------
      // find all gridblocks on this processor. Mark others as absent.
		
      for(d=0;d<Slice[slice].n;d++) {
	const int iloc=biglist[d].i-igoff+1;
	const int jloc=biglist[d].j-jgoff+1;
	const int kloc=biglist[d].k-kgoff+1;		
	int mycell = 0;

#define gindex(i,j,k)   ( ( (k)-1 )*kinc+ ( (j)-1)*jinc + ( (i) -1) )	
	  const int index = gindex(iloc,jloc,kloc);	
#undef gindex
	
	  // printf("\nTesting cell : %d %d %d mycell=?",iloc,jloc,kloc);

	  if( (iloc<=iend) && (iloc>=ibeg) && (kloc<=kend) && (kloc>=kbeg)
	      && (jloc<=JLV2[kloc-1]) && (jloc>=JLV1[kloc-1]) &&
	      (KEYOUT[index] == 1)
	      ) mycell = 1;

	  if (mycell == 1 ) biglist[d].index=index;
	  else  biglist[d].index=0;
	
	  Slice[slice].ncurr+=mycell;

	  /**
	  printf("\nPROC_any %d %d %d ind=%d sli_ind=%d",
		 biglist[d].i,
		 biglist[d].j,biglist[d].k,
		 biglist[d].index,d);
	  **/
	
      }  // end loop over all slice cells

      // ------------------------------------------------
      // geometry output for this slice on this processor
      {
	char INITfilename[50];
	const int idim=Slice[slice].idim;const int jdim=Slice[slice].jdim;
	const int n = idim*jdim;
	int iarg;

	_cr_INIT_fname(INITfilename,flag,slice);
	fp=fopen(INITfilename,"w");

	if (fp != NULL) {
	  int k,j,i,iarg,iblk=0;
	
	  _INFO_print(0,INITfilename,slice);

	  // print the header to the file

	  fprintf(fp,"title=init.%d\n",flag);
	  fprintf(fp,"variables=i,j,k,ind,x,y,z");
	  for(iarg=0;iarg<narg;iarg++)
	    if(_Vis_pars[iarg].name!=NULL)
	      fprintf(fp,", %s",_Vis_pars[iarg].name);
	    else
	      fprintf(fp,", v%d",iarg+1);
	
	  // if one of i,j,k is 1 then Tecplot cannot handle it:
	  // make it all then 1d
	
// bag8, gp : added strandid, solutiontime	
	  if ( ( idim == 1 ) || (jdim== 1) )
	    fprintf(fp,
		    "\nzone t=init, i=%d, F=point, ",n);
	  else
	    fprintf(fp,
		    "\nzone t=init, i=%d, j=%d, F=point, ",
		    idim,jdim);
          fprintf(fp,"strandid=0, solutiontime=0.0\n");

	
	  for(j=0;j<n;j++) {
	    int ii = biglist[j].i;
	    int jj = biglist[j].j;
	    int kk = biglist[j].k;
	    int ind =biglist[j].index;
	
	    //	    printf("\n% d %d %d %d %d",j,ii,jj,kk,ind);
	
	    fprintf(fp,"\n %d %d %d %d %g %g %g",
		    ii,jj,kk,ind,
		    vis_xrec[ii],vis_yrec[jj],vis_zrec[kk]);
	
	    for(iarg=0;iarg<narg;iarg++) fprintf(fp," %g",0.0);
	
	  }

	  fprintf(fp,"\n\n");
	  fclose(fp);

	  // ------- generate auxiliary output
	  if(_Myprc == 0) {
	    sprintf(INITfilename,"%s%s.%d",DIRname,ROOTname,slice);
	    fp=fopen(INITfilename,"w");

	    if (fp != NULL) {
	      fprintf(fp,"Slice = %d\n",slice);
	      fprintf(fp,"Numvars = %d\n",narg);
	      fprintf(fp,"Idim = %d\n",idim);
	      fprintf(fp,"Jdim = %d\n",jdim);

	      fclose(fp);
	    }
	
	  }
	}
      }
    } // end loop over slices

  } // end if Vis_cnt


  //-------------------------------------------------
  // values  output for all slices

  for(slice=0;slice<Numslices;slice++) {

    // ----------------------------------------------
    // print the values of variables to the ZONE file
    //
    char ZONEfilename[50];
    int j;
    int lvar;

    _cr_ZONE_fname(ZONEfilename,flag,slice);

    // printf("\n%s\n",ZONEfilename);

    fp=fopen(ZONEfilename,"w");
    if(fp !=NULL) _INFO_print(1,ZONEfilename,slice);

    if (fp != NULL) {
      int lvar,i,j,k;
      const int idim=Slice[slice].idim;const int jdim=Slice[slice].jdim;
      const int n = idim*jdim;

      static char ZONEname [50];
      sprintf(ZONEname,"zone%d.%d.%d",nblk,_Myprc,_Nstep);

      // give info about the zone data:
      // if one of i,j,k is 1 then Tecplot cannot handle it:
      // make it all then 1d

      if ( ( idim == 1 ) || (jdim == 1) )
	fprintf(fp,
		"\nzone t=%s, i=%d,  F=block,d=(1,2,3,4,5,6,7), ",ZONEname,n);
      else
	fprintf(fp,
		"\nzone t=%s, i=%d, j=%d,F=block,d=(1,2,3,4,5,6,7), ",
		ZONEname,idim,jdim);
      // bag8, gp : add strandid, solutiontime
      // gp : modify strandid to allow for time series plot extraction
      //fprintf(fp,"strandid=%d, solutiontime=%g\n",_Nstep+1,_Time);
      fprintf(fp,"strandid=%d, solutiontime=%g\n",nblk+1,_Time);

      for(lvar=0;lvar< (nscl);lvar++) {
	const int ldimoff=(_Vis_pars[lvar].ldim-1)*maxblk;
	_F_REAL_8 scalval;
	_F_REAL_8 *ptr=& ( r8_scl_list[lvar][ldimoff] );
	
	//printf("\nSlice %d, scalar variable %d n=%d\n",slice,lvar,n);
	
	for(j=0;j<n;j++) {
	  int ind = biglist[j].index;
	  const int newline=(j+1) % 5;
	
	  fprintf(fp,"%g ",ptr[ind]);
	  if(newline==0) fprintf(fp,"\n");
	
	  // printf("\n %d (%d) -> %g",j,ind,ptr[ind]);
	}
	
	fprintf(fp,"\n\n");
	
      }//end lvar

      fprintf(fp,"\n\n");
      fclose(fp);
    }  // end if fp != NULL

  } // end loop over slices
}

#undef biglist

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
 )
{
  const int maxblk = (*IDIM)*(*JDIM)*(*KDIM);
  const int maxpls = (*IDIM +1)*(*JDIM +1)*(*KDIM +1);
  const int   kinc = (*IDIM) * (*JDIM) ;
  const int   jinc = (*IDIM) ;
  const int   kbeg = (*KL1) ;
  const int   kend = (*KL2) ;
  const int   ibeg =  *IL1 ;
  const int   iend =  *IL2 ;
  const int   nblk = *NBLK-1;
  const int   jdim = *JDIM;

  int i,j,k;
  int ifo,jf,kf;
  int slice;

  //------------------------------------------------------------
  // finds the (global IPARS/C) grid coordinates for the Points
  // for all slices

  for(slice=0;slice<Numslices;slice++) {

#define Point1 GPoint1[slice]
#define Point2 GPoint2[slice]

    Point1.nb=0;

    Point1.i=find_ind(Point1.x,vis_xrec,vis_dxrec,0,(201)-1);
    Point1.j=find_ind(Point1.y,vis_yrec,vis_dyrec,0,(205)-1);
    Point1.k=find_ind(Point1.z,vis_zrec,vis_dzrec,0,(205)-1);
    /**
    printf("\nIndex(1) found: %g %g %g -> %d %d %d ",
	   Point1.x,Point1.y,Point1.z,Point1.i,Point1.j,Point1.k);
    **/
    Point2.nb=0;

    Point2.i=find_ind(Point2.x,vis_xrec,vis_dxrec,0,(201)-1);
    Point2.j=find_ind(Point2.y,vis_yrec,vis_dyrec,0,(205)-1);
    Point2.k=find_ind(Point2.z,vis_zrec,vis_dzrec,0,(205)-1);
    /**
    printf("\nIndex(2) found: %g %g %g -> %d %d %d ",
	   Point2.x,Point2.y,Point2.z,Point2.i,Point2.j,Point2.k);
    **/
  }
#undef Point1
#undef Point2

}

int find_ind(_F_REAL_8 y,
	     const _F_REAL_8 *yrec,
	     const _F_REAL_4 *dyrec,int jbeg,int jend)
{
int jf=-1,j;

 for(j=jbeg;j<jend;j++) {
    _F_REAL_8 vy =yrec[j];_F_REAL_8 vdy=dyrec[j];
    if( (vy<=y) && (y <=vy+vdy) ){jf=j; break;}
 }

 // printf("\n For %g found %d within %d %d",y,jf,jbeg,jend);

 return jf;
}


int get_line(_F_REAL_8 x1,_F_REAL_8 y1,_F_REAL_8 x2,_F_REAL_8 y2,
	     double *a,double *b,double *c)
{

  double det=x1*y2-x2*y1,detx=x1-x2,dety=y2-y1;

 printf("\n Parameters of line with %g %g %g %g:",
	x1,y1,x2,y2);

  if(fabs(det)>1.e-2) {// line does not pass through zero
   *b=detx/det; *a=dety/det; *c=-1.;
 } else {
   if(fabs(x1*y2)>1.e-2) {*b=1.;*a=-y1/x1; *c=0.;}
   else return -1;
 }

 printf("\n Found line with a=%g b=%g c=%g\n",
	*a,*b,*c);

 return 0;
}

/*
float diam(float x, float y){
  float d;
  d=x*x+y*y;
  d=sqrt(d);
  return d;
}
*/

// in FORTRAN for the array declared (NIDIM,NJDIM) return index(i,j)
// i numbered from 0 ... NIDIM
// j numbered from 1 ... NJDIM
#define NIDIM 3	
#define NJDIM 2
static int myind(int i,int j)
{
return i+(j-1)*NIDIM;
}
#undef NIDIM
#undef NJDIM

#define c_setslice     _F_NAME_(SETSLICE,setslice)

void c_setslice(
_F_INTEGER  *num,
_F_INTEGER *vblk,
_F_REAL_8  *vxc,
_F_REAL_8  *vyc,
_F_REAL_8  *vzc)
{

  int i;
  Numslices=*num;

  for(i=0;i<Numslices;i++) {

    /**
    printf("\nCSLICE : %d (%g %g %g), (%g %g %g)\n",
	   vblk[i],
	   vxc[myind(i,1)],
	   vyc[myind(i,1)],
	   vzc[myind(i,1)],
	   vxc[myind(i,2)],
	   vyc[myind(i,2)],
	   vzc[myind(i,2)]);
    **/

    GPoint1[i].nb=vblk[i];

    GPoint1[i].x=vxc[myind(i,1)];
    GPoint1[i].y=vyc[myind(i,1)];
    GPoint1[i].z=vzc[myind(i,1)];

    GPoint2[i].nb=vblk[i];

    GPoint2[i].x=vxc[myind(i,2)];
    GPoint2[i].y=vyc[myind(i,2)];
    GPoint2[i].z=vzc[myind(i,2)];

  }

}


_F_INTEGER _ismyslice (_F_INTEGER *sli) {
return 1;
}

