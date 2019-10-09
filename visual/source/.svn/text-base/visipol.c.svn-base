#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "pV3.h"

#include "cfsimple.h"

static void int1D_edge(int c1, int c2,
		       _F_REAL_8 p1, _F_REAL_8 p2, _F_REAL_8 d1,_F_REAL_8 d2,
		       int *count, _F_REAL_8 *val);

_F_REAL_8 sint3Dnonuniform_with_ghosts(			     
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

_F_REAL_8 vint3D_nonuniform(
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

_F_REAL_8 sint3D_with_ghosts(			     
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

static void int1D_edge(int c1, int c2,
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

_F_REAL_8 vint3D(
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
_F_REAL_8 sint3D_without_ghosts(
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

