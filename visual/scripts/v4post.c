#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "cfsimple.h"
#include "visualc.h"

typedef struct cell {int proc,index,nc,vx,vy;float val;} CELL;
CELL ** Cell [MAXBLK];
float * GVisdx [MAXBLK];
float * GVisdy [MAXBLK];

typedef struct rectcells { 
  int i,j,k,index;
  float vx,vy,x,y,z,dx,dy,dz,val;
} RECT ;
typedef struct rectlist  { 
  int n,visjmin,visjmax;
  RECT *cells;
} RECTL;
RECTL  * Rectangle [ MAXBLK ];

typedef struct visp {int visJ, visjmin, visjmax; int *vjp;} VISP ;
typedef struct visplist {VISP *vip;} VISPL;
VISPL   * Visp [ MAXBLK ];

float * * Visdx[MAXBLK];
float * * Visdy[MAXBLK];

int Nblks=1;
int Nprocs=1;
int visI, visJ, visJmin,visJmax;

float ipol(int nblk,int i,int j);

//-----------------------------------------------------------------
main() {

  int nblk=0;
  
  for(nblk=0;nblk<Nblks;nblk++){

    int nproc=0;int nc=0; int in,ix,jn,jx,kn,kx;
    FILE *f;

    Rectangle [nblk] = (RECTL *) malloc(Nprocs*sizeof(RECTL));
    Visp [nblk] = (VISPL *) malloc(Nprocs*sizeof(VISPL));
    Visdx [nblk] = (float **) malloc(Nprocs*sizeof(float *));
    Visdy [nblk] = (float **) malloc(Nprocs*sizeof(float *));
   
    // read/count all cells for this block and all processors
    
    //for(nproc=0;nproc<0;nproc++)
    {
      int n,vi,i,j,k,vin,vix;
      VISPL *vlp= &(Visp [nblk][nproc]) ;

      f=fopen("Rect.0.0","r");

      // get the total number of (jks): shoudl be equal for every proc.
      fscanf(f,"%d",&vi);
      if( (nproc!=0) && (vi!=visI)) exit(0);
      else visI=vi;

      Visdx [nblk] [nproc] = (float *) malloc(visI*sizeof(float));
      vlp->vip = (VISP *) malloc(visI*sizeof(VISP));

      //-------------------------
      // read the gridblock sizes : visdx first
      for(i=0;i<visI;i++) {
	fscanf(f,"%f",&Visdx[nblk][nproc][i]);
      }

      //--------------------------
      // read the gridblock sizes for visJ

      fscanf(f,"%d",&vi);visJ=vi;
      Visdy [nblk] [nproc] = (float *) malloc(visJ*sizeof(float));
      for(i=0;i<visJ;i++) {
	fscanf(f,"%f",&Visdy[nblk][nproc][i]);
      }

      //----------------------
      // echo visdx visdy
      for(i=0;i<visI;i++) for(j=0;j<visJ;j++) 
	printf("%d %d %f %f\n",i,j,
	       Visdx[nblk][nproc][i],
	       Visdy[nblk][nproc][j]
	       );
  
      //-----------------------------------
      // read cells for every visI

      for(i=0;i<visI;i++) {
	int j,visJ;
	VISP * vp=vlp->vip;
	fscanf(f,"%d",&j);if(j!=i) break;
	fscanf(f,
	       "%d %d %d",
	       &(vp[i].visJ),&(vp[i].visjmin),&(vp[i].visjmin));	
	visJ=vp[i].visJ;
	vp[i].vjp= (int *) malloc(visJ*sizeof(int));

	for(j=0;j<vp[i].visJ;j++)
	  fscanf(f,"%d",&(vp[i].vjp[j]));
      }

      // -----------------
      // echo what we read
      for(i=0;i<visI;i++) {
	int j,visJ;
	VISP * vp=vlp->vip;

	printf(" %d",i);
	printf(
	       " %d %d %d",
	       (vp[i].visJ),(vp[i].visjmin),(vp[i].visjmin));	
	visJ=vp[i].visJ;
	for(j=0;j<vp[i].visJ;j++)
	  printf(" %d",(vp[i].vjp[j]));
	printf("\n");
      }

      //-----------------
      // get info about the cells that are active on this processor
      fscanf(f,"%d",&n);
      Rectangle[nblk][nproc].n = n;

      fscanf(f,"%d",&vin);
      fscanf(f,"%d",&vix);
      Rectangle[nblk][nproc].visjmin = vin;
      Rectangle[nblk][nproc].visjmax = vix;
	
      //printf("Rectangle (%d %d) number cells: %d %d %d\n",
      //nblk,nproc,n,vin,vix);
	     
      nc+=n;
      if(n>0){
	int vc;
	RECT *r = (RECT *) malloc( n * sizeof(RECT));	
	Rectangle[nblk][nproc].cells = r;
	
	for (vc=0;vc<n;vc++) {	  
	  fscanf(f,"%d %d %d %d %f %f %f %f %f %f",
		 &(r[vc].i),&(r[vc].j),&(r[vc].k),&(r[vc].index),
		 &(r[vc].x),&(r[vc].y),&(r[vc].z),
		 &(r[vc].dx),&(r[vc].dy),&(r[vc].dz));
	  
	  printf("%d %d %d %g %g %g\n",
		 (r[vc].i),(r[vc].j),(r[vc].k),
		 (r[vc].x),(r[vc].y),(r[vc].z));
   
	}	
      }  
      fclose(f);
    }//loop over nprocs

    //---------- loop to find imin, imax, etc.
    
#ifndef INT_MAX
#include <limits.h>
#endif
    { 
      nproc=0; visJmin=INT_MAX; visJmax=0;

      //for(nproc=0;nproc<0;nproc++)
	{
	int n=Rectangle[nblk][nproc].n;
	if(n>0){
	  const int vn=Rectangle[nblk][nproc].visjmin;
	  const int vx=Rectangle[nblk][nproc].visjmax;	  
	  if (visJmin > vn) visJmin=vn;
	  if (visJmax < vx) visJmax=vx;

	  printf("\nLoop proc: %d %d %d %d\n",
		 vn,vx,visJmin,visJmax);
	}  
	}//loop over nprocs
    printf("Found minmax: %d %d \n",visJmin,visJmax);
    
    // compute size visJ=absolutely the largest span of cells in visJ dir.
    visJ = visJmax - visJmin+1;
    if(visJ <=0) exit(0);
    } // end visJmin
        
    printf("\nFound rectangle size : %d x %d (%d ..%d) \n",visI,visJ,
	   visJmin,visJmax);
    //----------------------------------------------
    Cell[nblk] = (CELL **) malloc(visI*sizeof(CELL *));
    GVisdx[nblk] = (float *) malloc(visI*sizeof(float));
    GVisdy[nblk] = (float *) malloc(visJ*sizeof(float));

    {
      int i,j;
      for(i=0;i<visI;i++) {
	Cell[nblk][i] = (CELL *) malloc(visJ*sizeof(CELL));
	for(j=0;j<visJ;j++) {
	  Cell[nblk][i][j].proc=-1;
	  Cell[nblk][i][j].index=-1;
	  Cell[nblk][i][j].nc=-1;
	}
      }
    }

    //allocated OK: fill in the data
    {
      int i;int nc=0;
      for(i=0;i<visI;i++) {
	int j,jvis;
	VISP *vp = &(Visp[nblk][nproc].vip[i]);
	int *ivp = vp->vjp;
	int visj = vp->visJ;

	CELL *cell = Cell[nblk][i];	

	printf("For visi=%d number visj=%d\n",i,visj);

	for(j=0;j<visj;j++) {
	  const int jvis = ivp [j] ;
	  const int index=Rectangle[nblk][nproc].cells[nc].index;
	  cell[jvis].proc = nproc;
	  cell[jvis].index = index;
	  cell[jvis].nc = nc;
	  nc++;
	} // end for j
      } // end for i
      
    } // end fill Cell

    //-----------------------------
    // for every j.. visJmin .. visJmax find processor location and
    // record visdy
    {
      int j,nproc;
      for(j=visJmin;j<=visJmax;j++) {
	for(nproc=0;nproc<=Nprocs;nproc++) {
	  const int vjn=Rectangle[nblk][nproc].visjmin;
	  if (
	      (Rectangle[nblk][nproc].visjmin <= j) 
	      &&
	      (Rectangle[nblk][nproc].visjmax >= j) 
	      ) {
	    const int jj = j-visJmin; // index in global visJ
	    const int jv = j-vjn; // index in local proc. numbers (source)
	    float glob=Visdy[nblk][nproc][jv];

	    GVisdy[nblk][jj]=Visdy[nblk][nproc][jv];

	    printf("Global found for %d in proc=%d with %d %d %d is=%g\n",
		   j,nproc,vjn,jj,jv,glob);
	    break;
	  }
	}
      }
      
    }
    //------------------------------
    // echo what's in the cells with value blanking and geometry 
    {
      int i,j;
      float x=0.0,y=0.0;

      printf("variables = x,y,b,val\n");
      printf("zone i=%d, j= %d, f=point\n",visI+1,visJ+1);
      
      for(j=0;j<=visJ;j++) {
	x=0.0;
	for(i=0;i<=visI;i++) {
	  if((i<visI) && (j<visJ)) {
	    CELL *cell = Cell[nblk][i];
	    int nproc=cell[j].proc;
	    printf("%g %g %d %g \n",
		   x,y,
		   cell[j].proc,0.0);
	  } else 
	    printf("%g %g %d %g \n",
		   x,y,
		   0,0.0);
	  x+=Visdx[nblk][0][i];
	}
	y+= GVisdy[nblk][j];
      }
    }
    //-------------------------------------------
    printf("\n\nzone i=%d, j=%d,  F=block,d=(1,2,3)\n",visI+1,visJ+1);
    //--------------------------------
    // input value of one variable for all processors
    { 
      FILE *f = fopen("Vals.0.0","r");
      int n = Rectangle[nblk][nproc].n;
      if (n>0) {
	int vc;
	RECT *r = Rectangle[nblk][nproc].cells;	
	for (vc=0;vc<n;vc++) {	  
	  fscanf(f,"%f",&(r[vc].val));   
	}	
      }  
      fclose(f);
    }//loop over nprocs

    //-----------------------------------------
    // copy values, with interpolation or copy at blanked spaces

    {
      int i,j;
      for(j=0;j<=visJ;j++) {
	for(i=0;i<=visI;i++) {
	  float val;
	  if ( (i<visI) && (j< visJ)) {
	    CELL *cell = Cell[nblk][i];	
	    const int nproc=cell[j].proc;
	    const int nc=cell[j].nc;
	    if(nc>=0) 
	      val=Rectangle[nblk][nproc].cells[nc].val;
	    else 
	      val=ipol(nblk,i,j);

	    cell[j].val=val;

	  } else val =0.0;

	  printf("%g  ",val);	  
	}
	printf("\n");
      }
    }


  } // end loop over blocks


}


float ipol(int nblk,int i, int j)
{
  int ii=0,jj=0;float val=0.0;int found=0;
  
  // if i=0 then use first nonzero value in this row, if exists, 
  // otherwise use first nonzero value in the column, 
  // if this still not possible, exit with value 0.0

  if(i==0) {
    for(ii=1;ii<visI;ii++) {
      CELL *cell = Cell[nblk][ii];	
      const int nproc=cell[j].proc;
      const int nc=cell[j].nc;
      if(nc>=0) {
	val=Rectangle[nblk][nproc].cells[nc].val;

	printf("\nIPOL Found %d %d with val=%g\n",ii,j,val);

	found=1;
	break;
      }
    }
    if (found==0) {
      for(jj=0;jj<visJ;jj++) {
	CELL *cell = Cell[nblk][i];	
	const int nproc=cell[jj].proc;
	const int nc=cell[jj].nc;
	if(nc>=0) {
	  val=Rectangle[nblk][nproc].cells[nc].val;

	  printf("\nIPOL Found %d %d with val=%g\n",i,jj,val);

	  found=1;
	  break;
	}
      }
    }
    if(found==0) val=0.0;
    return val;
  }
  
  // easiest situation: i is not zero and we can  do
  // interpolation for this cell between i+1, i-1

  if(i<visI-1){
    const int i1=i-1;
    const int i2=i+1;
    CELL *cell1 = Cell[nblk][i1];	
    CELL *cell2 = Cell[nblk][i2];	
    const int nproc1=cell1[j].proc;
    const int nproc2=cell2[j].proc;
    const int nc1=cell1[j].nc;
    const int nc2=cell2[j].nc;
    if( (nc1>=0) && (nc2>=0) ) {
      float val1 =Rectangle[nblk][nproc1].cells[nc1].val;
      float val2 =Rectangle[nblk][nproc2].cells[nc2].val;
      val = (val1+val2) / 2.0;
      found=1;      

      printf("\nIPOL Found (%d,%d) %d with val=%g\n",i1,i2,j,val);

    }
  }
  if (found==1) return val;

  // if the above conditions were not satisfied,
  // use value for ii=i-1 (which must have been previously computed)
  
  {
    const int ii=i-1;
    CELL *cell = Cell[nblk][ii];	
    const int nproc=cell[j].proc;
    const int nc=cell[j].nc;
    if(nc>=0) 
      val=Rectangle[nblk][nproc].cells[nc].val;
    else 
      val=cell[j].val;

    printf("\nIPOL Found <-- %d,%d  with val=%g\n",ii,j,val);

    return val;
  }
  

}
