// tetcall.dcpp - Added the following for tesselation
// Added by Saumik, Aug 2016

/*
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <utility>
#include <cmath>
#include <cstdarg>
*/

#include "tetgen.h" // Defined tetgenio, tetrahedralize().

extern "C" {
void iptetgen_(double *volume, int *global, double *x, 
               double *y, double *z);
}

double tetvol(double *pt1, double *pt2, double *pt3, double *pt4);
void cross(double *a, double *b, double *c);
double dot(double *a, double *b);

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
void iptetgen_(double *volume, int *global, double *x, 
               double *y, double *z)
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
{

  void initialize(); /* Dana */

  tetgenio in, out;
  int i,j,v1,v2,v3,v4;
  double tetvolume,p1[3],p2[3],p3[3],p4[3];
  int check;  /* Dana */

  // All indices start from 1
  in.firstnumber = 1;

  *volume = 0.0; /* Initialize - Dana */

  in.numberofpoints = *global;
  
  in.pointlist = new REAL[in.numberofpoints * 3];

  // Set all nodes
  for (i = 0; i < in.numberofpoints; i++) 
  {
    in.pointlist[i * 3]     = *(x+i);
    in.pointlist[i * 3 + 1] = *(y+i);
    in.pointlist[i * 3 + 2] = *(z+i);
  }

  // Tetrahedralize

  check = 0;  /* Dana */

  tetrahedralize("", &in, &out, &check);

  if(check==1) { 
     *volume = 0.0;
  }  /* Dana */
  else
  {
  for (i = 0; i < out.numberoftetrahedra; i++) 
  {
    // v1,v2,v3,v4 are global node # of (i+1)th tetrahedron
    v1 = out.tetrahedronlist[i*out.numberofcorners+0]; 
    v2 = out.tetrahedronlist[i*out.numberofcorners+1];
    v3 = out.tetrahedronlist[i*out.numberofcorners+2];
    v4 = out.tetrahedronlist[i*out.numberofcorners+3];
    // p1,p2,p3,p4 are coordinates of corners of (i+1)th tetrahedron
    for (j = 0; j < 3; j++)
    {
      p1[j] = out.pointlist[(v1-1)*3+j];
      p2[j] = out.pointlist[(v2-1)*3+j];
      p3[j] = out.pointlist[(v3-1)*3+j];
      p4[j] = out.pointlist[(v4-1)*3+j];
    }
    // tetvolume is volume of (i+1)th tetrahedron  
    tetvolume = tetvol(p1,p2,p3,p4);
    // *vol is sum of all tetrahedron volumes
    *volume = *volume + tetvolume;
  }
  }

  void deinitialize(); /* Dana */

}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
double tetvol(double *pt1, double *pt2, double *pt3, double *pt4)
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
{
  int i;
  double volume,v1[3],v2[3],v3[3],crossprod[3];
  
  for (i = 0; i < 3; i++)
  {
    v1[i] = *(pt2+i) - *(pt1+i);
    v2[i] = *(pt3+i) - *(pt1+i);
    v3[i] = *(pt4+i) - *(pt1+i);
  }

  cross(v2,v3,crossprod);
  volume = dot(v1,crossprod);
  volume = volume/6.0;
  return volume;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
void cross(double *a, double *b, double *c)
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
{

  *(c+0) = *(a+1)*(*(b+2)) - *(a+2)*(*(b+1));
  *(c+1) = *(a+2)*(*(b+0)) - *(a+0)*(*(b+2));
  *(c+2) = *(a+0)*(*(b+1)) - *(a+1)*(*(b+0));

}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
double dot(double *a, double *b)
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
{
  int i;
  double dotprod;

  dotprod = 0.0;

  for (i = 0; i < 3; i++)
  {
    dotprod = dotprod + *(a+i)*(*(b+i));
  }
  
  return dotprod;
}
