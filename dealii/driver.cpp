#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cmath>
#include <mpi.h>
#include "ipars_dealii.h"
using namespace std;

int main ()
{
  int nerr = 0;
  int ndim = 2;
  int Nlev = 2;
  int TotSteps = 100;
  double Tend = 864000.;
  double xLow[3] = {0.0, 0.0, 0.0};
  double xHigh[3] = {1.0, 4.0, 4.0};
  int Ncoarse[3] = {1,2,2};
  const int NF = 16;
  int grid[NF][4];
  int Nele;
  double *perm, *pres;
  bool onceonly=true;
  int adaptivity = 1;  // 0=off, 1=on

  if (adaptivity==1) {
    const int NE = 4;
    Nele=NE;
    int cgrid[NE][4] = {
      {1,1,1,1},
      {1,2,1,1},
      {1,1,2,1},
      {1,2,2,1}};
    for(int e=0; e<Nele; e++) {
    for(int i=0; i<4; i++) {
      grid[e][i]=cgrid[e][i];
    }}
  }
  else {
    const int NE = 7;
    Nele=NE;
    int cgrid[NE][4] = {
      {1,1,1,1},
      {1,2,1,1},
      {1,1,2,1},
      {1,3,3,2},
      {1,4,3,2},
      {1,3,4,2},
      {1,4,4,2}};
    for(int e=0; e<Nele; e++) {
    for(int i=0; i<4; i++) {
      grid[e][i]=cgrid[e][i];
    }}
  }

  nerr = MPI_Init(NULL,NULL);
  if (nerr != 0) {
    cout << "Error" << nerr << "from MPI_Init" << endl;
    exit(1);
  }

  perm = (double*) malloc(NF*sizeof(double));
  fill(perm, perm+NF, 9.869233e-18);

  pres = (double*) malloc(NF*sizeof(double));
  fill(pres, pres+NF, 0.0e0);

  double deltim = Tend/TotSteps;
  double tim = 0.0;
  for (int nstep=0; nstep<TotSteps; nstep++) {

    if (onceonly) {
      ipars_start_(&nerr);
      ipars_sendgrid_(xLow,xHigh,&ndim,Ncoarse,&Nlev,&Nele,&grid[0][0],
        &adaptivity,&nerr);
      ipars_init_(&nerr);
      ipars_set_array_pointers_(perm,pres,&nerr);
      ipars_sendperm_(&nerr);
      ipars_sendpres_(&nerr);
      onceonly=false;
    }
    else if (adaptivity==1) {
      // Adapt the grid
      if ((nstep==2) || (nstep==6)) {
        const int NE = 7;
        Nele = NE;
        int cgrid[NE][4] = {
          {1,1,1,1},
          {1,2,1,1},
          {1,1,2,1},
          {1,3,3,2},
          {1,4,3,2},
          {1,3,4,2},
          {1,4,4,2}};
        for(int e=0; e<NF; e++) {
        for(int i=0; i<4; i++) {
          if (e<NE) grid[e][i]=cgrid[e][i];
          else grid[e][i]=0;
        }}
      }
      else if (nstep==4) {
        const int NE = 10;
        Nele = NE;
        int cgrid[NE][4] = {
          {1,1,1,1},
          {1,2,1,1},
          {1,1,3,2},
          {1,2,3,2},
          {1,1,4,2},
          {1,2,4,2},
          {1,3,3,2},
          {1,4,3,2},
          {1,3,4,2},
          {1,4,4,2}};
        for(int e=0; e<NF; e++) {
        for(int i=0; i<4; i++) {
          if (e<NE) grid[e][i]=cgrid[e][i];
          else grid[e][i]=0;
        }}
      }
      else if (nstep==8) {
        const int NE = 4;
        Nele = NE;
        int cgrid[NE][4] = {
          {1,1,1,1},
          {1,2,1,1},
          {1,1,2,1},
          {1,2,2,1}};
        for(int e=0; e<NF; e++) {
        for(int i=0; i<4; i++) {
          if (e<NE) grid[e][i]=cgrid[e][i];
          else grid[e][i]=0;
        }}
      }

      // Change the perm
      //fill(perm, perm+NF, 9.869233e-18*(1.0+(double)nstep/TotSteps));

      ipars_reinit_(&Nele,&grid[0][0],&nerr);
      ipars_sendperm_(&nerr);
    }

    ipars_solve_(&nstep,&tim,&deltim,&nerr);
    ipars_visualize_(&nerr);
    ipars_recvpres_(&nerr);
    tim += deltim;
  }
  ipars_finish_(&nerr);

  free(perm);
  free(pres);

  MPI_Finalize();
}
