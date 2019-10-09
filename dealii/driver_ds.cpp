#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <iostream>
#include <fstream>
#include <cmath>
#include <mpi.h>
#include <unistd.h>
#include "dataspaces.h"
using namespace std;
    
// initialize fine-scale "level" array to zeros
void zerolevel(int *Nfine)
{
  int zero=0;
  uint64_t coord[3];
  for(coord[2]=1; coord[2]<=Nfine[2]; coord[2]++) {
  for(coord[1]=1; coord[1]<=Nfine[1]; coord[1]++) {
  for(coord[0]=1; coord[0]<=Nfine[0]; coord[0]++) {
    dspaces_put("level", 0, sizeof(int), 3, coord, coord, &zero);
  }}}
}

int main ()
{
  int nerr = 0;
  int ndim = 2;
  int Nlev = 2;
  int TotSteps = 100;
  double Tend = 864000.;
  double xLow[3] = {0.0, 0.0, 0.0};
  double xHigh[3] = {1.0, 4.0, 4.0};
  int Ncoarse[3]={1,2,2};
  int Nfine[3];
  if (ndim==2) Nfine[0]=1;
  else Nfine[0]=int(Ncoarse[0]*pow(2,Nlev-1));
  Nfine[1]=int(Ncoarse[1]*pow(2,Nlev-1));
  Nfine[2]=int(Ncoarse[2]*pow(2,Nlev-1));
  const int NF = 16;
  int Nele;
  int grid[NF][4];
  double *perm, *pres;
  int size, rank;
  int appid=1;     // 1=dealii, 2=ipars
  MPI_Comm gcomm;
  uint64_t origin[3] = {0};
  FILE *gofile;

  int adaptivity = 1;  // 0=off, 1=on

  cout << "driver_ds: starting up" << endl;
  nerr = MPI_Init(NULL,NULL);
  if (nerr != 0) {
    cout << "Error: MPI_Init returned nerr=" << nerr << endl;
    exit(1);
  }
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  gcomm = MPI_COMM_WORLD;
  nerr = dspaces_init(size, appid, &gcomm, NULL);
  if (nerr != 0) {
    cout << "Error: dspaces_init returned nerr=" << nerr << endl;
    exit(1);
  }

  perm = (double*) malloc(NF*sizeof(double));
  fill(perm, perm+NF, 9.869233e-18);

  pres = (double*) malloc(NF*sizeof(double));
  fill(pres, pres+NF, 0.0e0);

  double deltim = Tend/TotSteps;
  double tim = 0.0;
  int done = 0;
  bool onceonly = true;

  //fprintf(stderr, "driver: acquiring inital status lock\n");
  dspaces_lock_on_write("status", &gcomm);
  //fprintf(stderr, "Got write lock on status.\n");
  
  // Write initial scalar data to dataspaces
  if (rank==0) {
    dspaces_put("xlow", 0, 3*sizeof(double), 3, origin, origin, &xLow);
    dspaces_put("xhigh", 0, 3*sizeof(double), 3, origin, origin, &xHigh);
    dspaces_put("ndim", 0, sizeof(int), 3, origin, origin, &ndim);
    dspaces_put("ncoarse", 0, 3*sizeof(int), 3, origin, origin, &Ncoarse);
    dspaces_put("nlev", 0, sizeof(int), 3, origin, origin, &Nlev);
    dspaces_put("adaptivity", 0, sizeof(int), 3, origin, origin, &adaptivity);
  }

  // Set initial grid
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

  dspaces_put("done", 0, sizeof(int), 3, origin, origin, &done);

  // Loop over time steps
  for (int nstep=0; nstep<TotSteps; nstep++) {
    cout << "driver_ds: nstep=" << nstep << endl;

    // Write transient scalar data to dataspaces
    if (rank==0) {
      dspaces_put("tim", 0, sizeof(double), 3, origin, origin, &tim);
      dspaces_put("deltim", 0, sizeof(double), 3, origin, origin, &deltim);
      dspaces_put("nstep", 0, sizeof(int), 3, origin, origin, &nstep);
    }

    if (onceonly) {
    // Initial conditions

      for(int e=0; e<Nele; e++) {
        int i=grid[e][0];
        int j=grid[e][1];
        int k=grid[e][2];
        int l=grid[e][3];
        uint64_t coord[3];
        int stride = int(pow(2,Nlev-l));
        coord[0] = (i-1)*stride + 1;
        coord[1] = (j-1)*stride + 1;
        coord[2] = (k-1)*stride + 1;
        //cout << "driver_ds: coord=" << coord[0] << "," << coord[1] << ","
        //     << coord[2] << ", lev=" << l << endl;
        dspaces_put("level", 0, sizeof(int), 3, coord, coord, &l);
        dspaces_put("perm", 0, sizeof(double), 3, coord, coord, &perm[e]);
        dspaces_put("pres", 0, sizeof(double), 3, coord, coord, &pres[e]);
      }

      // Write empty file to disk to tell ipars to start
      if (rank==0) {
        char *gofilename = "iparsgo";
        gofile = fopen(gofilename, "w");
        fclose(gofile);
        while(access(gofilename, F_OK) != -1) usleep(10);
      }
      onceonly = false;
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
  
      // Write transient array data to dataspaces
      zerolevel(Nfine);
      for(int e=0; e<Nele; e++) {
        int i=grid[e][0];
        int j=grid[e][1];
        int k=grid[e][2];
        int l=grid[e][3];
        uint64_t coord[3];
        int stride = int(pow(2,Nlev-l));
        coord[0] = (i-1)*stride + 1;
        coord[1] = (j-1)*stride + 1;
        coord[2] = (k-1)*stride + 1;
        dspaces_put("level", 0, sizeof(int), 3, coord, coord, &l);
        dspaces_put("perm", 0, sizeof(double), 3, coord, coord, &perm[e]);
      }
    }

    //fprintf(stderr, "driver: releasing status lock\n");
    dspaces_put_sync();
    dspaces_unlock_on_write("status", &gcomm);
    //***********************************************
    // IPARS will work after releasing status lock
    //***********************************************

    //fprintf(stderr, "driver: acquiring status lock\n");
    dspaces_lock_on_write("status", &gcomm);   
    //***********************************************
    // Read ipars results after acquiring status lock
    //***********************************************

    MPI_Barrier(gcomm);
    tim += deltim;
  }

  // Tell IPARS to terminate
  done = 1;
  if (rank==0) {
    dspaces_put("done", 0, sizeof(int), 3, origin, origin, &done);
  }
  //fprintf(stderr, "driver: releasing data write lock\n");
  dspaces_unlock_on_write("status", &gcomm);

  // Clean up
  free(perm);
  free(pres);
  dspaces_finalize();
  MPI_Finalize();
  cout << "driver_ds: shutting down" << endl;
}

