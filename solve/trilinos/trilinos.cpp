/********************************************************************************/
/* trilinos.cpp
   This is a Trilinos wrapper program.
   Created By:   Gergina Pencheva
                 Ben Ganis
                 Chris Seifert
   Last Updated: 09/11/2012
*/
/********************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <iostream>
#include <fstream>

using namespace std;

#include "Teuchos_RCPDecl.hpp"
#include "Teuchos_ArrayRCPDecl.hpp"
#include "Epetra_CrsMatrix.h"
#include "Epetra_CrsGraph.h"
#include "Epetra_Vector.h"
#include "AztecOO.h"
#include "ml_MultiLevelPreconditioner.h"
#include "Teuchos_ConfigDefs.hpp"
#include "Teuchos_FileInputSource.hpp"
#include "Teuchos_XMLObject.hpp"
#include "Teuchos_XMLParameterListReader.hpp"
#include "EpetraExt_RowMatrixOut.h"
#include "EpetraExt_VectorOut.h"

#ifdef HAVE_MPI
#include "mpi.h"
#include "Epetra_MpiComm.h"
#else
#include "Epetra_SerialComm.h"
#endif

using Teuchos::RCP;
using Teuchos::rcp;
using Teuchos::ArrayRCP;

const int one = 1;
const bool DBG = false;

// gp dbg
int _Nstep;  // time step, setup externally from Fortran
int _Newt;   // Newton step, setup externally from Fortran 

extern "C" {
  
void trilinos_copy_vars_(const int *nstep, const int *newt) { 
   _Nstep = *nstep;
   _Newt  = *newt;
}
  
} // extern C
// gp dbg

// Function prototypes
extern "C" {

#ifndef UNIT_TEST
  void mycallwork_(void* sub, int* args);
#endif

  // Call to setup the Trilinos solvers.
  void trilinos_setup_solver_();

  // Call to cleanup when all is done.
  void trilinos_cleanup_();

  void trilinos_ipars_init1_ex_(
    const int *N_I4U,
    const int *N_I4U2,
    const int *N_COLGIDS);

  void trilinos_ipars_init2_ex_(
    const int *N_I4U,
    const int *N_I4U2,
    const int *N_COLGIDS);

  void trilinos_ipars_fill_ex_(
    const int *N_I4U,
    const int *N_I4U2,
    const int *N_COF,
    const int *N_RESID);

  void trilinos_ipars_solve_ex_(
    const int *N_I4U2,
    const int *N_DUNK,
    const int *N_I4U);

  void trilinos_ipars_init1(
    const int *IDIM,
    const int *JDIM,
    const int *KDIM,
    const int *LDIM,
    const int *IL1,
    const int *IL2,
    const int *JLV1,
    const int *JLV2,
    const int *KL1,
    const int *KL2,
    const int *KEYOUT,
    const int *NBLK,
    const int *NBLK2,
    const int *NUMEQ,
    int *ColGids);
  
  void trilinos_ipars_init2(
    const int *IDIM,
    const int *JDIM,
    const int *KDIM,
    const int *LDIM,
    const int *IL1,
    const int *IL2,
    const int *JLV1,
    const int *JLV2,
    const int *KL1,
    const int *KL2,
    const int *KEYOUT,
    const int *NBLK,
    const int *NBLK2,
    const int *NUMEQ,
    const int *ColGids);

  void trilinos_ipars_fill_matrix_and_rhs(
    const int *IDIM,
    const int *JDIM,
    const int *KDIM,
    const int *LDIM,
    const int *IL1,
    const int *IL2,
    const int *JLV1,
    const int *JLV2,
    const int *KL1,
    const int *KL2,
    const int *KEYOUT,
    const int *NBLK,
    const int *NUMEQ,
    const int *TEMPLATE,
    float *COF,
    double *RESID);

  void trilinos_ipars_solve(
    const int *IDIM,
    const int *JDIM,
    const int *KDIM,
    const int *LDIM,
    const int *IL1,
    const int *IL2,
    const int *JLV1,
    const int *JLV2,
    const int *KL1,
    const int *KL2,
    const int *KEYOUT,
    const int *NBLK,
    const int *NUMEQ,
    double *DUNK,
    int *ITLIN);

  void print_array_int_withghosts(
    const char *aname,
    int *array,
    const int *IDIM,
    const int *JDIM,
    const int *KDIM,
    const int *LDIM = &one);

  void print_array_int(
    const char *aname,
    int *array, 
    const int *IL1,
    const int *IL2,
    const int *JLV1,
    const int *JLV2,
    const int *KL1,
    const int *KL2,
    const int *KEYOUT,
    const int *IDIM,
    const int *JDIM, 
    const int *KDIM,
    const int *LDIM = &one);

   void print_array_int_glob(
     const char *aname,
     int *array,
     const int *NX,
     const int *NY,
     const int *NZ,
     const int *IOFF,
     const int *JOFF,
     const int *KOFF,
     const int *KEYOUT,
     const int *IDIM,
     const int *JDIM,
     const int *KDIM,
     const int *LDIM = &one);
   
   void print_array_double_withghosts(
     const char *aname,
     double *array,
     const int *IDIM,
     const int *JDIM,
     const int *KDIM,
     const int *LDIM);
   
   void print_array_double(
     const int *IDIM,
     const int *JDIM,
     const int *KDIM,
     const int *IL1,
     const int *IL2,
     const int *JLV1,
     const int *JLV2,
     const int *KL1,
     const int *KL2,
     const int *KEYOUT,
     const char *aname,
     double *array);
  
   void print_array_double_glob(
     const int *NX,
     const int *NY,
     const int *NZ,
     const int *IOFF,
     const int *JOFF,
     const int *KOFF,
     const int *IDIM,
     const int *JDIM, 
     const int *KDIM,
     const int *KEYOUT,
     const char *aname,
     double *array); 
 
   void print_array_float_withghosts(
     const char *aname,
     float *array,
     const int *IDIM,
     const int *JDIM,
     const int *KDIM,
     const int *LDIM);
   
   void print_array_float(
     const int *IDIM,
     const int *JDIM,
     const int *KDIM,
     const int *IL1,
     const int *IL2,
     const int *JLV1,
     const int *JLV2,
     const int *KL1,
     const int *KL2,
     const int *KEYOUT,
     const char *aname,
     float *array);

   void print_array_float_glob(
     const int *NX,
     const int *NY,
     const int *NZ,
     const int *IOFF,
     const int *JOFF,
     const int *KOFF,
     const int *IDIM,
     const int *JDIM, 
     const int *KDIM,
     const int *KEYOUT,
     const char *aname,
     float *array); 

}// extern "C"

// Static variables, used to the wrapper can keep hold of it's state between calls.
static RCP<Epetra_Comm> Comm_;
static RCP<Epetra_Map> RowMap_;
static RCP<Epetra_CrsMatrix> A_;
static RCP<Epetra_CrsGraph> Graph_; 
static RCP<Epetra_Vector> lhs_;
static RCP<Epetra_Vector> rhs_;
static RCP<ML_Epetra::MultiLevelPreconditioner> Preconditioner_;
static RCP<AztecOO> Solver_;
static RCP<ofstream> log_;
static ArrayRCP<int> ColGids_;
static bool first_time = true;
static bool use_ml = false;

Teuchos::ParameterList Params;
Teuchos::ParameterList SolverParams;
Teuchos::ParameterList PrecondParams;
Teuchos::ParameterList ConvParams;

extern "C"{

/***********************************************************************/
/***********************************************************************/

// Call to setup the Trilinos solvers.
void trilinos_setup_solver_(){
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  //if (rank==0) printf("In trilinos_setup_solver_\n");

  // Create the solver object
  Solver_ = rcp(new AztecOO(&*A_,&*lhs_,&*rhs_));
  Solver_->SetParameters(SolverParams);

  // redirect the solver output to a log file
  Solver_->SetOutputStream(*log_);

  if (use_ml)
  {
    // Create the preconditioner object
    Preconditioner_ = rcp(new ML_Epetra::MultiLevelPreconditioner(*A_,PrecondParams));
    //if (rank==0) printf("After allocating ML Preconditioner\n");

    // get ML complexity
    int nnz = A_->NumGlobalNonzeros();
    double complexity = Preconditioner_->GetML_Aggregate()->operator_complexity/nnz;
    if (rank==0) *log_<< "MG Operator Complexity = " << complexity << endl;
 
   // Link AztecOO solver to ML preconditioner
    Solver_->SetPrecOperator(&*Preconditioner_);
  }

}

/***********************************************************************/
/***********************************************************************/

// Call to cleanup when all is done.
void trilinos_cleanup_(){
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  //if(!Comm_->MyPID())
  //
  //printf("Trilinos: trilinos_cleanup called on Node %d\n",rank);
  Preconditioner_ = Teuchos::null;
  Solver_ = Teuchos::null;
  Graph_  = Teuchos::null;
  A_      = Teuchos::null;
  lhs_    = Teuchos::null;
  rhs_    = Teuchos::null;
  RowMap_ = Teuchos::null;
  Comm_   = Teuchos::null;
  log_    = Teuchos::null;
}

/***********************************************************************/
/***********************************************************************/

#ifndef UNIT_TEST
void trilinos_ipars_init1_ex_(const int *N_I4U,const int *N_I4U2, 
                              const int *N_COLGIDS) {
   int callworkdata[4];
   callworkdata[0] = 3;
   callworkdata[1] = *N_I4U;
   callworkdata[2] = *N_I4U2;
   callworkdata[3] = *N_COLGIDS;
   mycallwork_((void*)trilinos_ipars_init1, callworkdata);
}
#endif

/***********************************************************************/
/***********************************************************************/

void trilinos_ipars_init1(
  const int *IDIM,
  const int *JDIM,
  const int *KDIM,
  const int *LDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const int *NBLK,
  const int *NBLK2,
  const int *NUMEQ,
  int *ColGids) {

  // do separate callworks for each block, so we can have
  // numeq on the c++ level
  if (*NBLK != *NBLK2) return;

  // initialize ColGids to -1
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = 1; K <= *KDIM ; K++) {
     for (int J = 1; J <= *JDIM; J++) {
        for (int I = 1; I <= *IDIM; I++) {
             int IPARS_Index = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM)) +
                               (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
          ColGids[IPARS_Index] = -1;
        }
     }
  }
  }

  /* gp dbg
  print_array_int_withghosts("keyout_init1_1",(int*)KEYOUT,IDIM,JDIM,KDIM);
  const int N123 = (*IDIM)*(*JDIM)*(*KDIM);
  printf("init1 starts, KEYOUT: [ %p , %p ], DUNK: [ %p , %p ]\n",
         KEYOUT,KEYOUT+N123,ColGids,ColGids+N123);
  print_array_int_withghosts("colgids_init1_1",ColGids,IDIM,JDIM,KDIM,NUMEQ);
  */
 // Count number of local active elements
 int N = 0;
 for (int K = *KL1; K <= *KL2 ; K++) {
    for (int J = JLV1[K-1]; J <= JLV2[K-1]; J++) {
       for (int I = *IL1; I <= *IL2; I++) {
          int IPARS_Index = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
          if (KEYOUT[IPARS_Index]==1) N++;
       }
    }
 }
 N *= (*NUMEQ);  // Multiply grid blocks by phases to get dof's

  // gp dbg
  //print_array_int_withghosts("keyout_init1_2",(int*)KEYOUT,IDIM,JDIM,KDIM);

 // log file for the solver
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);

  //printf("in trilinos_ipars_init1, prc=%i, #local active elm=%i\n",rank,N);

  char logname[20];
  //sprintf(logname,"azteclog.%4.4d",rank);
  sprintf(logname,"aztec.log");
  log_ = rcp(new ofstream(logname));

  // Read parameters from an XML file,
  // with sublists "AztecOO", "ML", and "Convergence".
  
  struct stat buffer;
  char fileName[20] = "trilinos.xml";
  int status = stat(fileName, &buffer);

  // gp dbg
  if(!rank){
    if (status==0) {
      printf("\ntrilinos: Parameters file %s found\n",fileName);
    }
    else {
      printf("\ntrilinos: No parameters file %s found\n",fileName);
    }
  }
  // gp dbg

  if (status==0) {
    Teuchos::FileInputSource fileSrc(fileName);
    Teuchos::XMLObject fileXML = fileSrc.getObject();
    Teuchos::XMLParameterListReader ListReader;
    Params= ListReader.toParameterList(fileXML);
  }

  SolverParams  = Params.sublist("AztecOO");
  ConvParams    = Params.sublist("Convergence");

  // Set AztecOO default parameters

  if (!SolverParams.isParameter("AZ_solver")) 
    SolverParams.set("AZ_solver","AZ_gmres");
  if (!SolverParams.isParameter("AZ_conv")) 
    SolverParams.set("AZ_conv","AZ_r0");

// TO DO: READ THESE FROM IPARS 
  if (!SolverParams.isParameter("AZ_kspace")) 
    SolverParams.set("AZ_kspace",20);
  if (!ConvParams.isParameter("max iterations"))
     ConvParams.set("max iterations",1000); 
  if (!ConvParams.isParameter("tolerance"))
     ConvParams.set("tolerance",5e-6);

  // Use ML preconditioner if it appears in XML file
  use_ml = Params.isParameter("ML");

  if (use_ml) 
  {
    PrecondParams = Params.sublist("ML");
  
    if (!PrecondParams.isParameter("aggregation: threshold")) 
      PrecondParams.set("aggregation: threshold",0.001);
    if (!PrecondParams.isParameter("ML output"))  // ML output verbosity
      PrecondParams.set("ML output",0);
    if (!PrecondParams.isParameter("smoother: sweeps"))
      PrecondParams.set("smoother: sweeps",1); // try if # iter increases substantially
  
    // Set Defaults for ML (whatever the XML file didn't override)
    ML_Epetra::SetDefaults("SA",PrecondParams,0,0,false);
  }

  if(rank==0){
    /*
    cout<<"*** AztecOO SolverParams ***"<<endl<<SolverParams<<endl;
    cout<<"*** Convergence ConvParams ***"<<endl<<ConvParams<<endl;
    if (use_ml) cout<<"*** ML PrecondParams ***"<<endl<<PrecondParams<<endl;
    */
    *log_<<"*** AztecOO SolverParams ***"<<endl<<SolverParams<<endl;
    *log_<<"*** Convergence ConvParams ***"<<endl<<ConvParams<<endl<<endl;
    if (use_ml) *log_<<"*** ML PrecondParams ***"<<endl<<PrecondParams<<endl;
  } 



#ifdef HAVE_MPI
  Comm_=rcp(new Epetra_MpiComm(MPI_COMM_WORLD));
#else
  Comm_=rcp(new Epetra_SerialComm());
#endif
  RowMap_=rcp(new Epetra_Map(-1,N,0,*Comm_));
  int *MyGids = (int *) malloc(sizeof(int)* N);
  RowMap_->MyGlobalElements(MyGids);

  /* gp dbg
  char fname[20];
  sprintf(fname,"Gid_Pr%d.dat",rank);
  FILE *fid = fopen(fname,"w");
  for(int i=0; i<N; i++) {fprintf(fid,"%i  %i\n",i,MyGids[i]);}
  fclose(fid);
  */

  Graph_=rcp(new Epetra_CrsGraph(Copy,*RowMap_,0));
  //A_=rcp(new Epetra_CrsMatrix(Copy,*RowMap_,0));
  lhs_=rcp(new Epetra_Vector(*RowMap_));
  rhs_=rcp(new Epetra_Vector(*RowMap_));

  /* gp dbg
  print_array_int_withghosts("keyout_init1_3",(int*)KEYOUT,IDIM,JDIM,KDIM);
  printf("Before fill ColGids, 146->%1d, 149->%1d, 170->%1d, 173->%1d\n",
         KEYOUT[146],KEYOUT[149],KEYOUT[170],KEYOUT[173]);
  for (int i=0; i<(*IDIM)*(*JDIM)*(*KDIM); i++) {
    printf("%d ",KEYOUT[i]);
    if ( (i+1) % 20 == 0) printf("\n");
  }
  printf("\n");
  */

  // Fill ColGids
  int NLocal = 0;
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = *KL1; K <= *KL2 ; K++) {
       for (int J = JLV1[K-1]; J <= JLV2[K-1]; J++) {
         for (int I = *IL1; I <= *IL2; I++) {
           int IPARS_Index_cell = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
           int IPARS_Index_dof = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM)) +
                                 (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
           /* gp dbg
           printf("I=%2d,J=%2d,K=%2d,PH=%2d,idx_cell=%3d,idx_dof=%3d,keyout=%1d\n",
                  I,J,K,IPH,IPARS_Index_cell,IPARS_Index_dof,KEYOUT[IPARS_Index_cell]);
           */
           if (KEYOUT[IPARS_Index_cell] == 1) {
              ColGids[IPARS_Index_dof] = MyGids[NLocal++];
              /*
              int colgid = (int) ColGids[IPARS_Index_dof];
              //printf("in init1, rank=%i, setting dunk(%i) to %lf\n",
              //       rank,IPARS_Index,ColGids[IPARS_Index]);
              printf("in init1, rank=%i, setting ColGids(%i) to %d\n",
                     rank,IPARS_Index_dof,colgid);
              for (int i=0; i<(*IDIM)*(*JDIM)*(*KDIM); i++) {
                printf("%d ",KEYOUT[i]);
                if ( (i+1) % 20 == 0) printf("\n");
              }
              printf("\n");
              */
           }
         }
       }
  }
  }

  free(MyGids);

  /* gp dbg
  print_array_int_withghosts("keyout",(int*)KEYOUT,IDIM,JDIM,KDIM);
  print_array_int_withghosts("colgids_init1",ColGids,IDIM,JDIM,KDIM,NUMEQ);
  */

}

/***********************************************************************/
/***********************************************************************/

#ifndef UNIT_TEST
void trilinos_ipars_init2_ex_(const int *N_I4U, const int *N_I4U2,
                              const int *N_COLGIDS) {
   int callworkdata[4];
   callworkdata[0] = 3;
   callworkdata[1] = *N_I4U;
   callworkdata[2] = *N_I4U2;
   callworkdata[3] = *N_COLGIDS;
   mycallwork_((void*)trilinos_ipars_init2, callworkdata);
}
#endif

/***********************************************************************/
/***********************************************************************/

void trilinos_ipars_init2(
  const int *IDIM,
  const int *JDIM,
  const int *KDIM,
  const int *LDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const int *NBLK,
  const int *NBLK2,
  const int *NUMEQ,
  const int *ColGids) {

  // do separate callworks for each block, so we can have
  // numeq on the c++ level
  if (*NBLK != *NBLK2) return;

  //print_array_int_withghosts("colgids_init2",(int *)ColGids,IDIM,JDIM,KDIM,NUMEQ);

  // Count number of local active dofs plus neighboring active dofs
  int NumColGids = 0;
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = 1; K <= *KDIM ; K++) {
     for (int J = 1; J <= *JDIM; J++) {
        for (int I = 1; I <= *IDIM; I++) {
           int IPARS_Index_dof = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM)) +
                                 (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
           if (ColGids[IPARS_Index_dof]>=0) NumColGids++;
        }
        }
     }
  }
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  //printf("In trilinos_ipars_init2, prc=%d, NumColGids=%d\n",rank,NumColGids);
  //print_array_double_withghosts("ColGids",ColGids,IDIM,JDIM,KDIM);

  // Copy and save the ColGids vector
  int N1234 = (*IDIM)*(*JDIM)*(*KDIM)*(*NUMEQ);
  ColGids_.resize(N1234);
  for (int i=0; i<N1234; i++) ColGids_[i]=ColGids[i];
}

/***********************************************************************/
/***********************************************************************/

#ifndef UNIT_TEST
void trilinos_ipars_fill_ex_(const int *N_I4U, const int *N_I4U2, const int *N_COF, 
                             const int *N_RESID) {
   int callworkdata[5];
   callworkdata[0] = 4;
   callworkdata[1] = *N_I4U;
   callworkdata[2] = *N_I4U2;
   callworkdata[3] = *N_COF;
   callworkdata[4] = *N_RESID;
   mycallwork_((void*)trilinos_ipars_fill_matrix_and_rhs, callworkdata);
}
#endif


/***********************************************************************/
/***********************************************************************/

// Call to fill the matrix.  Pass in rowptr/colind/values in classic CSR format
void trilinos_ipars_fill_matrix_and_rhs(
  const int *IDIM,
  const int *JDIM,
  const int *KDIM,
  const int *LDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const int *NBLK,
  const int *NUMEQ,
  const int *TEMPLATE,
  float *COF,
  double *RESID)
{
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);

  //if(!Comm_->MyPID())
  //  printf("Trilinos: trilinos_fill called on Node %d\n",rank);

  int NCofsPerRow;
  if (*TEMPLATE==1) { NCofsPerRow=7; }              // To implement the others, see solve/ygmres/ticama.df.
  //else if (*TEMPLATE==2) { NCofsPerRow=19; }
  //else if (*TEMPLATE==3) { NCofsPerRow=27; }
  else { cerr << "Unknown template in trilinos_ipars_fill_matrix_and_rhs\n"; exit(-1); }

  //if(!Comm_->MyPID())
  //  printf("Trilinos: trilinos_fill_matrix called on Node %d\n",rank);
  // This example assumes that we're passing in a complete CSR matrix.
  // That might be a little unrealistic.
  //
  // The basic idea here is that we fill in the Epetra matrix row by row,
  // using GLOBAL indices passed in from colind.  In serial, these
  // are the same as the local ones, but in parallel, this is important
  // to get correct.
  //  
  // Modify this fill in routine to work for your matrix storage.
  int IPARS_CellRowIndex;
  int IPARS_RowIndex;
  int IPARS_CellColIndex;
  int IPARS_ColIndex;
  int IPARS_COFIndex;
  int count;

  int N=RowMap_->NumMyElements();
  int row;
  int    *cols = (int    *) malloc(sizeof(int   )* (NCofsPerRow+1) * (*NUMEQ));
  double *vals = (double *) malloc(sizeof(double)* (NCofsPerRow+1) * (*NUMEQ));
  int i,j,k;

  //printf("NUMEQ=%d\n",*NUMEQ);

  // Fill in the graph

  if (first_time) {
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = *KL1; K <= *KL2 ; K++) {
       for (int J = JLV1[K-1]; J <= JLV2[K-1]; J++) {
         for (int I = *IL1; I <= *IL2 ; I++) {
            IPARS_CellRowIndex = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
            IPARS_RowIndex = IPARS_CellRowIndex + (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
            if (KEYOUT[IPARS_CellRowIndex]==1) {  
               count = 0;
               row = ColGids_[IPARS_RowIndex];
               for (int JPH = 1; JPH <= *NUMEQ ; JPH++) {
               for (int kk = 0; kk < NCofsPerRow; kk++) {
   
// Warning: This is only for 7-point stencil! 
                  switch( kk ) {
                     case 0:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 1:
                        IPARS_CellColIndex = (I-2) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 2:
                        IPARS_CellColIndex = (I  ) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 3:
                        IPARS_CellColIndex = (I-1) + (J-2)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 4:
                        IPARS_CellColIndex = (I-1) + (J  )*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 5:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K-2)*((*IDIM)*(*JDIM));
                        break;
                     case 6:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K  )*((*IDIM)*(*JDIM));
                        break;
                     default:
                        printf("Only set up for 7 coefficients\n");
                        exit(1);
                  }
                  IPARS_ColIndex = IPARS_CellColIndex + (JPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
                  if (KEYOUT[IPARS_CellColIndex] !=0) {
                     IPARS_COFIndex = IPARS_CellRowIndex + (*IDIM)*(*JDIM)*(*KDIM)*kk 
                                    + (*IDIM)*(*JDIM)*(*KDIM)*NCofsPerRow*(IPH-1)
                                    + (*IDIM)*(*JDIM)*(*KDIM)*NCofsPerRow*(*NUMEQ)*(JPH-1);
                     //if (abs(COF[IPARS_COFIndex]) > 1e-20)
                     {
                       cols[count  ] = ColGids_[IPARS_ColIndex];
                       count++;
                     }
                     /* gp dbg
                     printf("local IJK=(%d,%d,%d), CELL ROW=%d COL=%d\n",I,J,K,
                            IPARS_CellRowIndex,IPARS_CellColIndex);
                     printf("IPH=%d JPH=%d, ",IPH,JPH);
                     printf("ROW=%d COL=%d, ",IPARS_RowIndex,IPARS_ColIndex);
                     printf("kk=%d, count=%d, colgids=%d, ",kk,count, cols[count-1]);
                     printf("COFIdx=%d, COF=%f \n\n",IPARS_COFIndex, COF[IPARS_COFIndex]);
                     */
                  }
               }
               }
               Graph_->InsertGlobalIndices(row,count,cols);
      
               }
               }
            }
         }
     }
    
     Graph_->FillComplete();
     A_=rcp(new Epetra_CrsMatrix(Copy,*Graph_));
     first_time=false;   
  }
  //EpetraExt::RowMatrixToMatlabFile("matrix1.out",*A_);

  //printf("Done filling graph\n");
  // Fill the matrix and RHS entries
 
  A_->PutScalar(0.0);
  rhs_->PutScalar(0.0);
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = *KL1; K <= *KL2 ; K++) {
       for (int J = JLV1[K-1]; J <= JLV2[K-1]; J++) {
         for (int I = *IL1; I <= *IL2 ; I++) {
            IPARS_CellRowIndex = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
            IPARS_RowIndex = IPARS_CellRowIndex + (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
            if (KEYOUT[IPARS_CellRowIndex]==1) {  
               count = 0;
               row = ColGids_[IPARS_RowIndex];
               for (int JPH = 1; JPH <= *NUMEQ ; JPH++) {
               for (int kk = 0; kk < NCofsPerRow; kk++) {
  
// Warning: This is only for 7-point stencil! 
                  switch( kk ) {
                     case 0:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 1:
                        IPARS_CellColIndex = (I-2) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 2:
                        IPARS_CellColIndex = (I  ) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 3:
                        IPARS_CellColIndex = (I-1) + (J-2)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 4:
                        IPARS_CellColIndex = (I-1) + (J  )*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
                        break;
                     case 5:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K-2)*((*IDIM)*(*JDIM));
                        break;
                     case 6:
                        IPARS_CellColIndex = (I-1) + (J-1)*(*IDIM) + (K  )*((*IDIM)*(*JDIM));
                        break;
                     default:
                        printf("Only set up for 7 coefficients\n");
                        exit(1);
                  }
                  IPARS_ColIndex = IPARS_CellColIndex + (JPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
                  if (KEYOUT[IPARS_CellColIndex] !=0) {
                     IPARS_COFIndex = IPARS_CellRowIndex + (*IDIM)*(*JDIM)*(*KDIM)*kk 
                                    + (*IDIM)*(*JDIM)*(*KDIM)*NCofsPerRow*(IPH-1)
                                    + (*IDIM)*(*JDIM)*(*KDIM)*NCofsPerRow*(*NUMEQ)*(JPH-1);
                     //if (abs(COF[IPARS_COFIndex]) > 1e-20)
                     {
                       cols[count  ] = ColGids_[IPARS_ColIndex];
                       vals[count] = COF[IPARS_COFIndex];
                       count++;
            }
         }
      }
  }
               A_->SumIntoGlobalValues(row,count,vals,cols);
               rhs_->ReplaceGlobalValues(1,&RESID[IPARS_RowIndex],&row);
               }
            }
         }
     }
     }
  //printf("Before fillcomplete\n");
  A_->FillComplete();

// gp dbg
  if (DBG) {
     char matrixfilename[50],rhsfilename[50];
     sprintf(matrixfilename,"A_s%d_n%d",_Nstep,_Newt);
     sprintf(rhsfilename,"b_s%d_n%d",_Nstep,_Newt);
     printf("output trilinos matrix in %s\n",matrixfilename);
     EpetraExt::RowMatrixToMatlabFile(matrixfilename,*A_);
     printf("output trilinos RHS in %s\n",rhsfilename);
     EpetraExt::VectorToMatrixMarketFile(rhsfilename,*rhs_);
     //cin.get();
  }
// gp dbg


  /* gp dbg
  print_array_double(IDIM,JDIM,KDIM,IL1,IL2,JLV1,JLV2,KL1,KL2,
                  KEYOUT,"resid",RESID);
  print_array_double_withghosts("resid",RESID,IDIM,JDIM,KDIM);
  print_array_int_withghosts("keyout",(int*)KEYOUT,IDIM,JDIM,KDIM);
  */
  // row map
  //Epetra_Map rowmap(A_->RowMap());
  /*
  if (rank==0) printf("\n.......... RowMap: starts ..........\n"); 
  //rowmap.Print(cout); 
  //(A_->RowMap()).Print(cout);
  RowMap_->Print(cout); 
  if (rank==0) printf("\n.......... RowMap: ends ..........\n\n"); 
  // col map
  if (rank==0) printf("\n.......... ColMap: starts ..........\n"); 
  (A_->ColMap()).Print(cout); 
  if (rank==0) printf("\n.......... ColMap: ends ..........\n\n"); 
  */
  /* // In our case, both DomainMap and RangeMap match RowMap
  // domain map
  if (rank==0) printf("\n.......... DomainMap: starts ..........\n"); 
  (A_->DomainMap()).Print(cout); 
  if (rank==0) printf("\n.......... DomainMap: ends ..........\n\n"); 
  // range map
  if (rank==0) printf("\n.......... RangeMap: starts ..........\n"); 
  (A_->RangeMap()).Print(cout); 
  if (rank==0) printf("\n.......... RangeMap: ends ..........\n\n"); 
  */

  //if(!Comm_->MyPID())
  //  printf("Trilinos: trilinos_fill finished on Node %d\n",rank);
}

/***********************************************************************/
/***********************************************************************/

#ifndef UNIT_TEST
void trilinos_ipars_solve_ex_(const int *N_I4U2, const int *N_DUNK, const int *N_I4U) {
   int callworkdata[4];
   callworkdata[0] = 3;
   callworkdata[1] = *N_I4U2;
   callworkdata[2] = *N_DUNK;
   callworkdata[3] = *N_I4U;
   mycallwork_((void*)trilinos_ipars_solve, callworkdata);
}
#endif

/***********************************************************************/
/***********************************************************************/

// Call to solve the problem, passing in rhs and lhs vectors
void trilinos_ipars_solve(
  const int *IDIM,
  const int *JDIM,
  const int *KDIM,
  const int *LDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const int *NBLK,
  const int *NUMEQ,
  double *DUNK,
  int *ITLIN)
{
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  //if(!Comm_->MyPID())
  //  printf("Trilinos: trilinos_solve called on Node %d\n",rank);
  int N=RowMap_->NumMyElements();

  // Get Params
  //Teuchos::ParameterList & ConvParams=Params_.sublist("Convergence");
  int MaxIters = ConvParams.get("max iterations",200); // MAKE CONSISTENT TO IPARS
  //double Tolerance = ConvParams.get("tolerance",1e-8);
  double Tolerance = ConvParams.get("tolerance",5e-6);  // IPARS default

// gp dbg if (!Comm_->MyPID()) {cout << "In trilinos_solve: MaxIters=" << MaxIters << ", Tolerance=" << Tolerance << endl; }
  //*
  if (Solver_->GetAllAztecOptions()[AZ_conv] == AZ_noscaled) {
    double initres;
    rhs_->Norm2(&initres);
    Tolerance *= initres;
    //if (rank==0) printf("Initial residual %12.6e, abs. tol=%22.15e\n",initres,Tolerance);
  }
  else {
    //if (rank==0) printf("Rel. tol=%22.15e\n",Tolerance);
  }

  lhs_->PutScalar(0.0);

  // solve
  Solver_->Iterate(MaxIters,Tolerance);

  //EpetraExt::VectorToMatrixMarketFile("lhs.out",*lhs_);

  // get # iterations
  int NumIter = Solver_->NumIters();
  //if (rank==0) printf("Number of iterations = %d\n",NumIter);
  *ITLIN = NumIter;  
 
  /* get # iterations
  double time = Solver_->SolveTime();
  if (rank==0) printf("Time spend in solver = %f sec\n",time);
  */

  // get residual
  //const double* status = Solver_->GetAztecStatus();
  //if (rank==0) printf("From AZstatus: true residual=%15.7e\n",status[AZ_r]);

  double res = Solver_->TrueResidual();
  //if (rank==0) printf("True residual %12.6e\n",res);
  //double scaledres = Solver_->ScaledResidual();
  //if (rank==0) printf("Scaled residual %12.6e\n",scaledres);

  if (rank==0) {
    printf("# of GMRES itns = %13d res.err=%15.7e\n",NumIter,res);
    *log_ << endl <<"# of GMRES itns = " << NumIter << " res.err=" << res << endl;
  }

// gp dbg
   if (DBG) {
     char solfilename[50];
     sprintf(solfilename,"x_s%d_n%d",_Nstep,_Newt);
     printf("output trilinos solution in %s\n",solfilename);
     EpetraExt::VectorToMatrixMarketFile(solfilename,*lhs_);
     cin.get();
  }
// gp dbg


  // Trilinos Ghost output
  //Epetra_Vector lhsG(A_->ColMap());
  //lhsG.Import(*lhs_,*A_->Importer(),Add);
 
  //lhsG.Map().Print(cout);
  //EpetraExt::VectorToMatrixMarketFile("lhsG.out",lhsG);

  // copy solution into DUNK
  int NLocal = 0;
  for (int IPH = 1; IPH <= *NUMEQ ; IPH++) {
  for (int K = *KL1; K <= *KL2 ; K++) {
     for (int J = JLV1[K-1]; J <= JLV2[K-1]; J++) {
        for (int I = *IL1; I <= *IL2; I++) {
           int IPARS_Index_cell = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
           int IPARS_Index_dof = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM)) +
                                 (IPH-1)*((*IDIM)*(*JDIM)*(*KDIM));
           if (KEYOUT[IPARS_Index_cell] == 1)
             DUNK[IPARS_Index_dof] = (*lhs_)[NLocal++];
           else
             DUNK[IPARS_Index_dof] = 0;
        }
        }
     }
  }

  // copy solution into DUNK, adding ghosts
  /*
  for (int K = 1; K <= *KDIM ; K++) {
     for (int J =1; J <= *JDIM; J++) {
        for (int I = 1; I <= *IDIM; I++) {
           int IPARS_Index = (I-1) + (J-1)*(*IDIM) + (K-1)*((*IDIM)*(*JDIM));
           //if (KEYOUT[IPARS_Index] != 0 && ColGids_[IPARS_Index]>=0){
           if (ColGids_[IPARS_Index]>=0){
	      int GID=ColGids_[IPARS_Index];
              int LID=lhsG.Map().LID(GID);
              if(LID==-1) printf("[%d] Error: LID=%d GID=%d\n",rank,LID,GID);
              else DUNK[IPARS_Index]=lhsG[LID];
           }
           else
              DUNK[IPARS_Index] = 0;
        }
     }
  }
  */
}

/***********************************************************************/
/***********************************************************************/

// debug print routines

void print_array_int_withghosts(
  const char *aname,
  int *array, 
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *LDIM /* = &one */)
{
  const int width = 3;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_gh_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx;
  for (int L=1; L<=*LDIM; L++) {
    if (L > 1) { fprintf(fid,"\n*****************************************\n"); }
    for (int I=1; I<=*IDIM; I++) {
      if (*LDIM==1) { fprintf(fid,"\n%s(%i,:,:)=\n",aname,I); }
      else { fprintf(fid,"\n%s(%i,:,:,%i)=\n",aname,I,L); }
      fprintf(fid," K J");
      for (int J=1; J<=*JDIM; J++) { fprintf(fid," %*d",width,J); }
      fprintf(fid,"\n");
      for (int K=1; K<=*KDIM; K++) {
        fprintf(fid,"\n %-*d",3,K);
        for (int J=1; J<=*JDIM; J++) {
          idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM) +
                (L-1)*(*IDIM)*(*JDIM)*(*KDIM);
          fprintf(fid," %*d",width,array[idx]);
        }
      }
      fprintf(fid,"\n");
    }
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_int(
  const char *aname,
  int *array, 
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *LDIM /* = &one */)
{
  const int width = 3;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx,J1,J2;
  J1 = JLV1[*KL1-1];
  J2 = JLV2[*KL1-1];
  for (int K = *KL1; K < *KL2 ; K++) {
    if ( JLV1[K] < J1 ) { J1 = JLV1[K]; }
    if ( JLV2[K] > J2 ) { J2 = JLV2[K]; }
  }

  for (int L=1; L<=*LDIM; L++) {
    if (L > 1) { fprintf(fid,"\n*****************************************\n"); }
    for (int I = *IL1; I <= *IL2; I++) {
      if (*LDIM==1) { fprintf(fid,"\n%s(%i,:,:)=\n",aname,I); }
      else { fprintf(fid,"\n%s(%i,:,:,%i)=\n",aname,I,L); }
      fprintf(fid," K J");
      for (int J = J1; J <= J2; J++) { fprintf(fid," %*d",width,J); }
      fprintf(fid,"\n");
      for (int K = *KL1; K <= *KL2 ; K++) {
        fprintf(fid,"\n %-*d",3,K);
        for (int J = J1; J <= J2; J++) {
          if ( (J >= JLV1[K-1]) && (J <= JLV2[K-1]) ) {
            idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM) +
                  (L-1)*(*IDIM)*(*JDIM)*(*KDIM);
            if (KEYOUT[idx] == 1) fprintf(fid," %*d",width,array[idx]);
            else fprintf(fid," %*s",width," "); 
          }
          else fprintf(fid," %*s",width," ");        
        }
      }
      fprintf(fid,"\n");
    }
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_int_glob(
  const char *aname,
  int *array,
  const int *NX,
  const int *NY,
  const int *NZ,
  const int *IOFF,
  const int *JOFF,
  const int *KOFF,
  const int *KEYOUT,
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *LDIM /* = &one */)
{
  const int width = 3;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_glb_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");

  for (int L=1; L<=*LDIM; L++) {
    if (L > 1) { fprintf(fid,"\n*****************************************\n"); }
    for (int IG = 1; IG <= *NX; IG++) {
      int I = IG - *IOFF; 
      if (*LDIM==1) { fprintf(fid,"\n%s(%i,:,:)=\n",aname,IG); }
      else { fprintf(fid,"\n%s(%i,:,:,%i)=\n",aname,IG,L); }
      fprintf(fid," K J");
      for (int JG = 1; JG <= *NY; JG++) { fprintf(fid,"  %*d ",width,JG); }
      fprintf(fid,"\n");
      for (int KG = 1; KG <= *NZ ; KG++) {
        int K = KG - *KOFF;
        fprintf(fid,"\n %-*d",3,KG);
        for (int JG = 1; JG <= *NY; JG++) {
          int J = JG - *JOFF;
          int idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM) +
                    (L-1)*(*IDIM)*(*JDIM)*(*KDIM);
          if ((I>=1) && (I<=*IDIM) && (J>=1) && (J<=*JDIM) && (K>=1) && (K<=*KDIM)) {
            if (KEYOUT[idx] == 1) fprintf(fid,"  %*d ",width,array[idx]);
            else fprintf(fid," (%*d)",width,array[idx]); 
          }
          else fprintf(fid," %*s",width+2," ");
        }
      }
      fprintf(fid,"\n");
    }
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_double_withghosts(
  const char *aname,
  double *array, 
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *LDIM /* = 1 */)
{
  const int precision = 3;
  const int width = precision + 7;
  //const int width = 5;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_gh_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx;
  for (int I=1; I<=*IDIM; I++) {
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,I);
    fprintf(fid," K J");
    for (int J=1; J<=*JDIM; J++) { fprintf(fid," %*d",width,J); }
    fprintf(fid,"\n");
    for (int K=1; K<=*KDIM; K++) {
      fprintf(fid,"\n %-*d",3,K);
      for (int J=1; J<=*JDIM; J++) {
        idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
        //fprintf(fid," %*.*e",width,precision,array[idx]);
        //fprintf(fid," %*.*lf",width,precision,array[idx]);
        //fprintf(fid," %lf",array[idx]);
        //fprintf(fid," %*g",width,array[idx]);
        fprintf(fid," %*.*g",width,precision,array[idx]);
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_double(
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const char *aname,
  double *array) 
{
  const int precision = 3;
  const int width = precision + 7;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx,J1,J2;
  J1 = JLV1[*KL1-1];
  J2 = JLV2[*KL1-1];
  for (int K = *KL1; K < *KL2 ; K++) {
    if ( JLV1[K] < J1 ) { J1 = JLV1[K]; }
    if ( JLV2[K] > J2 ) { J2 = JLV2[K]; }
  }

  for (int I = *IL1; I <= *IL2; I++) {
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,I);
    fprintf(fid," K J");
    for (int J = J1; J <= J2; J++) { fprintf(fid," %*d",width,J); }
    fprintf(fid,"\n");
    for (int K = *KL1; K <= *KL2 ; K++) {
      fprintf(fid,"\n %-*d",3,K);
      for (int J = J1; J <= J2; J++) {
        if ( (J >= JLV1[K-1]) && (J <= JLV2[K-1]) ) {
          idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
          if (KEYOUT[idx] == 1) fprintf(fid," %*.*g",width,precision,array[idx]);
          else fprintf(fid," %*s",width," "); 
        }
        else fprintf(fid," %*s",width," ");        
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_double_glob(
  const int *NX,
  const int *NY,
  const int *NZ,
  const int *IOFF,
  const int *JOFF,
  const int *KOFF,
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *KEYOUT,
  const char *aname,
  double *array) 
{
  const int precision = 3;
  const int width = precision + 7;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_glb_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");

  for (int IG = 1; IG <= *NX; IG++) {
    int I = IG - *IOFF; 
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,IG);
    fprintf(fid," K J");
    for (int JG = 1; JG <= *NY; JG++) { fprintf(fid,"  %*d ",width,JG); }
    fprintf(fid,"\n");
    for (int KG = 1; KG <= *NZ ; KG++) {
      int K = KG - *KOFF;
      fprintf(fid,"\n %-*d",3,KG);
      for (int JG = 1; JG <= *NY; JG++) {
        int J = JG - *JOFF;
        int idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
        if ((I>=1) && (I<=*IDIM) && (J>=1) && (J<=*JDIM) && (K>=1) && (K<=*KDIM)) {
          if (KEYOUT[idx] == 1) fprintf(fid,"  %*.*g ",width,precision,array[idx]);
          else fprintf(fid," (%*.*g)",width,precision,array[idx]); 
        }
        else fprintf(fid," %*s",width+2," ");
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_float_withghosts(
  const char *aname,
  float *array, 
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *LDIM /* = 1 */)
{
  const int precision = 3;
  const int width = precision + 7;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_gh_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx;
  for (int I=1; I<=*IDIM; I++) {
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,I);
    fprintf(fid," K J");
    for (int J=1; J<=*JDIM; J++) { fprintf(fid," %*d",width,J); }
    fprintf(fid,"\n");
    for (int K=1; K<=*KDIM; K++) {
      fprintf(fid,"\n %-*d",3,K);
      for (int J=1; J<=*JDIM; J++) {
        idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
        fprintf(fid," %*.*g",width,precision,array[idx]);
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_float(
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *IL1,
  const int *IL2,
  const int *JLV1,
  const int *JLV2,
  const int *KL1,
  const int *KL2,
  const int *KEYOUT,
  const char *aname,
  float *array) 
{
  const int precision = 3;
  const int width = precision + 7;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");
  int idx,J1,J2;
  J1 = JLV1[*KL1-1];
  J2 = JLV2[*KL1-1];
  for (int K = *KL1; K < *KL2 ; K++) {
    if ( JLV1[K] < J1 ) { J1 = JLV1[K]; }
    if ( JLV2[K] > J2 ) { J2 = JLV2[K]; }
  }

  for (int I = *IL1; I <= *IL2; I++) {
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,I);
    fprintf(fid," K J");
    for (int J = J1; J <= J2; J++) { fprintf(fid," %*d",width,J); }
    fprintf(fid,"\n");
    for (int K = *KL1; K <= *KL2 ; K++) {
      fprintf(fid,"\n %-*d",3,K);
      for (int J = J1; J <= J2; J++) {
        if ( (J >= JLV1[K-1]) && (J <= JLV2[K-1]) ) {
          idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
          if (KEYOUT[idx] == 1) fprintf(fid," %*.*g",width,precision,array[idx]);
          else fprintf(fid," %*s",width," "); 
        }
        else fprintf(fid," %*s",width," ");        
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}

/***********************************************************************/
/***********************************************************************/

void print_array_float_glob(
  const int *NX,
  const int *NY,
  const int *NZ,
  const int *IOFF,
  const int *JOFF,
  const int *KOFF,
  const int *IDIM,
  const int *JDIM, 
  const int *KDIM,
  const int *KEYOUT,
  const char *aname,
  float *array) 
{
  const int precision = 3;
  const int width = precision + 7;
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  char fname[30];
  sprintf(fname,"%s_glb_Pr%d.dat",aname,rank);
  FILE *fid = fopen(fname,"w");

  for (int IG = 1; IG <= *NX; IG++) {
    int I = IG - *IOFF; 
    fprintf(fid,"\n%s(%i,:,:)=\n",aname,IG);
    fprintf(fid," K J");
    for (int JG = 1; JG <= *NY; JG++) { fprintf(fid,"  %*d ",width,JG); }
    fprintf(fid,"\n");
    for (int KG = 1; KG <= *NZ ; KG++) {
      int K = KG - *KOFF;
      fprintf(fid,"\n %-*d",3,KG);
      for (int JG = 1; JG <= *NY; JG++) {
        int J = JG - *JOFF;
        int idx = (I-1) + (J-1)*(*IDIM) + (K-1)*(*IDIM)*(*JDIM);
        if ((I>=1) && (I<=*IDIM) && (J>=1) && (J<=*JDIM) && (K>=1) && (K<=*KDIM)) {
          if (KEYOUT[idx] == 1) fprintf(fid,"  %*.*g ",width,precision,array[idx]);
          else fprintf(fid," (%*.*g)",width,precision,array[idx]); 
        }
        else fprintf(fid," %*s",width+2," ");
      }
    }
    fprintf(fid,"\n");
  }
  fclose(fid);
}


}// extern C


/***********************************************************************/
/***********************************************************************/



/***********************************************************
* Driver for Unit Test of Trilinos in IPARS, single phase
***********************************************************/
#ifdef UNIT_TEST
int main(int argc, char *argv[])
{
  int IDIM,JDIM,KDIM,LDIM,IL1,IL2,*JLV1,*JLV2,KL1,KL2,*KEYOUT,NBLK,N123;
  float *COF;
  double *RESID,*DUNK,*DUNK2;
  int NX,NY,NZ,IOFF,JOFF,KOFF;

// Initialize MPI
  MPI_Init(&argc,&argv);
  int nproc;
  MPI_Comm_size (MPI_COMM_WORLD, &nproc);
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);

  int testnum = 2;  // 1: single processor, 2x4x4
                    // 2: two processors, 2x4x4
                    // 3: three processors, 2x4x4
	                // 4: four processors, 2x5x5

//-----------------------------------------------------------------------
  int pdbg = 1;
// bag8 : Uncomment this for parallel debugging with gdb....
//  pdbg = 0;
  if (pdbg==0) printf("Parallel debugging ON -- pdbg=0\n");
  while (pdbg == 0) {}

// check for proper # processors usage

  switch (testnum) {

    case 1:
      if (nproc!=1) {
        printf("Test %d requires 1 processor!\n", testnum);
        exit(1);
      }
      break;

    case 2:
      if (nproc!=2) {
        printf("Test %d requires 2 processors!\n", testnum);
        exit(1);
      }
      break;

    case 3:
      if (nproc!=3) {
        printf("Test %d requires 3 processors!\n", testnum);
        exit(1);
      }
      break;

    case 4:  // slightly bigger problem, 2x5x5
      if (nproc!=4) {
        printf("Test %d requires 4 processors!\n", testnum);
        exit(1);
      }
      break;

    default:
      printf("Test %d is not defined yet!\n", testnum);
      exit(1);
      break;   
  }

// copy the right data 

  if (rank==0) {
     char cmd[100];
     printf("Removing test files in current directory ...\n"); 
     sprintf(cmd,"\\rm -f Abx_Pr*");
     printf("%s\n",cmd);
     system(cmd);   
     sprintf(cmd,"\\rm -f dunk1_Pr*");
     printf("%s\n",cmd);
     system(cmd);
     sprintf(cmd,"\\rm -f dunk2_Pr*");
     printf("%s\n",cmd);
     system(cmd);
     printf("Copying test files from directory test%d ...\n",testnum);
     sprintf(cmd,"\\cp -f test%d/Abx_Pr* .",testnum);
     printf("%s\n",cmd);
     system(cmd);
     if (testnum>1) {
       sprintf(cmd,"\\cp -f test%d/dunk1_Pr* .",testnum);
       printf("%s\n",cmd);
       system(cmd);
       sprintf(cmd,"\\cp -f test%d/dunk2_Pr* .",testnum);
       printf("%s\n",cmd);
       system(cmd);
     }
  }

//--------------------------------------------------------
// Read in IPARS file
  MPI_Barrier( MPI_COMM_WORLD );
  char fname[20];
  sprintf(fname,"Abx_Pr%d.dat",rank);
  printf("Proc %d reading file %s\n",rank,fname);
  FILE *fid = fopen(fname,"r");
// added the next 6 for printing in GLOBAL indexes
  fscanf(fid,"%i",&NX);
  fscanf(fid,"%i",&NY);
  fscanf(fid,"%i",&NZ);
  fscanf(fid,"%i",&IOFF);
  fscanf(fid,"%i",&JOFF);
  fscanf(fid,"%i",&KOFF);
// the twelve standard args in callwork
  fscanf(fid,"%i",&IDIM);
  fscanf(fid,"%i",&JDIM);
  fscanf(fid,"%i",&KDIM);
  N123=IDIM*JDIM*KDIM;

  JLV1 = (int *) malloc(sizeof(int) * KDIM);
  JLV2 = (int *) malloc(sizeof(int) * KDIM);
  KEYOUT = (int *) malloc(sizeof(int) * N123);
  COF = (float *) malloc(sizeof(float) * 7 * N123);   // matrix
  RESID = (double *) malloc(sizeof(double) * N123);   // rhs
  DUNK = (double *) malloc(sizeof(double) * N123);    // solution from Trilinos
  DUNK2 = (double *) malloc(sizeof(double) * N123);   // solution from ygmres

  fscanf(fid,"%i",&LDIM);
  fscanf(fid,"%i",&IL1);
  fscanf(fid,"%i",&IL2);
  for(int i=0; i<KDIM; i++) {fscanf(fid,"%i",&JLV1[i]);}
  for(int i=0; i<KDIM; i++) {fscanf(fid,"%i",&JLV2[i]);}
  fscanf(fid,"%i",&KL1);
  fscanf(fid,"%i",&KL2);
  for(int i=0; i<N123; i++) {fscanf(fid,"%i",&KEYOUT[i]);}
  fscanf(fid,"%i",&NBLK);
  for (int i=0; i<7*N123; i++) { fscanf(fid, "%f", &COF[i]); }
  for (int i=0; i<N123; i++) { fscanf(fid, "%lf", &RESID[i]); }
  for (int i=0; i<N123; i++) { fscanf(fid, "%lf", &DUNK2[i]); }
  fclose(fid);
//--------------------------------------------------------

// gp dbg
//  sprintf(fname,"keyout_Pr%d.dat",rank);
//  fid = fopen(fname,"w");
//  //printf("fname='%s'\n",fname);
//  int idx=0;
//  fprintf(fid,"i,j,k, idx,KEYOUT\n");
//  for (int K=1; K<=KDIM; K++) {
//    for (int J=1; J<=JDIM; J++) {
//      for (int I=1; I<=IDIM; I++) {
//        fprintf(fid,"%d %d %d -> %d  %d\n",I,J,K,idx,KEYOUT[idx]);
//        idx++;
//      }
//    }
//  }
//  fprintf(fid,"\n============\n");
//  for (int I=1; I<=IDIM; I++) {
//    fprintf(fid,"KEYOUT(%i,:,:)=\n",I);
//    for (int K=1; K<=KDIM; K++) {
//      for (int J=1; J<=JDIM; J++) {
//        idx = (I-1) + (J-1)*IDIM + (K-1)*IDIM*JDIM;
//        fprintf(fid,"%*d ",2,KEYOUT[idx]);
//      }
//      fprintf(fid,"\n"); 
//    }
//    fprintf(fid,"\n");
//  }
//  fclose(fid);

  print_array_int_withghosts(&IDIM,&JDIM,&KDIM,"KEYOUT",KEYOUT); 
//  print_array_int("keyout",KEYOUT,&IL1,&IL2,JLV1,JLV2,&KL1,&KL2,
//                  KEYOUT,&IDIM,&JDIM,&KDIM);
  print_array_int_glob("KEYOUT",KEYOUT,&NX,&NY,&NZ,&IOFF,&JOFF,&KOFF,
                       KEYOUT,&IDIM,&JDIM,&KDIM);
//  print_array_double_withghosts(&IDIM,&JDIM,&KDIM,"DUNK2",DUNK2); 
//  print_array_double(&IDIM,&JDIM,&KDIM,&IL1,&IL2,JLV1,JLV2,&KL1,&KL2,
//                  KEYOUT,"dunk2",DUNK2);
//  print_array_float_withghosts(&IDIM,&JDIM,&KDIM,"COF1",&COF[0]); 
//  print_array_float(&IDIM,&JDIM,&KDIM,&IL1,&IL2,JLV1,JLV2,&KL1,&KL2,
//                  KEYOUT,"cof1",&COF[0]);
//  print_array_float_withghosts(&IDIM,&JDIM,&KDIM,"COF2",&COF[0+1*N123]); 
//  print_array_float(&IDIM,&JDIM,&KDIM,&IL1,&IL2,JLV1,JLV2,&KL1,&KL2,
//                  KEYOUT,"cof2",&COF[0+1*N123]);
//  print_array_float_withghosts(&IDIM,&JDIM,&KDIM,"COF3",&COF[0+2*N123]); 
//  print_array_float(&IDIM,&JDIM,&KDIM,&IL1,&IL2,JLV1,JLV2,&KL1,&KL2,
//                  KEYOUT,"cof3",&COF[0+2*N123]);
// gp dbg

  // Initialize Trilinos
  for (int i=0; i<N123; i++) { DUNK[i] = -1; }
  trilinos_ipars_init1(&IDIM,&JDIM,&KDIM,&LDIM,&IL1,&IL2,
     JLV1,JLV2,&KL1,&KL2,KEYOUT,&NBLK,DUNK);

// gp dbg  print_array_double_withghosts(&IDIM,&JDIM,&KDIM,"DUNK1",DUNK);

  if (testnum>1) { 
    // The "fake update"
    int step = 2;
    if (step==1) {
      sprintf(fname,"dunk1_Pr%d.dat",rank);
      printf("Proc %d writing file: %s\n",rank,fname);
      fid = fopen(fname,"w");
      for(int i=0; i<N123; i++) {fprintf(fid,"%lf\n",DUNK[i]);}
      fclose(fid);
      MPI_Barrier( MPI_COMM_WORLD );
      exit(0);
    }
    else if (step==2) {
      sprintf(fname,"dunk2_Pr%d.dat",rank);
      printf("Proc %d reading file %s\n",rank,fname);
      fid = fopen(fname,"r");
      for(int i=0; i<N123; i++) {
       fscanf(fid,"%lf\n",&DUNK[i]);
        //if (rank==0) printf("Proc %d: i=%d dunk2=%lf\n",rank,i,DUNK[i]);
      }
      fclose(fid);
    }
  }

// gp dbg  print_array_double_withghosts(&IDIM,&JDIM,&KDIM,"DUNK2",DUNK);
/* print_array_double_glob(&NX,&NY,&NZ,&IOFF,&JOFF,&KOFF,&IDIM,&JDIM,&KDIM,
                  KEYOUT,"DUNK2",DUNK);
MPI_Barrier( MPI_COMM_WORLD );
exit(0); */
  trilinos_ipars_init2(&IDIM,&JDIM,&KDIM,&LDIM,&IL1,&IL2,
     JLV1,JLV2,&KL1,&KL2,KEYOUT,&NBLK,DUNK);
  for (int i=0; i<N123; i++) { DUNK[i] = 0; }

  // Solve system with Trilinos
  trilinos_ipars_fill_matrix_and_rhs(&IDIM,&JDIM,&KDIM,&LDIM,&IL1,&IL2,
     JLV1,JLV2,&KL1,&KL2,KEYOUT,&NBLK,COF,RESID);
  trilinos_setup_solver_();
  trilinos_ipars_solve(&IDIM,&JDIM,&KDIM,&LDIM,&IL1,&IL2,
     JLV1,JLV2,&KL1,&KL2,KEYOUT,&NBLK,DUNK);
  // gp dbg  
  //print_array_double_withghosts(&IDIM,&JDIM,&KDIM,"sol-tri",DUNK);  
  //print_array_double_withghosts(&IDIM,&JDIM,&KDIM,"sol-ipars",DUNK2); 
  print_array_double_glob(&NX,&NY,&NZ,&IOFF,&JOFF,&KOFF,&IDIM,&JDIM,&KDIM,
                  KEYOUT,"sol-tri",DUNK);
  print_array_double_glob(&NX,&NY,&NZ,&IOFF,&JOFF,&KOFF,&IDIM,&JDIM,&KDIM,
                  KEYOUT,"sol-ipars",DUNK2);

  // Compute norm(DUNK-DUNK2)
  double nrm = 0.0;
  /*
  printf("proc, i, DUNK[i], DUNK2[i]\n");
  for (int i=0; i<N123; i++) { 
     nrm += pow(DUNK[i]-DUNK2[i],2);
     printf("%d:  %d, %lf, %lf\n",rank,i,DUNK[i],DUNK2[i]);
  }
  */
  nrm = sqrt(nrm);
  printf("\nproc %d: norm(DUNK-DUNK2)=%f\n\n",rank,nrm);

  // Exit
  trilinos_cleanup_();
  MPI_Finalize();
  return 0;
}
#endif
