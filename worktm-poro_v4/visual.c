// -----------------------------------------------------------------
// file: visual.c
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
// XGai, 08/18/03
// Add in _vis_vnodal_set() routine to set the flag indicating that a
// variable is valued at nodal points. Therefore, interpolation is not
// necessary.
//-----------------------------------------------------------------

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "cfsimple.h"
#include "visualc.h"

// ------------------------------------------------------------------
// definitions of global variables, see declarations in visualc.h

int _Vis_flag;         // visual flag C-global
int _Vis_cnt[MAXBLK];  // counts if init file for the block exists
int _Vis_init;         // counts if some initialization has been made
int _Vis_newtimestep;  // counts if we need to output tstep info to Vis.inf
char* _Vis_vecnam[MAXVISVARS];
VIS_INFO _Vis_pars[MAXVISVARS];

// -- data structures for fully unstructured output : VIS3
NODE * _Nodes[MAXBLK]; // structure holding info about the nodes
int _Nnod[MAXBLK]; //global counter of the number of nodes for the block

int _Ngblk[MAXBLK]; // global cnter of # of gridbcks for the block
int _Myprc; // processor number, set by vis_fname_set
int _Filestyle; // style of the file name, see vis_fname_set
char ROOTname [MAXFNAMELEN] ; // root name for the vis output files
                                     // set by vis_fname_set
                                     // or by visualization_init (default)
char INFOname [MAXFNAMELEN] ; // root name for the info vis file
                                     // set by vis_fname_set
                                     // or by visualization_init (default)

// bag8, gp
char DIRname [MAXFNAMELEN] ;

// ---------------------------------------------------------
//  copies of variables from IPARS

_F_INTEGER _Nstep; // time step setup externally from Fortran
_F_REAL_8   _Time; // time step value set externally from Fortran

_F_INTEGER _Mxrecxp,_Myrecyp,_Mzreczp,_Mblk,_Nprocs;

//------------------------------------------------------ DEBUG
FILE *fvdebug;

// --------------------- local functions
static void _vis_reset();

//-------------------------------------------------------------
void _visual_init()
{
  int i;
  for(i=0;i<MAXVISVARS;i++) {
    _Vis_pars[i].ldim=1; // standard offset: zero
    _Vis_pars[i].name=NULL; // names of variables
    _Vis_pars[i].vnodal=0;  // cell centered variables
  }

	//  _Ifnew = 1; // flag that determines how many times we opened Vis.inf

  _Vis_newtimestep=-1;
  _vis_reset();

}

//----------------------------------------------------------

void _vis_off_set(
		_F_INTEGER *num_vis_vars,
		_F_INTEGER *offsets
		)
// set the offset of the variables to _Vis_pars
//
{
  const int nvars=*num_vis_vars;

  if( nvars >0  ) {
    int i;

    for(i=0;i< nvars ;i++) {
      if ( offsets [i] > 0 )
	_Vis_pars[i].ldim= offsets [i];

      //DEBUG
      // printf("\n offset for %d variable is %d\n",i,offsets [i]);
    }
  }

}

//----------------------------------------------------------
// set flags indicating corner point variables

void _vis_vnodal_set(
                _F_INTEGER *num_vis_vars,
                _F_INTEGER *val_nodal
                )
// set the offset of the variables to _Vis_pars
//
{
  const int nvars=*num_vis_vars;

  if( nvars >0  ) {
    int i;

    for(i=0;i< nvars ;i++) {
      if ( val_nodal [i] > 0 )
        _Vis_pars[i].vnodal = val_nodal [i];

      //DEBUG
      //printf("\n offset for %d variable is %d\n",i,val_nodal [i]);
    }
  }

}

//----------------------------------------------------------
// set the names of the vis variables in _Vis_pars->name
//
void _vis_name_set(
		    _F_INTEGER *narg, /* number of strings in the array */
		    _F_INTEGER *len, /* the length as computed in the
					calling Fortran routine **/
 		    char * carray  /* ptr to the first character */
		    //_F_INTEGER slen /* this is really a dummy parameter */
		    )	
{
  // set the names according to the arg list

  if ( ( (*narg) >0 ) && ( (*len) >0) ) {
    char *buffer = (char *) malloc( ((*narg) *(*len)+1) * sizeof(char) );
    int i;
    if( (buffer!= NULL)) {
      char *p;

      /** copy the whole string array to the buffer **/
      buffer[0]='\0';p=strncat(buffer,carray,(*len)*(*narg));

      /** partition the buffer and copy it to the permanent array **/
      for (i=0;i< (*narg);i++) {
	char ptr[MAXSTRLEN];size_t l;
	
	ptr[0]='\0';p=strncat(ptr,& (buffer[i* (*len) ] ), *len);
	l= strcspn(ptr," ");
	
	if(_Vis_pars[i].name !=NULL) free(_Vis_pars[i].name);
	_Vis_pars[i].name=(char *) malloc ((l+1) * sizeof(char));
	_Vis_pars[i].name[0]='\0';p=strncat(_Vis_pars[i].name,ptr,l);
	
	// DEBUG
	/*
	printf("\nParameter %d: <%s> from <%s>\n",
	       i, _Vis_pars[i].name,ptr);
	*/
      }
      free(buffer);
    }

  }

}


// ----------------------------------------------------------------
// set the root name for the filename and some aux. names for zones
//
void _vis_info_set(
		    _F_INTEGER *Nprocs,_F_INTEGER *myprc,
		    _F_INTEGER *Mblk,
		    _F_INTEGER *Mxrecxp,_F_INTEGER *Myrecyp,
		    _F_INTEGER *Mzreczp
		    )
{
  _Myprc=*myprc;
  _Mblk=*Mblk; // max number of faultblocks
  _Nprocs=*Nprocs; // current number of processors
  _Mxrecxp=*Mxrecxp;  _Myrecyp=*Myrecyp;  _Mzreczp=*Mzreczp;

  // reset all properties here
  _vis_reset();

}


// ------------------------------------------------------
// set the current time step
//
void _vis_tstep_set(_F_INTEGER *ntstep, _F_REAL_8 *time)
{
  _Nstep=*ntstep;_Time=*time;

  if(_Myprc == 0) {

    char VISTIMname[30];
    sprintf(VISTIMname,"%sVis.tim",DIRname);
    FILE *fp=fopen(VISTIMname,"a");
    if(fp==NULL) return;
    fprintf(fp,"%10d  %4G\n",_Nstep,_Time);
    fclose(fp);
  }
}


// ----------------------------------------------------------
static void _vis_reset()
{
int i;
for(i=0;i<MAXBLK;i++)_Vis_cnt[i]=0;
for(i=0;i<MAXBLK;i++)_Nnod[i]=0;
for(i=0;i<MAXBLK;i++)_Ngblk[i]=0;
for(i=0;i<MAXBLK;i++)

  if(_Nodes[i]!=NULL){
    free(_Nodes[i]);_Nodes[i]=NULL;
  }
}

// ---------------------------------------------------------
// set vector field names (from input file)
//
void _vis_setvec_nam(_F_INTEGER* n, _F_CHARACTER* vnam)
{
  int i=0; int j;
  int ic=*n-1;
  if (_Vis_vecnam[ic] != NULL) free(_Vis_vecnam[ic]);
  while(vnam[i++]!=' '){
    if(i>MAXSTRLEN) {
       if(_Myprc==0)
       fprintf(stderr,"Warning: Vector field %d name too big!\n",ic);
       break;
    }
  }
  if(i==1) {
     if(_Myprc==0)
          fprintf(stderr,"Warning: Vector field %d has no name!\n",ic+1);
     exit(-1);
  }
  _Vis_vecnam[ic]=(char*) malloc(i*sizeof(char));
  for(j=0; j < (i-1); j++)
  {
  _Vis_vecnam[ic][j]=vnam[j];
  }
  _Vis_vecnam[ic][i-1]='\0';
}
