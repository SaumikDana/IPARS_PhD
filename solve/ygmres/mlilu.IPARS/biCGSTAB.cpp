#include "library.h"

#include <time.h>
#ifndef _DEF_
#include "definitions.h"
#endif

#include <iostream.h>
#include <fstream.h>


#define _umd2so   _F_NAME_(UMD2SO,umd2so)
#define _umd2fa   _F_NAME_(UMD2FA,umd2fa)
#define _umd2rf   _F_NAME_(UMD2RF,umd2rf)
#define _umd2in   _F_NAME_(UMD2IN,umd2in)
_F_EXTERN_(void) _umd2so(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER*, _F_REAL_8 * , _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *, _F_REAL_8 *, _F_REAL_8 *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2fa(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *,_F_INTEGER *, _F_INTEGER *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2rf(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER , _F_REAL_8 *,
            _F_INTEGER *,_F_INTEGER *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2in(_F_INTEGER *, _F_REAL_8 *, _F_INTEGER *);


void precond(typ **matrix, int **index, typ *vres, typ *vinit,   int dim, int which_prec)
{

  int j;
 
  switch ( which_prec)
    {
    case 0:
      {
	for ( j= 0; j< dim; j++)
	  {
	    vres[j] = vinit[j];
	  }
	break;
      }
    case 1:
      {
	int lv,lvi;
	/*
	  ofstream sm("sm.dat");
	for ( i=0;i< dim;i++)
	{
	sm << vinit[i] << "\n";
	sm.flush();
	  }
	*/

	/*
	ofstream mat("mat.dat");
	for ( i=0; i< dim; i++)
	  {
	    mat << index_init[i][0] << "\n";
	    mat << mat_init[i][0] << "\n";
	    for (j=1;j<index_init[i][0]; j++)
	      {
		mat << index_init[i][j] << "\n";
		mat << mat_init[i][j] << "\n";
	      }
	  }
	*/
  //	plot(matrix,index,dim,"matricereorg.dat");
	  
	/*
	  // exit(1);
	  
	  int pipo;
	  cout << " MATRIX \n";
	  cin >> pipo;
	*/
	/*
	  for (i=0; i< dim; i++)
	   {
	  vres[i] =0;
	  }
	*/
	lv = 0 ;
	lvi = (size_tab)[10];
	
/*	double t2,t1;

	t1=(double)clock() / CLOCKS_PER_SEC;
*/

	MLILU( &vres, &vinit, lv , &matrix,&index, dim,dim,lvi,gama, delta);
//	cout << "FINI";
//	exit(1);
//	exit(1);
	
//	t2 = (double)clock() / CLOCKS_PER_SEC- t1;

//	printf(" Temps d'un Vcycle : %.15g \n",t2);
//	exit(1);
	
	/*
	  for ( i=0; i< dim; i++)
	  {
	  cout << "PRES " << vres[i] << "\n";
	  }
	*/
	/*
	
	  for  ( i=0; i< dim; i++)
	  {
	    cout << "sol pares prec: " << vres[i] << "\n";
	    }
	    exit(1);
	*/
	



//	initial_order(&vres,dim);





	//exit(1);

	/*
	
	for ( i=0; i< dim; i++)
	  {
	    cout << "sol pares prec: " <<vres[i] << "\n";
	  }
	exit(1);
	*/


	/*
	int stop;
	cin >> stop;

	*/

	/*

	  for ( i=0; i< dim; i++)
	  {
	  cout << "RES " << vres[i] << "\n";
	  }
	  exit(1);
	*/

	  /*	kill(matrix);
		kill2(size_tab);
		killC(tab);
		killC(spec_tab);
	  */
	break;
      }
    case 2:
      {
	// Jacobi
	int i;
	for (i=0; i<dim; i++)
	  {
	    vres[i] = vinit[i]/matrix[i][0];
	  }
	break;
      }
    case 3:
      {
	// Gauss-Seidel
	int iterations = 50;
	int increment,j0,i,j;

	for ( i=0; i < dim; i++)
	  {
	    vres[i] = 0;
	  }

	for (increment =0; increment < iterations; increment++)
	  {
	    for ( i=0; i< dim; i++)
	      {
		for ( j = 1; j< index[i][0]; j++)
		  {
		    j0 = index[i][j];
		    if ( (j0 > i) )
		      {
			vres[i] = vres[i] - (matrix[i][j])*vres[j0];
		      }
		  }
		for (j = 1; j< index[i][0] ; j++)
		  {
		    j0 = index[i][j];
		    if  ((j0 < i))
		      {
			vres[i] = vres[i]- (matrix[i][j])*vres[j0];
			//		  cout << "J0" <<  j0-size_begin << "\n";
		      }
		  }
		vres[i] = vinit[i]+vres[i]/ matrix[i][0];
	      }
	  }
      }
      break;
    case 4:
      {
	// iLU(0)
	//plot(matrix,index,dim,"matrice.dat");
	
	int i,j,j0,k,k0,ll;
	typ val;
	int **indexLU = NULL;
	//ofstream outsm("sm_init.dat");
	typ **LU = NULL;
	copy(&matrix,&LU,&index,&indexLU,dim);
	/*
	  for ( i=0; i<dim; i++)
	  {
	  //	    outsm << vinit[i] << "\n";
	  outsm.flush();
	  }
	*/




	for (i=1; i< dim; i++)
	  {
	    for ( k=1; k< indexLU[i][0]; k++)
	      {
		k0 = indexLU[i][k];
		if ( k0 < i )
		  {
		    LU[i][k] = LU[i][k]/LU[k0][0];
		  }
		for ( j = k+1; j< indexLU[i][0]; j++)
		  {
		    j0 = indexLU[i][j];
		    val = 0;
		    for ( ll=1; ll < indexLU[k0][0]; ll++)
		      {
			if ( indexLU[k0][ll] == j0) val = LU[k0][ll];
			break;
		      }
		    if ( i == j0)
		      {
			LU[i][0] = LU[i][0] - LU[i][k]*val;
		      }
		    else
		      {
			LU[i][j] = LU[i][j] - LU[i][k]*val;
		      }
		  }
	
	      }
	    /*	    val = 0;
		    for ( ll= 1; ll< indexLU[i][0]; ll++)
		    {
		    if ( indexLU[i][ll] == i-1)
		    {
		    val = LU[i][ll];
		    break;
		  }
		  }
		  val1 =0;
		  for ( ll= 1; ll< indexLU[i-1][0]; ll++)
		  {
		if ( indexLU[i-1][ll] == i)
		{
		val1 = LU[i-1][ll];
		break;
		}
		}
		LU[i][0] = LU[i][0] - val*val1;
	    */
	  }
	/*
	  plot(LU,indexLU,dim,"LU.dat");
	  
	  exit(1);
	*/
	// FORWARD
       

	typ *temp= (typ*) malloc(sizeof(typ)*dim);
	for ( i=0; i<dim; i++)
	  {
	    temp[i] = 0;
	    for ( j=1; j< indexLU[i][0]; j++)
	      {
		j0 = indexLU[i][j];
		//		cout << "i" << i<< " " <<"J0 " << j0 << "\n";
		if ( j0 < i)
		  {
		    // cout << "i " << i << "j " << j0 << "\n";
		    temp[i] = temp[i] - temp[j0]*LU[i][j];
		  }
	      }
	    temp[i] = (vinit[i] + temp[i]);
	  }
	/*
	  for ( i =0; i<dim; i++)
	  {
	  cout << temp[i] << "\n";
	  cout.flush();
	  }
	*/
	// BACKWARD
	for ( i=dim-1; i>=0; i--)
	  {
	    vres[i] = 0;
	    for ( j = index[i][0]-1; j>0; j--)
	      {
		j0 = index[i][j];
		if ( j0 > i)
		  {
		    vres[i] = vres[i] - vres[j0]*LU[i][j];
		  }
	      }
	    vres[i] = (temp[i]+vres[i])/LU[i][0];
	  }
/*
	for ( i=0; i< dim; i++)
	  {
	    cout << "SOL " << vres[i] << "\n";
	  }
	exit(1);
*/	
	/*
	int stop ;
	cin >> stop;
	*/
	free(temp);
	break;
      }
    default:
      break;
    }
  return;
}

void bicgstab(typ **matrix, int **index, typ *x, typ *b, int dim, int maxit, typ epsilon, int which, bool verbose)
{
//  typ *r0 = (typ*) malloc(sizeof(typ)*dim);
  typ *ri_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *s = (typ*) malloc(sizeof(typ)*dim);
  typ *pi = (typ*) malloc(sizeof(typ)*dim);
  typ *pi_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *pstar = (typ*) malloc(sizeof(typ)*dim);
  typ *vi =  (typ*) malloc(sizeof(typ)*dim);
  typ *vi_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *t = (typ*) malloc(sizeof(typ)*dim);
  typ *rtilde = (typ*) malloc(sizeof(typ)*dim);
  typ *sstar = (typ*) malloc(sizeof(typ)*dim);
  typ alphai,betai,omegai,rhoi_1,rhoi_2,norme;
  double fou = 0;
  int i,j;

  /* NEEDED*/
  
  extern typ **mat_init; 
  extern int **index_init; //= (int**) malloc(sizeof(int*));
  extern typ **matt; //= (typ**) malloc(sizeof(typ*));
  extern int **indext; //= (int**) malloc(sizeof(int*));
  //extern typ *fi; //= (typ*) malloc(sizeof(typ));
  extern int ***indexLU;
  extern typ ***LU;
  extern int ***indKi;
  extern typ ***matKi;
extern int *temporaire2;
  extern int *temporaire;
  extern int taillei;
  extern int factor_for_real;
  extern int factor_for_int;
  extern int *worki;
  extern double *workr;

  worki = (int*) malloc(sizeof(int)*(dim*factor_for_int+30));
  workr = (double*) malloc(sizeof(double)*(dim*factor_for_real+80+4*dim));

  if ( dim > 2000 )
    {
      taillei = 10*dim;
    }
  else taillei = 2000;
  temporaire = (int*) malloc(sizeof(int)*taillei);
  temporaire2 = (int*) malloc(sizeof(int)*taillei);

  //  copy(&matrix,&mat_init,&index,&index_init,dim);
  //  copy(&matrix,&matt,&index,&indext,dim);


  static int deb=0;

  if ( deb == 0 )
    {
      matKi[0] = NULL; 
      mat_init = (typ**) malloc(sizeof(typ*)*dim);
      index_init = (int**) malloc(sizeof(int*)*dim);
      matt = (typ**) malloc(sizeof(typ*)*dim);
      indext = (int**) malloc(sizeof(int*)*dim);
      *indexLU = (int**) malloc(dim*sizeof(int*));
      *LU = (typ**) malloc(dim*sizeof(typ*));
      (*LU)[0] = NULL;
      deb++;
    }
  
  if ( ( *LU)[0] == NULL )
	{
      for (i=0;i<dim;i++)
	{
	  mat_init[i] = (typ*) malloc(sizeof(typ)*7);
	  index_init[i] = (int*) malloc(sizeof(int)*7);
	  matt[i] = (typ*) malloc(sizeof(typ)*7);
	  indext[i] = (int*) malloc(sizeof(int)*7);
	  (*indexLU)[i] = (int*) malloc(sizeof(int)*7);  
	  (*LU)[i] = (typ*) malloc(sizeof(typ)*7);
	  (*LU)[i][0] = 1;
	  (*indexLU)[i][0] = 1;
	  for ( j=0; j< index[i][0];j++)
	    {
	      matt[i][j] =mat_init[i][j] = matrix[i][j];
	      indext[i][j] = index_init[i][j] = index[i][j];
	    }
	}
    }
  else
    {
      for (i=0;i<dim;i++)
	{
	//  mat_init[i] = (typ*) malloc(sizeof(typ)*7);
	//  index_init[i] = (int*) malloc(sizeof(int)*7);
	  (*indexLU)[i] = (int*) realloc((*indexLU)[i],sizeof(int)*7);
	  matt[i] = (typ*) realloc(matt[i],sizeof(typ)*7);
	  indext[i] = (int*) realloc(indext[i],sizeof(int)*7);
	  (*LU)[i] = (typ*) realloc((*LU)[i],sizeof(typ)*7);
	  (*LU)[i][0] = 1;
	  (*indexLU)[i][0] = 1;
	  for ( j=0; j< index[i][0];j++)
	    {
	      matt[i][j] = mat_init[i][j] = matrix[i][j];
	      indext[i][j] = index_init[i][j] = index[i][j];
	      
	    }
	}
    }
  
//  transpose(&mat_init,&index_init,&matt,&indext,dim);


  /*END NEEDED*/

  typ normeb;
  
  /* RPECOND MILU */
  
  // precond(matrix,index,pstar,pi,dim,which,i,firstp,graph_init);

  double t1,t2,t3,t4,t5;
  t1 = (double)clock() / CLOCKS_PER_SEC;

  if ( which == 1)
    {
      initialise(&mat_init,&index_init,&matt,&indext,dim);
      t3 = (double)clock() / CLOCKS_PER_SEC;
    }
  
  else     
{
      mat_init = (typ **) malloc(sizeof(typ*));
      index_init = (int **) malloc(sizeof(int*));
      mat_init = matrix;
      index_init = index;

    }
  
  /* END OF MLILU PRECOND */

 // matvec(matrix,index,b,sstar,dim);

  /*
    for ( i=0; i< dim; i++)
    {
    cout << sstar[i] << "\n";
    }
  */

  for (i=0; i<dim; i++)
    {
      ri_1[i] = b[i];// - sstar[i];
      rtilde[i] = b[i];// - sstar[i];
      x[i] = 0;//b[i];
//      cout << "Sm : " << b[i] << "\n";
    }
  

  normeb = norm(b,dim);
  if ( verbose)  printf(" \n NORME DU SECOND MEMBRE :   %.15g \n\n", normeb); 
 
  
  for (i=1; i <= maxit; i++)
    {
      
      if ( verbose ) 
	{
	   printf(" ITERATION NUMERO :  %d \n\n", i);
//	  cout.flush();
	}
       rhoi_1 = pscal(rtilde,ri_1,dim);
//      cout << rhoi_1;
      if ( rhoi_1 == 0) 
	{
	  if (verbose) printf(" DO NOT CONVERGE ");
	  exit(1);
	}
      if ( i == 1)
	{
	  for ( j = 0; j< dim; j++)
	    {
	      pi[j] = ri_1[j];
	      rhoi_2 = rhoi_1 ;
	      pstar[j] = 0;
	      sstar[j]=0;
	    }
	}
      else
	{
	  betai = (rhoi_1/rhoi_2)*(alphai/omegai);
	  for (j=0; j<dim; j++)
	    {
	      pi[j] = ri_1[j] + betai*(pi_1[j]-omegai*vi_1[j]);
	      pstar[j]=0;
	      sstar[j] = 0;
	    }
	}
      if ( i==1 )
	{ 
	  t4 = (double)clock() / CLOCKS_PER_SEC;
	}
      precond(mat_init,index_init,pstar,pi,dim,which);
      if ( i==1 )
	{
	  t5 = (double)clock() / CLOCKS_PER_SEC - t4;
	}

      //plot(mat_init,index_init,dim);
   
      matvec(matrix,index,pstar,vi,dim);
      alphai = rhoi_1/(pscal(rtilde,vi,dim)+fou); //cout << "ALPHA " << alphai << "\n";
      for ( j=0; j< dim; j++)
	{
	  s[j] = ri_1[j]-alphai*vi[j];
	}
      norme = norm(s,dim)/normeb;
      if ( verbose ) printf(" NORME 1 :  %.15g \n\n",norme);
      if ( norme < epsilon) 
	{
	  for (j=0; j< dim; j++)
	    {
	      x[j] = x[j] + alphai*pstar[j];
	    }
	  if ( verbose) 
	    {
	      printf(" NOMBRE D'ITERATIONS RES1:  %d \n\n", i );
	      printf(" TAUX DE CONVERGENCE  %.15g ",pow(norme,1/double(i)));
	    }
	  for (j=0; j< dim; j++)
	    {
	      /*
	      free(mat_init[j]) ;
	      free(index_init[j]);
	      free( matt[j]);
	      free(indext[j]);
	      free((*indexLU)[j]);
	      free( (*LU)[j]);
	      */
	      
	    }
	  free(ri_1); 
	      free(s); 
	       free(pi); 
	       free(pi_1);
	       free(pstar); 
	      free(t); 
	       free(vi); 
	       free(vi_1);
	       free(sstar); 
	      free(rtilde); 
	      t2 = (double)clock() / CLOCKS_PER_SEC-t1;
	      printf( " \nTEMPS DE CALCUL %.5g \n",t2) ;
	      printf(" TEMPS INIT /  V-CYCLE %.5g \n",(t3-t1)/t5) ;
	      printf( " TEMPS INIT / T RESOLUTION  %.5g \n", ( t3-t1) / t2); 
	  return;
	}

      precond(mat_init,index_init,sstar,s,dim,which);
      matvec(matrix,index,sstar,t,dim);
      omegai = pscal(t,s,dim)/ (pscal(t,t,dim));
      if ( omegai == 0) 
	{
	  printf("\n OMEGAI = 0, FAILURE \n");exit(1);
	}
      for ( j=0; j<dim; j++)
	{
	  x[j] = x[j] + alphai*pstar[j]+omegai*sstar[j];
	  ri_1[j] = s[j]-omegai*t[j];
	  pi_1[j] = pi[j];
	  vi_1[j] = vi[j];
	}
      rhoi_2 = rhoi_1;
      norme = norm(ri_1,dim)/normeb; 
      if ( verbose) printf(" NORME 2  %.15g \n\n",norme);
//      cout << " EPS " << epsilon;

      if ( norme < epsilon) 
	{
	  if ( verbose) 
	    {
	      printf(" NOMBRE D'ITERATIONS RES2:  %d \n", i) ;
	      printf(" TAUX DE CONVERGENCE %.15g ",pow(norme,1/double(i)));
	    }
	  /*
	  for ( j=0; j<dim; j++)
	    {
	      free(mat_init[j]) ;
	      free(index_init[j]);
	      free( matt[j]);
	      free(indext[j]);
	      free((*indexLU)[j]);
	      free( (*LU)[j]);
	  
	    }*/
	  free(ri_1); 
	      free(s); 
	       free(pi); 
	       free(pi_1);
	       free(pstar); 
	      free(t); 
	       free(vi); 
	       free(vi_1);
	       free(sstar); 
	      free(rtilde); 
	   
	      t2 = (double)clock() / CLOCKS_PER_SEC-t1;
	  printf( " \nTEMPS DE CALCUL %.5g \n",t2) ;
	      printf(" TEMPS INIT /  V-CYCLE %.5g \n",(t3-t1)/t5) ;
	      printf( " TEMPS INIT / T RESOLUTION  %.5g \n", ( t3-t1) / t2); 
	  return;
	}
    }
  if ( verbose) printf(" NOMBRE MAX D'ITERATIONS ATTEINT ");
  t2 = (double)clock() / CLOCKS_PER_SEC-t1;
 printf( " \nTEMPS DE CALCUL %.5g \n",t2) ;
 printf(" TEMPS INIT /  V-CYCLE %.5g \n",(t3-t1)/t5) ;
printf( " TEMPS INIT / T RESOLUTION  %.5g \n", ( t3-t1) / t2); 
return; 
}




typ pscal(typ *u, typ *v, int dim)
{
  int i;
  typ res = 0;

  for ( i = 0; i< dim; i++)
    {
      res = res+ u[i]*v[i];
    }
  return res;
}


typ norm(typ *u, int dim)
{
  int j;
  typ norme = 0;

  for ( j=0; j < dim; j++)
    {
      norme = norme + u[j]*u[j];
    }
  norme = sqrt(norme);
  return norme;
}

void matvec(typ **matrix, int **index,typ *vinit,typ *vres,int dim)
{
  
  int i,j,j0;
  
  for ( i=0; i< dim; i++)
    {
      vres[i] = 0;
      for  ( j=1; j< index[i][0]; j++)
	{
	  j0 = index[i][j];//cout << j0 << "\n";
	  vres[i] = vres[i]+ matrix[i][j]*vinit[j0];
	}
      vres[i] = vres[i] + matrix[i][0]* vinit[i];
    }

}




void bicgstab2(typ **matrix, int **index, typ *x, typ *b, int dim, int maxit, typ epsilon, int which, bool verbose)
{
//  typ *r0 = (typ*) malloc(sizeof(typ)*dim);
  typ *ri_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *s = (typ*) malloc(sizeof(typ)*dim);
  typ *pi = (typ*) malloc(sizeof(typ)*dim);
  typ *pi_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *pstar = (typ*) malloc(sizeof(typ)*dim);
  typ *vi =  (typ*) malloc(sizeof(typ)*dim);
  typ *vi_1 = (typ*) malloc(sizeof(typ)*dim);
  typ *t = (typ*) malloc(sizeof(typ)*dim);
  typ *rtilde = (typ*) malloc(sizeof(typ)*dim);
  typ *sstar = (typ*) malloc(sizeof(typ)*dim);
  typ alphai,betai,omegai,rhoi_1,rhoi_2,norme;

  double fou = 0;
  int i,j;
   
  typ normeb;
  
  /* RPECOND MILU */
  
  // precond(matrix,index,pstar,pi,dim,which,i,firstp,graph_init);


  
  for (i=0; i<dim; i++)
    {
      ri_1[i] = b[i];// - sstar[i];
      rtilde[i] = b[i];// - sstar[i];
      x[i] = 0;//b[i];
//      cout << "Sm : " << b[i] << "\n";
    }
  

  normeb = norm(b,dim);
  if ( verbose)  printf(" \n NORME DU SECOND MEMBRE :  %f \n\n" , normeb); 
 
  
  for (i=1; i <= maxit; i++)
    {
      if ( verbose ) 
	{
	  printf(" ITERATION NUMERO :  %d",i) ;
//	  cout.flush();
	}
      rhoi_1 = pscal(rtilde,ri_1,dim);
//      cout << rhoi_1;
      if ( rhoi_1 == 0) 
	{
	  if (verbose) printf(" DO NOT CONVERGE ");
	  exit(1);
	}
      if ( i == 1)
	{
	  for ( j = 0; j< dim; j++)
	    {
	      pi[j] = ri_1[j];
	      rhoi_2 = rhoi_1 ;
	    }
	}
      else
	{
	  betai = (rhoi_1/rhoi_2)*(alphai/omegai);
	  for (j=0; j<dim; j++)
	    {
	      pi[j] = ri_1[j] + betai*(pi_1[j]-omegai*vi_1[j]);
	    }
	}
  
      precond(matrix,index,pstar,pi,dim,which);
	
      //plot(mat_init,index_init,dim);
   
      matvec(matrix,index,pstar,vi,dim);
      alphai = rhoi_1/(pscal(rtilde,vi,dim)+fou); 
      for ( j=0; j< dim; j++)
	{
	  s[j] = ri_1[j]-alphai*vi[j];
	}
      norme = norm(s,dim)/normeb;
      if ( verbose ) printf(" NORME 1 : %f \n", norme);
      if ( norme < epsilon) 
	{
	  for (j=0; j< dim; j++)
	    {
	      x[j] = x[j] + alphai*pstar[j];
	    }
	  if ( verbose) 
	    {
	      printf("NOMBRE D'ITERATIONS RES1: %d \n" ,i);
	      printf(" TAUX DE CONVERGENCE  %f " ,pow(norme,1/double(i)));
	    }
	  return;
	}
  
      precond(matrix,index,sstar,s,dim,which);
      matvec(matrix,index,sstar,t,dim);
      omegai = pscal(t,s,dim)/ (pscal(t,t,dim));
      if ( omegai == 0) 
	{
	  printf("\n OMEGAI = 0, FAILURE \n");exit(1);
	}
      for ( j=0; j<dim; j++)
	{
	  x[j] = x[j] + alphai*pstar[j]+omegai*sstar[j];
	  ri_1[j] = s[j]-omegai*t[j];
	  pi_1[j] = pi[j];
	  vi_1[j] = vi[j];
	}
      rhoi_2 = rhoi_1;
      norme = norm(ri_1,dim)/normeb; 
      if ( verbose) printf(" NORME 2  %f \n\n", norme);
//      cout << " EPS " << epsilon;

      if ( norme < epsilon) 
	{
	  if ( verbose) 
	    {
	      printf(" NOMBRE D'ITERATIONS RES2:  %d \n", i);
	      printf(" TAUX DE CONVERGENCE %f" ,pow(norme,1/double(i)));
	    }

	  return;
	}
    }
  printf(" NOMBRE MAX D'ITERATIONS ATTEINT ");
  return; 
}


void initialise (typ ***mat_init, int ***index_init,typ ***matt, int ***indext, int dim)
{
  extern int *level;   
*level =25;     
  extern  int *size;
  *size = dim;
  extern int *init;
  *init = 1;
  extern int oldlevel;
  extern int *sauvtaille;
  extern int taillei ;

  int i;//,j;

  extern  int *size_tab;
  extern int **tab;
  extern spec_noeud **spec_tab;
  extern int **indices;
      
  extern typ ***matKi;
  extern int ***indKi;
  
  
  int j;
  extern double temps;
  double tt1,tt2;
  
  tt1 = (double)clock() / CLOCKS_PER_SEC;
  if ( matKi[0] != NULL )
    {
      for (i=0; i< oldlevel;i++)
	{
	  free(indices[i]);
	  for (j=0;j<sauvtaille[i];j++)
	    { 
	  
	      free(matKi[i][j]);
	      free(indKi[i][j]);
	    }
	}
    }
    tt2 = (double)clock() / CLOCKS_PER_SEC;
    temps = temps+tt2-tt1;
//  printf(" \n CONSTRUCTION DU GRAPHE \n") ;exit(1);
//  fflush(NULL);


//  plot(*mat_init,*index_init,*size);
/*
    for ( j=0; j< *size; j++)
      {
	for ( int k=0; k < (*indext)[j][0];k++)
	  {
	    cout << (*matt)[j][k] << ", ";
	  }
	cout << "\n";
      }
    exit(1);
  */  

/*
  double t1,t2;
  
  t1 = (double)clock() / CLOCKS_PER_SEC;
*/

  reccursive_graph(matt,indext,mat_init,index_init,sigma,&size_tab,&tab,spec_tab,&lens,size,pack,level,init,delta);


//  plot(*LU,*indexLU,dim);


 // t2 = (double)clock() / CLOCKS_PER_SEC -t1;
  
//exit(1);


// printf("\n FIN DE LA CONSTRUCTION DU GRAPHE \n");
// printf("Temps d'initialisation : %.15g \n", t2);
//  fflush(NULL);
 // plot(*LU,*indexLU,dim);

  extern int factor_for_int;
  extern int factor_for_real;
  extern int *worki;
  extern double *workr;

  int dims = sauvtaille[*level-1];
  int **index = indKi[*level-1] ;
  double **Ki = matKi[*level-1];

   if ( dims > size_min) 
    {
      printf(" LAST LEVEL CONSTRUCTED IS TOO LARGE \n OR PROBLEM WITH THE ORIGINAL MATRIX");
      exit(1);
    }


  _F_REAL_8 * PR = workr;
  _F_REAL_8  * W  = PR + factor_for_real*dims;
  _F_REAL_8  * XX = W + 2*dims;
  _F_REAL_8  * YY = XX + dims;
  _F_REAL_8  * Cntl  = YY + dims;
  _F_REAL_8  * Rinfo = Cntl + 10;
 
 _F_INTEGER * PI  = worki;
 _F_INTEGER * Keep= PI + factor_for_int *dims;
  _F_INTEGER * Icntl= Keep + 20;
  _F_INTEGER * InfoUMS = Icntl + 20;

   _F_INTEGER ldw = factor_for_real*dims;
   _F_INTEGER liw = factor_for_int *dims;
  
 _F_INTEGER ZERO = 0 ;
//  const _F_INTEGER ONE = 1 ;
   _F_INTEGER nel,ii;
      
  
  // INITIALISATION
 
   //cout.flush() << "DEBUT CONTROLEURS\n";
   _umd2in(Icntl, Cntl, Keep);
   Icntl[5] = 0; // Preserve symmetry of the pattern
   Icntl[2] = 0; // Silence
   Icntl[1] = -1;
   Icntl[7] = 0; // No iterative refinement
   //cout.flush() << " FIN CONTROLEURS \n ";

  
   ii = nel = ZERO;
   
   for ( i=0;i< dims;i++)
     {
       PI[ii] = (_F_INTEGER) i+1;
       PI[(++ii)++] = (_F_INTEGER) i+1;
       PR[nel++] = (_F_REAL_8) Ki[i][0];
       for ( j=1; j< index[i][0];j++)
	 {
	   PI[ii] =  (_F_INTEGER) i+1;
	   PI[(++ii)++] =  (_F_INTEGER) index[i][j]+1;
	   PR[nel++] = (_F_REAL_8) Ki[i][j];
	 }
     }

   for (i=0;i<2*nel;i+=2) PI[2*nel+i/2]=PI[i+1];
   for (i=0;i<2*nel;i+=2) PI[i/2] = PI[i];
   for (i=0;i<nel;i+=1)   PI[nel+i] = PI[2*nel+i];


 //  ldw = 3*nel+factor_for_real*dims;
//   liw = 3*nel+2*dims+1+factor_for_int*dims;

   //cout.flush() << "DEBUT FAC\n";
    _umd2fa(&dims,&nel,&ZERO,&ldw,&liw,PR,PI,
	    Keep,Cntl,Icntl,InfoUMS,Rinfo);
    if (  InfoUMS[0] < 0 ) 
      {
	printf( " Error In fatorizer.  ERROR IS : %d\n", InfoUMS[0]) ;
	if ( InfoUMS[0] == -3 ) 
	  {
	    printf(" INCREASE factor_for_int IN solve/ygmres/slblk.dc : %d\n", InfoUMS[18]/dims);
	  }
	else if (  InfoUMS[0] == -4 )
	  {
	    printf( " INCREASE factor_for_real IN solve/ygmres/slblk.dc : %d \n", InfoUMS[20]/dims );
	  }
	else printf( " CHE CK UMD2FAC FOR INSTRUCTIONS \n");
	exit(1);
      }
    //cout.flush() << " FIN FAC\n ";
      
    //printf("FACTORIZED , %d,   %d\n",nel, dims); //exit(1);
  




  //  exit(1);
    if (InfoUMS[0] < 0) 
      {
	printf( "\nError in UMS2FA\n");
	exit(1);
      }

  oldlevel = *level;
  
  return;
}


void copy2(typ ***Ki, typ ***Ki1, int ***index, int ***ind1, int dim)
 {
  int i,j;
  
  (*Ki1) = (typ**) malloc(sizeof(typ*)*(dim));
  *ind1 = (int**) malloc(sizeof(int*)*(dim));
  for ( i = 0; i < dim; i++)
    {
      
     
      (*ind1)[i] = (int*) malloc(sizeof(int)*(*index)[i][0]);
      (*Ki1)[i] = (typ*) malloc(sizeof(typ)*(*index)[i][0]);
      (*ind1)[i][0] = (*index)[i][0];
      (*Ki1)[i][0] =  (*Ki)[i][0];
      for ( j = 1; j < (*index)[i][0]; j++)
	{
	  
	  (*ind1)[i][j] = (*index)[i][j];
	      (*Ki1)[i][j] = (*Ki)[i][j];
	}
    }
}
