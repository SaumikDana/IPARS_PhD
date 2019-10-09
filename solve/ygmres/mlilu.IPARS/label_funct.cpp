#include "library.h"

// USEFUL FUNCTIONS FOR THE GRAPH CONSTRUCTION DEFINITIONS

/****************************************************************/

double maxi(double  e1, double  e2)
{
  if ( e1 > e2) return e1;
  else return e2;
}

/*****************************************************************/

void look_4_neighbours(int i, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]))
{
  /* Get the neighbours of the node i in the remaining list of nodes*/
  
  int j,node,taille,pack;  
  bool isneigh;
  
  pack = (*size_tab)[9];
  int *Nodes = (*tab)[0];
  int size_Nodes = (*size_tab)[8];
  ensemble *Neighbours = (*lens)[0];

// Loop over the nodes of the list  

  for (j=0; j<size_Nodes; j++)
    {
      node = Nodes[j];
      
      // Check if the corresponding node is a neighbour 
      
      isneigh = Is_neighbour(i,node,size_tab, lens);
      
      // if it is a neighbour add it to the neighbour list and check memory

      if (isneigh)
	{
	  taille = (Neighbours[i]).nbre;
	  check_size(&((Neighbours[i])).liste,taille,pack);
	  ((Neighbours[i]).liste[(Neighbours[i]).nbre++])=node;
	   (*lens)[0] = Neighbours;
	}
      
    }
  (*lens)[0] = Neighbours;
  
  kill(Nodes);
  kill(Neighbours);
}

/*****************************************************************/

bool Is_neighbour(int i, int j, int **size_tab,  ensemble *(*lens[3]))
{

  /* Look if node j is a neighbour of node i */
  
  int loop; 
  bool res;
  
  res = false;
  ensemble *neighbours = (*lens)[0];
  
  for (loop =0; loop < neighbours[i].nbre; loop++)
    {
      if ( ((neighbours[i]).liste[loop]) == j )
	{
	  res = true;
	  break;
	}
    }
   
 
  return res;
}
      
/***************************************************************/

// Function evaluates the Lambda value related to the node numbre i


int lambda_i(int i, int **size_tab, int **tab[3], spec_noeud **spec_tab)
{
  int k,node,lambda,pos;

  spec_noeud *liste = (*spec_tab);
//  int size_Nodes = (*size_tab)[8];
//  int *Nodes = (*tab)[0];

  lambda=0;

  /* Loop over the nodes with edges >= 0 */
  
  for (k=0; k< liste[i].nbre; k++)
    {
      node = (liste[i]).liste_noeud[k].num_noeud;
      //	  for ( l=0; l < liste[node].nbre; l++)
      //{
      if  ((liste[node]).where == -1)
	{
	  pos = (liste[i]).liste_noeud[k].post;
	  //lambda = lambda + ((liste[node]).liste_noeud[l]).edge;
	  //	      cout << "NODES FOUND :" << node << "\n";
	  lambda = lambda + ((liste[node]).liste_noeud[pos]).edge;
	  //break;
	}
      //	    }
    }
  //  cout << "For node " << i << " lambda = " << lambda << "\n";
  //  kill(Nodes);
  kill(liste);
  return lambda;
}

/****************************************************************/

/* Functions to evaluate Optimal Parents */


// Function returns 0 if edge exists, 1 otherwise 

int chi(int r, int s, ensemble *(*lens[3]))
{

  int k,res,node;
  ensemble *Neighbours = (*lens)[0];

  if ( r == s ) return 0;
  res=1;
  for ( k=0; k<Neighbours[r].nbre; k++)
    {
      node = Neighbours[r].liste[k];
      if ( node == s) 
	{
	  res=0;
	  break;
	}
    }
  
  return res;
}



/* Functions to evaluate Optimal Parents */


// Function returns 0 if edge exists, 1 otherwise

int chi(int r, int s, spec_noeud **spec_tab)
{

  int k,res;
 
  spec_noeud *liste = (*spec_tab);

  spec_noeud temp = liste[r];
  noeud *liste_temp = (liste[r]).liste_noeud;
  noeud cell;

  res=1;
  for ( k=0; k<temp.nbre; k++)
    {
      cell = liste_temp[k];
      if ( cell.num_noeud == s)
        {
          res=0;
          break;
        }
    }
  kill(liste_temp);
  kill(liste);
   
  return res;
}



 
/****************************************************************/
 
// Function returns the number of parents of node i

int np(int i, ensemble  *(*lens[3]))
{
  int nombre;
  ensemble *liste = (*lens)[1];

  nombre = (liste[i]).nbre;
  kill(liste);
  return nombre;
}

/****************************************************************/

int ncn(int i, int j, ensemble *(*lens[3]))
{

  /* Returns the number of common neighbours */

  int *l1, *l2;
  int k,kk,nb1,nb2,res,neigh;
  
  ensemble *liste = (*lens)[0]; // neighbours 


  if ( liste[i].nbre < liste[j].nbre )
    {
      nb1=(liste[i]).nbre;
      nb2=(liste[j]).nbre;
      l1= (liste[i]).liste;
      l2 = (liste[j]).liste;
    }
  else
    {
      nb2=(liste[i]).nbre;
      nb1=(liste[j]).nbre;
      l2= (liste[i]).liste;
      l1 = (liste[j]).liste;
    }
 
  res=0;
  for (k=0; k<nb1; k++)
    {
      neigh = (l1[k]);
      for ( kk=0; kk< nb2; kk++)
	{
	  if ( ( l2[kk] == neigh) ) 
	    {
	      res++;
	      break;
	    }
	}
    }
  kill(liste);
  kill(l1);
  kill(l2);
  return res;
}


/****************************************************************/

// Funnction check the existence of an edge between i and j
// Returns the value and the position in the list

void check_edge(int i, int j, spec_noeud **spec_tab, int **val)
{
  int k,nbi,n;
  noeud *l;
  noeud node;
  
  spec_noeud *liste = (*spec_tab);


  if ( liste[i].nbre < liste[j].nbre)
    {
      nbi = liste[i].nbre;
      l =  (liste[i]).liste_noeud;
      n= j;
    }
  else
    {
      nbi = liste[j].nbre;
      l =  (liste[j]).liste_noeud;
      n = i;
    }

  (*val)[0]= -1 ;
  (*val)[1] = -1;

  for ( k =0; k< nbi; k++)
    {
      node = l[k];
      if ( node.num_noeud == n)
	{
	  (*val)[0]= node.edge;
	  (*val)[1] = node.post ;
	  break;
	}
    }
  kill(liste);
  kill(l);
}

/****************************************************************/

// Function returns the number of neighbours of node i

int nb(int i, ensemble *(*lens[3]))
{
  int nombre;
  ensemble *Neighbours = (*lens)[0];
  nombre = (Neighbours[i]).nbre;
  return nombre;
}

/****************************************************************/

// Formula that allows to evaluate the maximum number of new edges in the next virtual graph

int max_new_edges(int i, spec_noeud **spec_tab, ensemble *(*lens[3]))
{
  int ncni, chii, res, j, l, npi, node,node1;
  
  
  ensemble *Parents = (*lens)[0];
//  spec_noeud *liste = (*spec_tab)[3];

  res=0;
  ncni = 0;
  chii = 0;
  npi=Parents[i].nbre;
 
 /* cout << " LES VOIS ";
  for ( j=0; j< npi; j++)
    {
      cout << Parents[i].liste_noeud[j].num_noeud << " , " ;
    }
  cout << "\n";
  */
  
  if ( npi == 1 )
    {
      res = 0;
    }
  else
    {
      for (j=0; j< npi; j++) 
	{
	  node = Parents[i].liste[j];
	  // cout << " VOISINS DES VOIS ";
	  /*
	    
	      for (int  jj=0; jj< Parents[node].nbre; jj++)
	      {
	      //  cout << Parents[node].liste_noeud[jj].num_noeud << " , " ;
	      if (liste[Parents[node].liste_noeud[jj].num_noeud].where == 1 ) 
	      {
	      //  cout << "DSDSD" ;
	      //xit(1);
	      }
	      }
	  
	//  cout << "\n";
	  */
	  
	  /*  cout << " CONNECTIONS ";
	      for (  jj=0; jj< liste[node].nbre; jj++)
	      {
	      cout << liste[node].liste_noeud[jj].num_noeud << " , " ;
	      if (Parents[Parents[node].liste_noeud[jj].num_noeud].where == 1 ) 
	      {
	      cout << "DSDSD" ;
	      exit(1);
	      }
	      }
	      cout << "\n";
	  */
	  
	  ncni=ncni + ncn(i,node,lens);
	  for ( ( l=0 ); l<npi; l++)
	    {
	      node1 = Parents[i].liste[l];
	      chii = chii + chi(node,node1,lens);
	    }
	}
      
    //  cout << "NCNI" << ncni <<  "   CHII  " << chii ; 

      res = 2*npi*(npi-1)-2*ncni - chii;
    }
  kill(Parents);
  
  return res;
}


int max_new_edges2(int i, int k, spec_noeud **spec_tab, ensemble *(*lens[3]))
{
  int res, j, npi, nbi;
  
  ensemble *Parents = (*lens)[1];
  
  res=0;
  ensemble Pi = Parents[i];
  int lpi;
  /*
    for (j=0; j< Pi.nbre; j++) 
    {
    lpi= (Pi).liste[j];
    ncni=ncn(i,lpi,lens);
      res = res -ncni;
      for ( (l=j+1); l<Pi.nbre; l++)
      {
      lpl = (Pi).liste[l];  
      chii = chi(lpi,lpl,spec_tab);
      res= res - chii;
      }
      }
  */
      
  res = res - 2*ncn(i,k,lens);

  
  for ( j=0; j< Pi.nbre; j++) 
    {
      lpi= (Pi).liste[j];
      res = res -  chi(k,lpi,lens);
    }
  
  //  npi = np(i,lens)+1;

  npi=1;
  nbi = nb(i,lens);
  
  res = res + 2*npi*(nbi-1);
 kill(Parents);
 
 return 2*res;
}









/****************************************************************/


// Evaluates the eta function given in page 15

int eta( int i, int j, int **size_tab, spec_noeud **spec_tab, ensemble *(*lens[3]))
{
  int res = 0;
  int edge,edge1, ss, s,node,pos;

  ensemble *Neigh = (*lens)[0];
  spec_noeud *liste = *spec_tab; 

  
 
   for ( s=0; s < liste[i].nbre; s++)
     {
       node = liste[i].liste_noeud[s].num_noeud;
       pos =  liste[i].liste_noeud[s].post;
       if  ( liste[node].where < 1 )
	 {
	   edge = liste[i].liste_noeud[s].edge;
	   for ( ss=0; ss < liste[j].nbre; ss++)
	     {
	       if ( liste[j].liste_noeud[ss].num_noeud == node )
		 {
		   edge1 =  liste[j].liste_noeud[ss].edge;
		   res = res+1;
		   if ( edge1 > 0 )
		     {
		       res = res+1;
		       if ( edge > 0 ) 
			 {
			   res = res+2;
			   if ( node == j )
			     {
			       if ( liste[j].liste_noeud[pos].edge > 0 )  res = res +16;
			     }
			 }
		       else if ( edge > 0 ) res = res+1;
		       else if ( node == j ) res = res+4;
		       break;
		 }
	     }
	     }
	 }
     }

  return res;
}







int eta( int i, int j, int k, int **size_tab, spec_noeud **spec_tab, ensemble *(*lens[3]))
{
  int res = 0;
  int node;
  int *val, *val1, edge, edge1, s,m,jj,ii;

 

  ensemble *neighbours = (*lens)[0];

  val= (int*) malloc(sizeof( int)*2);
  val1 = (int*) malloc(sizeof( int)*2);

  
  for ( m=0; m< 3; m++)
    {
      ii = i;
      if ( m == 0)    jj = j;
      else if ( m==1 )jj= k;
      else 
	{
	  jj=k;
	  ii =j;
	}
      
      check_edge(ii,jj,spec_tab,&val);
      edge = val[0];
      
      if ( edge > 0 )
	{
	  res = res + 16;
	}
      else  if ( edge == 0 ) res = res +4;
      
      for ( s=0; s< neighbours[ii].nbre; s++)
	{
	  node = neighbours[ii].liste[s];
	  
	  check_edge(ii,node,spec_tab,&val);
	  check_edge(jj,node,spec_tab,&val1);
	  edge = val[0];
	  edge1=val1[0];
	  
	  if (edge > 0)
	    {
	      if ( edge1 > 0) 
		{
		  res=res+4;
		}
	      else  if ( edge1 == 0 ) res = res + 2;
	    }
	  else
	    {
	      if ( edge1 > 0) 
		{
		  res=res+2;
		}
	      else  if ( edge1 == 0 ) res = res + 1;
	    }
	}
    }
  free(val);
  free(val1);
  
  return res;
}













/****************************************************************/

// Function look if j is a parent of node i

int check_parents(int i, int j,  ensemble *(*lens[3]))
{
  int k,res;
  
 ensemble *Parents = (*lens)[1];

  int nbi = Parents[i].nbre;
 int *l = Parents[i].liste;

  res = -1;
  for ( k=0; k< nbi; k++)
    {
//      cout << l[k].num_noeud << " , " ;
      if ( l[k] == j) 
	{
	  res=k;
	  break;
	}
    }
//  cout << "\n";
//  cout << "POUR NOUDS " << i << ", " << j << "    RES = " << res << "\n";
  kill(Parents);
  return res;   
 }
    
/****************************************************************/


int nui( int i, int r,  int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn)
{
  
  int npi, nbi, ncni, res;
 
  npi = 1;    
  nbi = nb(i,lens);
  //np(i,spec_tab);
  //  cout << "NPI " << npi << "\n";
  
 // ncni = ncn( i, r,lens);
  
  ncni = ncn;

 // cout << "dans nui  valeur de etai pour le noeud " << i << " : " << npi*(nbi-1)<< "\n";

 res = 2*(npi*(nbi-1)-ncni);
 
 // cout << "dans nui  valeur de nui pour le noeud " << i << " : " << res << "\n";
  return res;
}



int  nui( int i, int r, int s, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn)
{

  int npi, nbi, ncni, chii, res;

  npi = 2;    
   nbi = nb(i,lens);
   //np(i,spec_tab);
   //  cout << "NPI " << npi << "\n";
  if ( r == s)
    {
      //ncni =ncn( i, r,lens);
      ncni = ncn;
      npi=1;
      chii =0;
    }
  else
    {
    //  nbi = nb(i,lens);
      /*
      ncni = ncn( i, r,lens);
      ncni = ncni + ncn(i,s,lens);
      */
      ncni = ncn;
      chii =  2*chi(r,s,lens);//+ chi(s,r,lens);
    }
 // cout << "dans nui  valeur de etai pour le noeud " << i << " : " << npi*(nbi-1)<< "\n";

  res = 2*(npi*(nbi-1)-ncni)-chii ;
 // cout << "dans nui  valeur de nui pour le noeud " << i << " : " << ncni<< "\n";
  return res;
}


int nui( int i, int r, int s, int t, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn)
{

  int npi, nbi, ncni, chii, res;
  
  
  
  
  
  npi = 3;         //np(i,spec_tab);
  //  cout << "NPI " << npi << "\n";
 
  nbi = nb(i,lens);
  /*
  ncni = ncn( i, r,lens);
  ncni = ncni + ncn(i,s,lens)+ncn(i,t,lens);
  */
  ncni = ncn;
  chii =  chi(r,s,lens)+chi(r,t,lens)+chi(t,s,lens);
  
//  cout << "dans nui  valeur de etai pour le noeud " << i << " : " << chii << "\n";
  /*
    if( r == s)
    {      ncni =0;
    chii =0;
    }
  */  

  res = 2*(npi*(nbi-1)-ncni-chii);
  //  cout << "dans nui  valeur de nui pour le noeud " << i << " : " << res << "\n";
  return res;
}




/*****************************************************************/

void update_neigh(int r, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  extern int taillei;
  ensemble *Neighbours = (*lens)[0];
  spec_noeud *liste = (*spec_tab);
  ensemble *Parents = (*lens)[1];
     
  int taille,k,kk,kkk,kkkk,j,i;
  
  int compteur = 0;
  extern int *temporaire2;
  //  cout << " Elimination du noeud : " << r << "\n";
  

  for ( k=0; k< Neighbours[r].nbre; k++)
    {
      compteur = 0;      
      i  =  (Neighbours[r].liste[k]);
      for ( kk =0; kk < Neighbours[i].nbre; kk++)
	{
	  j = (Neighbours[i]).liste[kk];
	  if  ( j  != r  )
	    {
	      if ( compteur > taillei-1 ) 
		{
		  printf( " MEMOIRE INSUFFISANTE TEMP VOIS  \n ");
		  exit(1);
		}
	      temporaire2[compteur++] = j;
	    }
	}
      
      for ( kkk =0; kkk< Parents[r].nbre;kkk++)
	{
	  if ( compteur > taillei-1 ) 
	    {
	      printf( " MEMOIRE INSUFFISANTE TEMP VOIS  \n ");
	      exit(1);
	    }
	  if ( Parents[r].liste[kkk] != i )
	    {
	      temporaire2[compteur++] = Parents[r].liste[kkk];
	    }
	}
      
 //     cout.flush() << " COMPT " << compteur << "\n";
      if ( compteur != 0 )
	{
	  sort(temporaire2, temporaire2+ compteur);
	  taille=unique(temporaire2, compteur);
	  compteur= taille;
	  (Neighbours[i]).liste = (int*) realloc((Neighbours[i]).liste,sizeof(noeud)*compteur);
	  Neighbours[i].nbre = compteur;
	  for (kk=0; kk < compteur ; kk++)
	    {
	      ((Neighbours[i]).liste[kk]) = temporaire2[kk];
	    }
	}
      
    }

  for ( k=0; k< Parents[r].nbre; k++)
    {
      j  =  (Parents[r].liste[k]);
      compteur =0;
      for ( kk =0; kk < liste[r].nbre; kk++)
	{
	  i = (liste[r]).liste_noeud[kk].num_noeud;
	  if (( i != r ) && (  i != j))
	    {
	      if ( compteur > taillei-1 ) 
		{
		  printf( " MEMOIRE INSUFFISANTE TEMP VOIS   \n");
		  exit(1);
		}
	      temporaire2[compteur++] = i;
	    }
	}
      for (kkk=0; kkk < Neighbours[j].nbre; kkk++)
	{
	  i = Neighbours[j].liste[kkk];
	  if ( compteur > taillei-1 ) 
	    {
	      printf(" MEMOIRE INSUFFISANTE TEMP VOIS  \n " );
	      exit(1);
	    }
	  if ( i != r )
	    {
	      temporaire2[compteur++] = i;
	    }
	}
      if ( compteur != 0 )
	{
	  sort(temporaire2, temporaire2+ compteur);
	  taille=unique(temporaire2, compteur);
	  compteur= taille;
	  (Neighbours[j]).liste = (int*) realloc((Neighbours[j]).liste,sizeof(noeud)*compteur);
	  Neighbours[j].nbre = compteur;
	  for (kk=0; kk < compteur ; kk++)
	    {
	      ((Neighbours[j]).liste[kk]) = temporaire2[kk];
	    }
	}
      
    }
      
  (*lens)[0] = Neighbours;
  return;
}  

