 //#define _LABEL_T_
#include "library.h"
//#include "vector.h"

#include <iostream.h>


//template<class typ>
void Init_Graph(typ **matrix, int **index, double sigma, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  
  /* Max is a vector containig the maximum on the lines and the colums */

  /* SIZE OF THE ELEMENTS GIVEN BY INDEX[0] AND SIZE IS 1+NUMBER OF ELEMENTS OF COURSE, DIAGONAL ELEMENT IS IN 2nd POSITION*/
 
  extern double thresh;
  int r,s;
  int line, col;
  int size = (*size_tab)[0];
  typ *max = (typ*) malloc(sizeof(typ)*size);
  double valij,valji;
  spec_noeud **elem1 = NULL; //(spec_noeud**) malloc(sizeof( spec_noeud*));
  ensemble *Parents = (*lens)[1];
  ensemble *Neigh = (*lens)[0];
  ensemble *Opt = (*lens)[2];
  int taille, pack;
  int resij,resji,k,taille2;
  bool cree;
  

  pack = (*size_tab)[9];
  int *Nodes = (*tab)[0];
  spec_noeud *liste =  (*spec_tab);
  int *size_Nodes = *size_tab + 8;
  int ind;

extern double thresh;
 //plot(matrix,index,size);

 
  /* Search for maximum in line and colums */
  /* Has to be replaced by the parallel version of ipars */
  
 /***********************************************************************/ 
  /* Prevent from errors */


  if ((sigma > 1) || (sigma <= 0)) 
    {
      printf("FATAL ERROR LITTLE JOKER \n\n  Sigma belongs to the [0,1] interval \n\n");
      exit(1);
    }

/************************************************************************/

// Loop to determine the maximum of the colums

  for ( line=0; line< size; line++)
    {
      Nodes[line]=line;
      liste[line].where = -1;
      Parents[line].nbre = 0;
      Parents[line].liste = NULL;
      Neigh[line].nbre = 0;
      Opt[line].nbre = 0;
      Opt[line].liste = NULL;
      liste[line].nbre = 0;
      liste[line].liste_noeud = (noeud*) malloc(sizeof(noeud)*index[line][0]*3);
      Neigh[line].liste = (int*) malloc(sizeof(int)*index[line][0]*3);
      *(max+line)=matrix[line][1];     
      for ( col=1; col< (index[line])[0]; col++)
	{
	  *(max+line)= maxi(max[line],matrix[line][col]); 
	}
    }
  
  // Loop that calculates the values of the edges e_rs and allows memory

  for (r=0; r< size; r++)
    {
      (elem1)= &liste;
      for ( s=1; s< (index[r])[0]; s++)
	 {
	   valij=matrix[r][s];
	   if ( fabs( valij / (max[r])) > thresh)
	     {
	       
	       ind = (index[r])[s];
	       taille = (*(*elem1+r)).nbre;
	       taille2 = (*(*elem1+ind)).nbre;
	       cree = false;
	       for ( k=0; k< taille; k++)
		 {
		 
		   if ( (*(*elem1+r)).liste_noeud[k].num_noeud == ind)
		     {
		       cree = true;
		       break;
		     }
		 }
	       if ( ! cree ) 
		 {
		   resij = 0;
		   resji = 0;
		   valji = 0;
		   bool found = false;
		   for ( k=1; k< index[ind][0]; k++)
		     {
		       if ( index[ind][k] == r )
			 {
			   valji = matrix[ind][k];
			   found = true;
			   break;
			 }
		     }
		   
		   
		   if ( ( valij >= sigma*(*(max+r))) ) //  ||  (valji >  sigma*(*(max+ind))) )
		     {
		       resij = 1;
		     }
		   
		   if ( (valji >  sigma*(*(max+ind))) && ( found == true))
		     {
		       resji = 1;
		     }
		   (*(*elem1+r)).liste_noeud[(*(*elem1+r)).nbre].edge=resij;
		   (*(*elem1+ind)).liste_noeud[(*(*elem1+ind)).nbre].edge=resji;
		   (*(*elem1+r)).liste_noeud[(*(*elem1+r)).nbre].num_noeud = ind;
		   (*(*elem1+ind)).liste_noeud[(*(*elem1+ind)).nbre].num_noeud = r;
		   (*(*elem1+r)).liste_noeud[(*(*elem1+r)).nbre].post = taille2;
		   (*(*elem1+ind)).liste_noeud[(*(*elem1+ind)).nbre].post = taille;
		   (*(*elem1+r)).where = -1;
		   Neigh[r].liste[Neigh[r].nbre]= ind;
		   
		   (*(*elem1+ind)).where =-1; 
		   Neigh[ind].liste[Neigh[ind].nbre] = r;
		   (*(*elem1+ind)).nbre++;
		   Neigh[ind].nbre ++;
		   (*(*elem1+r)).nbre++;
		   Neigh[r].nbre ++;
	      
		 }
	     }
    }
    }
  
  //      cout << "\n";
 
  // Create initial Neighbours
  
  
  /* Free Memory */
  
  
  //int node;
  // plot(matrix,index,size);
  
 /*
  cout.flush() << "GRAPHE \n" ;
  for ( s=0; s< size; s++)
     {
       //    cout << liste[s].restant;
       for ( r = 0; r < liste[s].nbre; r++)
	 {
	   //node =  (liste[s]).liste_noeud[r].num_noeud;
	   
	   cout.flush() << " " << (liste[s]).liste_noeud[r].edge<< " " ;
	   //	  cout.flush() << " " << liste[node].liste_noeud[liste[s].liste_noeud[r].post].num_noeud - s << " " ;
	 }
       cout << "\n";
     }
*/
 // exit(1);
 
 /*
  cout << "GRAPHE \n" ;
  for ( s=0; s< size; s++)
    {
      for ( r = 0; r < liste[s].restant; r++)
	{
	  cout.flush() << " (" << liste[s].liste_noeud[r].edge << ","<<  liste[s].liste_noeud[r].num_noeud << ")" << " ";
	}
      cout.flush() << "\n";
    }
  
    
  exit(1);
 
  cout.flush();
  int stop;
  cin >> stop;
 */
  (*spec_tab) = liste;
  (*tab)[0] = Nodes;
  (*lens)[0] = Neigh;
  
  kill(elem1);
  kill(Parents);
  kill(Neigh);
  kill(Nodes);
  kill(liste);
  kill(size_Nodes);
  free(max);
}


/*********************************************************/


/********************************************************/

/* Mark the nodes that were checked */


bool compc2(const nlamb x, const nlamb y)
{
    if (x.val < y.val) return true;
    else return false;
}




//template<class typ>
void Mark_Nodes(typ **matrix, int **index, double sigma, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  
  extern int *temporaire;
  extern int taillei;
  int i,j,k,jj,node,lambda,edge;
  int node2,node3,kk,pos,post;
//  int *val = (int*) malloc(sizeof( int)*2);
  int pack = (*size_tab)[9];
  int size = (*size_tab)[0];
  bool isF,reord;
  extern bool compc2(nlamb x, nlamb y);

  ensemble *Neighbours = (*lens)[0];
  spec_noeud *liste = (*spec_tab);
  int *C_nodes = NULL ; //(int*) malloc(sizeof( int));
 // CHANGER  int *Nodes = (int*) malloc(sizeof( int));
  int *size_C = (*size_tab)+2;
  int *size_Nodes = (*size_tab) + 8;

  int *Nodes_val = (int*) malloc(size*sizeof(int)); 

  nlamb *lamb = (nlamb*) malloc(1000*sizeof(nlamb));

  Init_Graph(matrix,index,sigma,size_tab,tab, spec_tab,lens);
  update_ptr(&liste,&(*spec_tab));
 // CHANGER  update_ptr(&Nodes, &((*tab)[0]));
 
// int *tmp = (int*) malloc(sizeof(int));
  
int ttmp = 0;
  int tmps = ttmp;
    int cpt = 0;
    int cptf = 0;

  reord = false;
  // Create initial neighbours
  
  /*
    (*spec_tab)[0] = (*spec_tab)[3];
    Neighbours = (*spec_tab)[3];
  */
  
  for (i=0; i<size; i++)
    {
      Mark_opt_par(i,size_tab,tab, spec_tab,lens);
    }
  
  int tempo =0 ;
  int taille_table = 1;
  lamb[0].nbre =1;
  bool  nouw;
  
  i=0;
  tempo = lambda_i(i,size_tab,tab,spec_tab);
  Nodes_val[i] = tempo;
  (lamb[0]).val = tempo;
  (lamb[0]).liste = (int*) malloc(sizeof(int));
  ((lamb[0]).liste)[0] = 0;

 

  for ( i=1; i < size; i++)
    {
      tempo = lambda_i(i,size_tab,tab,spec_tab);
      Nodes_val[i] = tempo;
      nouw = true;
      for ( int kk =0; kk < taille_table; kk++)
	{
	  if (lamb[kk].val == tempo )
	    {
	      nouw = false;
	      jj = kk;
	      break;
	    }
	}
      /*
	cout << " FOR NODE " << i << " nouv? " << nouw << " LAMB " << tempo << "\n";
      */
      if ( nouw )
	{
	 // lamb = (nlamb*) realloc(lamb,sizeof(nlamb)*(taille_table+1));
	  if ( taille_table > 999 ) { printf(" ERREUR LAMBD ALLOCATION");exit(1);}
	  lamb[taille_table].nbre = 1;
	  lamb[taille_table].val = tempo;
	  lamb[taille_table].liste = (int*) malloc(sizeof(int));
	  lamb[taille_table].liste[lamb[taille_table].nbre-1] = i;
	  taille_table++;
	}
      else
	{
	  lamb[jj].liste = (int*) realloc(lamb[jj].liste,sizeof(int)*(lamb[jj].nbre+1));
	  lamb[jj].liste[lamb[jj].nbre++] = i;
	}
    }
 
 // qsort(lamb,taille_table,sizeof(nlamb),comp);


  if ( lamb == NULL ) exit(1);
  if ( lamb+taille_table == NULL ) exit(1);

  sort(lamb,
       lamb+
       taille_table,
       compc2);

  /*
  for ( j=0; j< taille_table; j++)
    {
      cout << " NB ELEM " << lamb[j].val << "\n" << "NOEUDS ";
      for ( jj=0; jj <  lamb[j].nbre ; jj++)
	{
	  cout << lamb[j].liste[jj] << ", ";
	} 
      cout << "\n";
    }
 
  //exit(1);
 */
/*
  int cont;
   cin >>  cont;
   cout.flush();
*/ 
  node = 
    Next_node(size_tab,tab,spec_tab,&lamb,&taille_table,Nodes_val);





//  Nodes = (*tab)[0];
  
//  cout << "Noeud choisi " << node << "\n";
 
 
    while ( node != -1 )
    {
      
   /*   
      for ( j=0; j< size; j++)
	{
	  cout << " VOIS DE " << j << "  :  ";
	  for ( k=0; k< Neighbours[j].nbre; k++) cout << Neighbours[j].liste[k] << " , ";
	  cout << "\n";
	}
     */ 



      lambda = Nodes_val[node];
      ttmp=0;
      if (lambda > 0)
	{
	  check_size(&C_nodes,*size_C,pack);
	  C_nodes[(*size_C)++] = node;

	  liste[node].where = 0;
	  (*tab)[2] = C_nodes;
//
	  for ( j=0; j < liste[node].nbre; j++)
	    {
	      node2= ((liste[node]).liste_noeud[j]).num_noeud;
	      post = (liste[node]).liste_noeud[j].post;
	      if ( liste[node2].where == -1)
		{
		  //	         cout << "Nouveau noeud "<< node2 << "\n";

		  // check_edge(node2,node,spec_tab,&val);
		  
		  edge = liste[node2].liste_noeud[post].edge;
		//           cout << "verif de l'arete: "<< edge << "\n";
		  
		  if ( edge > 0 ) 
		    {
	//	      cout << " ON VERIFIE " << node2 << "\n";
		      isF = Check_if_F_node(node2 ,size_tab,tab, spec_tab,lens);
		      
		      if (isF)
			{ 
			  Nodes_val[node2] = -10;
	//		  cout << " ISF " << node2 << "\n";
			  (*size_Nodes)--;
			  temporaire[ttmp++]= node2;
			   
			  }
		    }
		  //   REALLOC THE RESTING NODES
		}
	    }

	  tmps = ttmp;

	  for (j=0; j< Neighbours[node].nbre; j++)
	    {


	      node2 = Neighbours[node].liste[j];
	      /*
		for ( k=0; k < liste[node2].nbre; k++)
		{
		if ( liste[(liste[node2]).liste_noeud[k].num_noeud].where == -1)
		{
		tmp = (int*) realloc(tmp, sizeof(int)*(tmps+1));
		tmp[tmps++] = (liste[node2]).liste_noeud[k].num_noeud;
		}
		}
	      */
	      if ( liste[node2].where == -1 )
		{
	//	  cout << "MARQUE 1 \n";
	//	  Mark_opt_par(node2,size_tab,tab,spec_tab,lens);
		  /*
		  tmp = (int*) realloc(tmp, sizeof(int)*(tmps+1));
		  tmp[tmps++] = node2;
		  */
		  if ( tmps +1 > taillei ) 
		    {
		      printf( " MEMOIRE INSUFFIASANTE TEMPORAIRE  ");
		      exit(1);
		    }
		  temporaire[tmps++]=node2;
		}
	    }
	  /*
	  sort( temporaire+ttmp, temporaire+tmps);
	  tmps = unique(temporaire+ttmp, tmps-ttmp);
	  tmps = ttmp+tmps;
	  */
	  for ( j=0; j< ttmp; j++)
	    {

// CHANGER
	 
	      for ( k=0; k < Neighbours[temporaire[j]].nbre; k++)
		{
		  
	//	  node2 = Neighbours[tmp[j]].liste_noeud[k].num_noeud;
		  
		  node2 = Neighbours[temporaire[j]].liste[k];
		  
		  if ( liste[node2].where == -1 )
		    {
		      /*		      
					      tmp = (int*) realloc(tmp, sizeof(int)*(tmps+1));
					      tmp[tmps++] = node2;
		      */	      
		      if ( tmps +1 > taillei ) 
			{
			  printf( " MEMOIRE INSUFFIASANTE TEMPORAIRE : \n" );
			  exit(1);
			}
		      temporaire[tmps++] = node2;
		      //      cout << "TEMPORAIRE 2 " << tmps << "\n";
		 //     Mark_opt_par(node2,size_tab,tab,spec_tab,lens);
		    }
		  
		}
	      
	    }
	  
	  for ( k=0; k< ttmp; k++)
	    {
	      update_neigh(temporaire[k],size_tab, tab, spec_tab,  lens );
	    }
	  
	  ensemble *Opt = (*lens)[2];
	  /*
	  if ( tmps < ttmp ) exit(1);
	  cout.flush() << " TMP " << ttmp << " ,  " << tmps<< "\n";*/
	  sort( temporaire+ttmp, temporaire+tmps);
	  /*
	  for ( k=0; k< tmps ; k++)
	    {
	      cout << temporaire[k] << " | " ;
	    }
	  cout << "\n";
	  */
	  tmps = unique(temporaire+ttmp, tmps-ttmp)+ttmp;
	  //cout.flush() << " TMP2 " << ttmp << " ,  " << tmps<< "\n";
	  /*
	  if ( tmps < ttmp ) {
	    exit(1);
	    cout.flush() << " TMP2 " << ttmp << " ,  " << tmps<< "\n";

	  }
	  */
	  int add = tmps;
	  for ( j =ttmp;j<tmps;j++)
	    {
	      Mark_opt_par(temporaire[j],size_tab,tab,spec_tab,lens);
	      for ( int ll=0; ll< Opt[temporaire[j]].nbre; ll++)
		{
		  if ( add+1 > taillei )
		    {
		      printf(" MEMOIRE INSUFFIASANTE TEMPORAIRE : \n");
		      exit(1);
		    }
		  temporaire[add++] = Opt[temporaire[j]].liste[ll];
		}
	    }
	  
	  //	  int *temporaire = (int*) malloc(sizeof(int)*(-ttmp+tmps));
	  
	  //	  cout.flush() << " TMPS " << tmps << "\n";
//	  cout.flush() << " TMP " << ttmp << " ,  " << add << "\n";
	  sort( temporaire+ttmp, temporaire+add);
	  tmps = unique(temporaire+ttmp, add-ttmp)+ttmp;

	
	  for ( j=ttmp; j< tmps; j++)
	    {
	      node3 = temporaire[j];
	      /*	      for ( k=0; k< Neighbours[node2].nbre; k++)
			      {
			      node3 = Neighbours[node2].liste_noeud[k].num_noeud;
			      if ( liste[node3].where == -1 )
			      }`
	      */
	      tempo = lambda_i(node3,size_tab,tab,spec_tab);
	//      cout << " LAMBDA REEVALUE " << node3 << " VAL " << tempo << "\n";
	      if ( Nodes_val[node3] != tempo )
		{
		  Nodes_val[node3] = tempo;
		  nouw = true;
		  for ( jj=0; jj< taille_table; jj++)
		    {
		      if ( lamb[jj].val == tempo )
			{
			  nouw = false;
			  pos = jj;
			  break;
			}
		    }
		  if ( !nouw )
		    {
		      lamb[pos].liste = (int*) realloc(lamb[pos].liste,sizeof(int)*(lamb[pos].nbre+1));
		      lamb[pos].liste[lamb[pos].nbre++] = node3;
		    }
		  else
		    {
		      reord= true;
		      //lamb = (nlamb*) realloc(lamb,sizeof(nlamb)*(taille_table+1));
		      if ( taille_table > 999 ) {printf("ERREUR ALLOC LAMBDA");exit(1);}
		      lamb[taille_table].val = tempo;
		      lamb[taille_table].nbre = 0;
		      lamb[taille_table].liste = (int*) malloc(sizeof(int));
		      lamb[taille_table].liste[lamb[taille_table].nbre++] = node3;
		      taille_table++;
		    }
		}
	      
	    }
	  
	  
	  
	  
	  if ( reord) sort(lamb,lamb+taille_table,compc2);
//	  free(temporaire);
	  
	  

	  
 
	  
	}
      else 
	{
//	  cout.flush() << " F_C : " << node << "\n";
//	  cpt++;
	  isF = Check_if_F_or_C(node,size_tab,tab,spec_tab, lens,&taille_table, &lamb, &Nodes_val);
 	
//	  if ( isF ) { cptf++;}

	  update_ptr(&C_nodes,&((*tab)[2])); 
	}     
     
      node = Next_node(size_tab,tab,spec_tab,&lamb,&taille_table,Nodes_val);
 

     
//   CHANGER      Nodes = (*tab)[0];
      
  //  cout.flush() << "Noeud choisi " << node << "\n";


    }
   

//   exit(1);   
   

    
  Free_parents(size_tab, tab, spec_tab,lens);
//   update_ptr(&Neighbours, &((*lens)[0]));
   

 // cout.flush() << cptf <<" F SUR " << cpt << "\n";
   
  //  cout.flush() << " Niveau terminer ";
    
    //  (*tab)[2] = C_nodes;
    
    /* INVERT THE C-NODES ORDER */
    
    /*
    int endroit = int(*size_C/2);
    int tp;
  
    for ( i=0; i< endroit; i++)
      {
 	tp = C_nodes[*size_C-1 -i];
 	(*tab)[2][*size_C-1 -i] = C_nodes[i];
 	(*tab)[2][i] = tp; 
      }
*/
   

 
  //  free( val );
    free(Nodes_val);
    kill( Neighbours );
    kill( liste );
    kill(C_nodes);
 // CHANGER    kill(Nodes);
    kill(size_C);
    kill(size_Nodes);   
   

}


int unique (int *t, int taille)
{
  int i,tmp,accu;
  if ( taille == 0 ) return 0;
  tmp=t[0];
  accu = 1;
  for ( i=1; i< taille; i++)
    {
      if ( t[i] != tmp )
	{
	  t[accu++] = t[i];
	   tmp = t[i];
	}
    }
  return accu;
}

int unique (noeud *t, int taille)
{
  int i,tmp,accu;
  

  if ( taille == 0 ) return 0;
  tmp=t[0].num_noeud;
  accu = 1;
  for ( i=1; i< taille; i++)
    {
      if ( t[i].num_noeud != tmp )
	{
	  t[accu++] = t[i];
	   tmp = t[i].num_noeud;
	}
    }
  return accu;
}



 int comp( const void * x, const void * y) 
{

  if   (   ((*((nlamb *)x)).val) < ( (*((nlamb *)y)).val) ) return -1;
  else if ( ( (*((nlamb *)x)).val) == ((*((nlamb *)y)).val ) ) return 0;
  else return 1;
}



int recomp( const void * x, const void * y) 
{
  if   (  (*((int *)x)) <  (*((int *)y)) ) return -1;
  else if (  (*((int *)x)) ==  (*((int *)y) ) ) return 0;
  else return 1;
}

	 
