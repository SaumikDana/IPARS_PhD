#define _LABEL_SPEC_

//#include "library.h"

#include "library.h"

/* Implementation file */

/* R. E. Bank, C. Wagner 2 dimensionnal Milu decomposition. Solving Ku=f*/

/* Includes the templated class element dealing with scalar or matricial elements whether it is a system or not
 */

/**********************************************************/


/* LABELLING */



/*Initialise the starting graph for the jacobian*/





/* Check for the optimal parents of node i */

void Mark_opt_par(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  int j;
  int size_Wi, size_Qi, size_Pi;
  int pos; 
  int pack, node, node1,pos1;
  int jj,nu_i, min_nu_i;
  spec_noeud *liste;
  bool isC = false;
  int ncni;
  pack = (*size_tab)[9];
  liste = (*spec_tab);


  extern  refr *Wi ;//= (refr*) malloc(sizeof(refr)*((liste[i]).nbre));
  ensemble *Opt = (*lens)[2];
  pack = (*size_tab)[9];
  liste = (*spec_tab);
 
  
//  return;
  /* Create the list to minimise new edges and reset marks */
  
  size_Wi = 0;
  size_Qi = 0;
  size_Pi = 0;
   
  for ( j=0; j < liste[i].nbre; j++)
    {
      node = ((liste[i]).liste_noeud[j]).num_noeud;
      if ( (((liste[i]).liste_noeud[j]).edge > 0 ) && ( liste[node].where < 1) )
	{
	  if ( size_Wi >= 200 ) {printf("OUIWQ "); exit(1);}
	//  check_size(&Wi,size_Wi, pack);
	  (Wi[size_Wi]).noeud = node;
	  (Wi[size_Wi]).ncn = ncn(i,node,lens);
	  (Wi[size_Wi++]).pos = j;
	  ((liste[i]).liste_noeud[j]).edge = 1;
	}
    }

  
    if ( size_Wi == 0 ) 
      {
  	//    free(Wi);
	return ;    
      }
    extern couple *Pi ;//= NULL; 
    extern couple *Qi ;//=(couple*) malloc(sizeof(couple)*size_Wi*size_Wi); 
    extern int smaxqi;
    extern int smaxpi;

  //  if ( size_Wi*(size_Wi ) > smaxqi) { printf(" QI NOT LARGE ENOUGH \n");exit(1);}

    if ( size_Wi == 1)
      {
	//Pi = (couple*) malloc(sizeof(couple));
	ncni =  (Wi[0]).ncn;
	node = (Wi[0]).noeud;
	nu_i =  nui(i,node,size_tab,tab,spec_tab,lens,ncni);
	pos = (Wi[0]).pos;
//	check_size(&Qi,size_Qi,pack);
	(Qi[size_Qi]).v1 = node;
	(Qi[size_Qi  ]).v2 = node;
	(Qi[size_Qi]).pos1 = pos;
	(Qi[size_Qi  ]).pos2 = pos;
	(Qi[size_Qi++ ]).nu = nu_i;
	//check_size(&Pi,size_Pi,pack);
	(Pi[size_Pi]).v1 = node;
	(Pi[size_Pi  ]).v2 = node;
	(Pi[size_Pi]).pos1 = pos;
	(Pi[size_Pi  ]).pos2 = pos;
	(Pi[size_Pi++ ]).nu = nu_i;
	if ( liste[node].where == 0) isC =true;
      }
    
    else
      {
	
	
	/* Minimise new edges */
	
	
	int ncn1;
	min_nu_i = 9999;
	for (j=0; j< size_Wi; j++)
	  {
	    node = (Wi[j]).noeud;
	    pos = (Wi[j]).pos;
	    ncn1 =  (Wi[j]).ncn;
	    for (jj=j+1; jj < size_Wi; jj++)
	      {
		ncni =  (Wi[jj]).ncn+ncn1;
		node1 = (Wi[jj]).noeud;
		pos1 = (Wi[jj]).pos;
		nu_i = nui(i,node,node1,size_tab,tab,spec_tab,lens,ncni);
		
		// debug
		
		//	cout << " nu_i " << nu_i << "\n";
		
		
		if ( nu_i < min_nu_i)
		  {
		    size_Qi = 0;
		    min_nu_i = nu_i;
		   
		    (Qi[size_Qi]).v1 = node;
		    (Qi[size_Qi]).v2 = node1;
		    
		    (Qi[size_Qi]).pos1 = pos;
		    (Qi[size_Qi]).pos2 = pos1;
		    
		    (Qi[size_Qi++]).nu = nu_i;
		  }
		else if ( nu_i == min_nu_i)
		  {
		     if ( size_Qi >= smaxqi) { printf("WHASA");exit(1);}
		    (Qi[size_Qi]).v1 = node;
		    (Qi[size_Qi]).v2 = node1;
		    (Qi[size_Qi]).pos1 = pos;
		    (Qi[size_Qi]).pos2 = pos1;
		    (Qi[size_Qi++]).nu = nu_i;
		  }
	
	      }
	  }

	// Weakest connections give optimal parents //
	
	int etai, min_etai,etai2; 
	min_etai = 9999; 
      
      
	//Pi =(couple*) malloc(sizeof(couple)*size_Qi*size_Qi); 	
	for (j=0; j< size_Qi; j++)
	  { 
	    node = (Qi[j]).v1;
	    node1 = (Qi[j]).v2;
	    if ( size_Qi == 1 )
	    {
	      etai = min_etai;
	    }
	  else
	    {
	      etai = eta(node , node1, size_tab,spec_tab,lens);
	    }

	  if ( etai < min_etai)
	    
	    {
	      min_etai = etai;
	      size_Pi = 0;
	      (Pi[size_Pi]).v1 = node;
	      (Pi[size_Pi]).v2 = node1;
	      (Pi[size_Pi]).nu = (Qi[j]).nu;
	      (Pi[size_Pi]).pos1 = (Qi[j]).pos1;
	      (Pi[size_Pi++]).pos2 = (Qi[j]).pos2;
	      if ( ( liste[node].where == 0 ) || ( liste[node1].where == 0)) 
		{
		  isC =true;
		}
	      else isC = false;
	    }
	  else if ( etai == min_etai )
	    {
	       if ( size_Pi >= smaxpi) { printf("TARTINE");exit(1);}
	      (Pi[size_Pi]).v1 = node;
	      (Pi[size_Pi]).v2 = node1;
	      (Pi[size_Pi]).nu = (Qi[j]).nu;
	      (Pi[size_Pi]).pos1 = (Qi[j]).pos1;
	      (Pi[size_Pi++]).pos2 = (Qi[j]).pos2;
	      if ( ( liste[node].where == 0 ) || ( liste[node1].where == 0)) 
		{
		  isC =true;
		}
	    }
	  
	}
      }
  //  cout << "SIZE_PI " << size_Pi;
    

    int nbi;

    if ( Opt[i].nbre != 0 )
      {
	free(Opt[i].liste);
	Opt[i].nbre = 0;
      }
    Opt[i].liste = (int*) malloc(sizeof(int)*size_Pi*2);
    if ( isC )
      {
	for (j=0; j< size_Pi; j++)
	  { 
	    node = (Pi[j]).v1;
	    node1 = (Pi[j]).v2;
	    nu_i =  (Pi[j]).nu;
	    pos = (Pi[j]).pos1;
	    pos1 = (Pi[j]).pos2;
	    nbi = nb(i,lens);

	    if ( ((liste[node]).where == 0 ) || ( (liste[node1]).where == 0)) 
	      {
		((liste[i]).liste_noeud[pos]).edge = ((liste[i]).liste_noeud[pos1]).edge  = 250 + 2 *nbi - nu_i;
		(Opt[i]).liste[Opt[i].nbre++] = node;
		if ( node != node1)
		  {
		    (Opt[i]).liste[Opt[i].nbre++]= node1;
		  }
	//	cout << "\n for node " << i << " nodes are " << (liste[i]).liste_noeud[pos].num_noeud << " , " << (liste[i]).liste_noeud[pos1].num_noeud << " and value is " ;
		//cout << 250 + 2 *nbi - nu_i<< "\n";
	      }
	  }
      }
    else
      {
	for (j=0; j< size_Pi; j++)
	  { 
	    node = (Pi[j]).v1;
	    node1 = (Pi[j]).v2;
	    nu_i =  (Pi[j]).nu;
	    pos = (Pi[j]).pos1;
	    pos1 = (Pi[j]).pos2;
	    nbi = nb(i,lens);
	    ((liste[i]).liste_noeud[pos]).edge = ((liste[i]).liste_noeud[pos1]).edge = 100 + 2* nbi- nu_i;
	    (Opt[i]).liste[Opt[i].nbre++] = node;
	    if ( node != node1)
	      {
		(Opt[i]).liste[Opt[i].nbre++] = node1;
	      }
//	    cout << "\n for node " << i << " nodes are " << (liste[i]).liste_noeud[pos].num_noeud << " , " << (liste[i]).liste_noeud[pos1].num_noeud << " and value is " ;
	//    cout  << 100 + 2* nbi- nu_i << "\n";
	    
	  }
      }
 
    (*spec_tab) = liste;
    (*lens)[2] = Opt;
   
    kill(liste);
    kill(Opt);
 /*   free(Qi);
    free(Pi);
    free(Wi);
*/
    return;
    
    
}


/*********************************************************/

/* Check for the next node to be eliminated */

int Next_node(int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), nlamb **lamb, int *taille_tab,int* Nodes_val)
{
  
  int choosen_node;
  
  
  int *size_Nodes= *size_tab + 8;
  spec_noeud *liste = (*spec_tab);

  
bool found = false;
  int value;
  //  cout << "Next node : taille : " << *size_Nodes << "\n";
  
// cout.flush() << "ON PASSE LA " << "SIZE " << *size_Nodes<< "\n" ;

  
  if (*size_Nodes == 0) 
    {
      free((*lamb)[0].liste);
      free(*lamb);
      //      cout << "je renvoie -1 \n";
      return  -1;
      
    }
  
  while ( !found )
    {
  //    cout << " TAILLE TABLE "<<  (*lamb)[*taille_tab-1].nbre<< "\n";
      value = (*lamb)[*taille_tab-1].val;//cout << " TAILLE TABLE "<< *taille_tab-1 << "\n";
      choosen_node = (*lamb)[*taille_tab-1].liste[(*lamb)[*taille_tab-1].nbre-1];
    //  cout.flush() << " choosen node " << choosen_node;
      // cout << " DONE ";
      if (  (*lamb)[*taille_tab-1].nbre != 1 )
	{
	   
	  (*lamb)[*taille_tab-1].liste = (int*) realloc((*lamb)[*taille_tab-1].liste,sizeof(int)*((*lamb)[*taille_tab-1].nbre-1));
	  (*lamb)[*taille_tab-1].nbre--;
	}
      else if ( ((*lamb)[*taille_tab-1].nbre == 1 ) && (*taille_tab != 1 ))
	{
	  //	  choosen_node = (*lamb)[*taille_tab-1].liste[0];
	  free(  (*lamb)[*taille_tab-1].liste);
	  (*taille_tab) = (*taille_tab)-1;
	 // (*lamb) = (nlamb*) realloc((*lamb),sizeof(nlamb)*(--(*taille_tab)));
	}
      //     else  break;
      else if ((*lamb)[*taille_tab-1].nbre < 1 )
	{ printf("BEURK ") ; exit(1);} 
      
      if ( (value == Nodes_val[choosen_node]) && ( liste[choosen_node].where == -1 ) ) found = true;
      
    }
      
  (*size_Nodes)--; 
 
  kill(liste);
  return choosen_node;
}

/*******************************************************/

/* Look if the considerated node belongs to the F-nodes set*/

bool Check_if_F_node(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  int np,ne,j, nombre, vois, l;
  np=0;
  ne=0;
  spec_noeud noeuds;
  
  //  int size = size_tab[0];
  int *size_F = *size_tab + 1;
  int *F_nodes = (*tab)[1];
  int *Nodes = (*tab)[0];
  int *size_Nodes = *size_tab + 8;
  ensemble *Parents = (*lens)[1];
  spec_noeud *liste = (*spec_tab);
  int pack = (*size_tab)[9];
  int *temp = (int*) malloc(sizeof(int)*liste[i].nbre);
  bool res = false;
  
  noeuds = liste[i];
  nombre = noeuds.nbre;
  
  for (j=0; j<nombre; j++)
    {
      if ( (noeuds.liste_noeud[j]).edge > 0 )
	{
	  
	  ne++;
	  vois = (noeuds.liste_noeud[j]).num_noeud;
	  if ((liste[vois]).where == 0)
	    {

	      //	      check_size(&((Parents[i]).liste_noeud),(Parents[i]).nbre,pack);
	      //	      (Parents[i]).liste_noeud[np].num_noeud=vois;

	      temp[np++] = vois;

	      //	      cout << " ON MET UN PARENT \n"; 
	      
	      //	      Parents[i].nbre++;
	    }
	}
    }
  
  if ( (np > 1) || ( (ne ==1) && (np ==1)))
    {
      //      cout << " IF "

      check_size(&F_nodes, *size_F, pack);
      F_nodes[(*size_F)++]= i;
      
//      cout << " le noeud suivant devient un noeud F: "<< i << "\n";
//      cout.flush();
      //      int *temp = (int*) malloc(sizeof(int)*(*size_Nodes));

      (liste[i]).where = 1;

      /*      for (j=0; j< *size_Nodes; j++)
	      {
	      temp[j] = Nodes[j];
	      if (Nodes[j] == i) 
	      {
	      (*size_Nodes)--;
	      for ( l=j; l< *size_Nodes; l++)
	      {
	      temp[l] = Nodes[l+1];
	      }
	      break;
	      //Nodes[j] = Nodes[--(*size_Nodes)];
	      }
	      }
	      
	      //      if ( *size_Nodes > 0 )
	      //	{
	      (*tab)[0] = (int*) realloc((*tab)[0],sizeof(int)*(*size_Nodes));
	      //	}
	      for ( l=0; l<*size_Nodes; l++ )
	      {
	      (*tab)[0][l] = temp[l];
	      }*/
     



   //   cout << "PARENTS ";

      (((*lens)[1])[i]).liste = (int*) malloc(sizeof(int)*np);

      for (l=0; l< np;l++)
	{
	  (((*lens)[1])[i]).liste[l] = temp[l];
	  //cout.flush() << temp[l] << ", ";
	}
      //cout << "\n";
      (((*lens)[1])[i]).nbre= np;
      
	/*
      (Parents[i]).liste_noeud = (noeud*) malloc(sizeof(noeud)*((liste[i]).nbre));
      for (l=0; l< liste[i].nbre;l++)
	{
	  ((Parents[i]).liste_noeud[l]).num_noeud = (liste[i]).liste_noeud[l].num_noeud;
	}
      (Parents[i]).nbre= liste[i].nbre;
	*/
	



    
      (*tab)[1]= F_nodes;
      
 //   update_neigh(i,size_tab,tab,spec_tab,lens);

      res = true;
      //      cout << "\n Le noeud " << i << " est un noeud F \n";
    }
  else
    {
      
      res = false;

    }
  
/*  cout << "\n Les parents du noeud " << i << " sont : ";
  
  for (j=0; j < np; j++)
    {
      cout << ((Parents[i]).liste_noeud[j]).num_noeud << ", ";
    }
  cout << " nbre de parents \n" << (Parents[i]).nbre;
  
 */ 
  //  (*spec_tab)[3] = liste;
  free(temp);
  (*tab)[1]= F_nodes;
  kill(Parents);
  kill(F_nodes);
  kill(liste);
  kill(size_F);
  kill(Nodes);
  kill(size_Nodes);
//  (*spec_tab)[1] = Parents;

  return res;

}
     
 bool compc3(const nlamb x, const nlamb y)
{
    if (x.val < y.val) return true;
    else return false;
}


/********************************************************/

/* Look up whether the node is a F- or a C- node */

bool  Check_if_F_or_C(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]), int *taille_table, nlamb **lamb, int **Nodes_val)
{
  int np,j,k;
  //pos,
  int s,node,edge,par, node3;
  int compteur =0;
//  int size = (*size_tab)[0];
//  int size_temp =0;
  //  int *temp = new int[size];
  int nedges;
  bool new_F = false;
  int pack = (*size_tab)[9];
  int *size_F = (*size_tab+1) ;
  int *size_C = (*size_tab+2) ;
  int *F_nodes = (*tab)[1];
  int *C_nodes = (*tab)[2];
  int *val =  (int*) malloc(sizeof( int)*2);
  int *val2 = (int*) malloc(sizeof( int)* 2) ;
  ensemble *Opt = (*lens)[2];
  spec_noeud *liste = (*spec_tab);
  ensemble *Parents = (*lens)[1];
  ensemble *Neighbours = (*lens)[0];
extern bool compc2(nlamb x, nlamb y);

  nedges = max_new_edges(i,spec_tab,lens);
 
 //out << " EDGES " << nedges << "\n";


 if ( ( nedges == 0 ) || ( Neighbours[i].nbre  == 0    ))
    {

   // cout << "NOEUD F \n";

      check_size(&F_nodes,*size_F,pack);
      F_nodes[(*size_F)++]=i;
      new_F = true ;
      Parents[i].nbre = Neighbours[i].nbre;
      Parents[i].liste = (int*) malloc(sizeof(int)*(Neighbours[i]).nbre);
      for (j=0; j< Neighbours[i].nbre; j++)
	{
	      (Parents[i]).liste[j] = (Neighbours[i]).liste[j];
	  //if ( liste[(Parents[i]).liste[j]].where != 0 ) cout << " ATTENTION VOIS " << "\n"; 
	  // Parents[i].liste_noeud[j].edge = (Neighbours[i]).liste_noeud[j].edge;
	}

      
      //      cout << "Dans F ou C le noeud suivant devient F ( vois): "<< i << "\n";
      //      cout.flush();
      liste[i].where = 1;

//      Parents[i].liste_noeud = (Neighbours[i]).liste_noeud;
//      Parents[i].nbre = (Neighbours[i]).nbre;

      (*lens)[1] = Parents;
      (*tab)[1] = F_nodes;
      //  return ;
    

//  cpt++;

    }

  else

    {
      //cout << "ELSE\n";

      /* Correct values */

 //     Parents[i].nbre = tmp_nbre;
  //    Parents[i].liste_noeud = lnoeud;
      
      
      np=0;
      for ( j=0; j < liste[i].nbre; j++)
	{
	  node = ((liste[i]).liste_noeud[j]).num_noeud;
//	  check_edge(i,node,spec_tab,&val);
//	  edge = (val)[0];
	

	  /* WARNING CHANGED THERE */
  
	  //	  if ( edge >= 0 )
	  if ( liste[node].where == 0)
	    {
	      edge = ((liste[i]).liste_noeud[j]).edge;
	      // pos = (val)[1];
	      //	      temp[size_temp++] = node;
	      if ( edge > 0)  np++;
	      
	      compteur = 0;
	      //	      for (s=0; s< size; s++)
	      
	      for ( s=0; s < liste[i].nbre; s++)
		
		{
		  if ( ((liste[i]).liste_noeud[s]).edge > 0)
		    {
		      par = check_parents(s, node, lens);
		      if (par != -1 )
			{
			  //    check_edge(i,s,spec_tab,&val2);
			  // edge = (val2)[0];
			  //if (edge > 0 ) compteur++;
			  compteur ++;
			}
		    }
		}
	      if ( compteur >= 2 )
		{
		  np++;
		  ((liste[i]).liste_noeud[j]).edge = 1;
		  //   break;
		}
	    }
	}
      
      if ( np > 1) 
	{
	  
	  // updates values

 
	  (*spec_tab) = liste;
	  (*lens)[1] = Parents;
	  (*lens)[0] = Neighbours;

	  
	  Mark_opt_par(i,size_tab,tab,spec_tab,lens);
	  check_size(&F_nodes,*size_F,pack);
	  F_nodes[(*size_F)++]= i;

	//  cout.flush() << " NOEUD F CHOISI " << i << "\n";
	  //	  update_neigh(i,size_tab,tab,spec_tab);
//	  cpt ++;

	  new_F = true;

	  //	  cout << "Dans F ou C, le noeud suivant devient F (np>1):" << i << "\n";
	  //	  cout.flush();

	  liste[i].where = 1;

	  //	  for ( j=0; j<size_temp; j++)
	  for ( j=0; j< liste[i].nbre; j++)
	    {

	      //node = temp[j];

	      node = ((liste[i]).liste_noeud[j]).num_noeud;
	    
	      if ( (liste[node].where == 0) &&  (((liste[i]).liste_noeud[j]).edge > 0))
		   {
		     for (k=0; k< Opt[i].nbre; k++)
		       {
			 if ( (Opt[i]).liste[k] == node) 
			   {
			     check_size(&((Parents[i]).liste),(Parents[i]).nbre,pack);
			     ((Parents[i]).liste[(Parents[i]).nbre++])=node;
			     (((*lens)[1])[i]).liste = (Parents[i]).liste;
			     
			     break;
			   }
		       }
		   }

	    }

	}
      else 
	{
	  
	  check_size(&C_nodes,*(size_C),pack);
	  C_nodes[(*size_C)++]=i;
	  (*tab)[2] = C_nodes;
	  //	  cout << "Dans F ou C, le noeud suivant devient C: " << i << "\n";
	  //	  cout.flush();
	  liste[i].where = 0;
	
	}
    }
  
  (*spec_tab) = liste;
  (*tab)[1] = F_nodes;
  (*tab)[2] = C_nodes;
  (*lens)[1] = Parents;
  (*lens)[0] = Neighbours;
  (*lens)[2] = Opt;
  if ( new_F)  update_neigh(i,size_tab,tab,spec_tab,lens);
 //cout <<  " REAL F : " << cpt << "\n";

  int tempo,jj,pos;
  bool nouw,reord;
  
  
  reord = false;
  
  for ( j=0; j< liste[i].nbre; j++)
    {
      node3 = liste[i].liste_noeud[j].num_noeud;
      
      if (liste[node3].where == -1 )
	{
	  tempo = lambda_i(node3,size_tab,tab,spec_tab);
	  //	  cout << " LAMBDA REEVALUE " << node3 << " VAL " << tempo << "\n";
	  if ((*Nodes_val)[node3] != tempo )
	    {
	      (*Nodes_val)[node3] = tempo;
	      nouw = true;
	      for ( jj=0; jj< (*taille_table); jj++)
		{
		  if ( (*lamb)[jj].val == tempo )
		    {
		      nouw = false;
		      pos = jj;
		      break;
		    }
		}
	      if ( !nouw )
		{
		  (*lamb)[pos].liste = (int*) realloc((*lamb)[pos].liste,sizeof(int)*((*lamb)[pos].nbre+1));
		  (*lamb)[pos].liste[(*lamb)[pos].nbre++] = node3;
		}
	      else
		{
		  reord= true;
		 // (*lamb) = (nlamb*) realloc((*lamb),sizeof(nlamb)*((*taille_table)+1));
		  if ( *taille_table+1 > 999 ) { printf("ERREUR LAMBDA ALLOC");exit(1);}
		  (*lamb)[(*taille_table)].val = tempo;
		  (*lamb)[(*taille_table)].nbre = 0;
		  (*lamb)[(*taille_table)].liste = (int*) malloc(sizeof(int));
		  (*lamb)[(*taille_table)].liste[(*lamb)[(*taille_table)].nbre++] = node3;
		  (*taille_table)++;
		}
	    }
	}
    }
  
      
  
  
  if ( reord) sort((*lamb),(*lamb)+(*taille_table),compc3);
  
  
  //      kill2(temp);
      kill(size_F);
      kill(size_C);
      kill(F_nodes);
      kill(C_nodes);
      free(val);
      free(val2);
      kill(Opt);
      kill(liste);
      kill(Parents);
      kill(Neighbours);
      return new_F;
}

  
/*******************************************************/

/* Free parents for the next stage */

void Free_parents(int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]))
{
  int k, node, node1,nedges, neigh, kk;
  int pack = (*size_tab)[9];
  int size_F = (*size_tab)[1];
  int *F_nodes = (*tab)[1];
  ensemble *Neighbours = (*lens)[0];
  ensemble *Parents = (*lens)[1];
  spec_noeud *liste = (*spec_tab);
//  spec_noeud *liste = (*lens);
  int newP = 0;

  int kkk,node2;
  
  for ( k=0; k< size_F; k++)
    {
      node = F_nodes[k];
   
/*

      cout << "NODE " << node << ":  "; 
      for ( kk=0; kk < Neighbours[node].nbre; kk++)
	{
	  cout << Neighbours[node].liste_noeud[kk].num_noeud << " , ";
	    }
      cout << "\n\n";

*/

      newP=0;
      for (kk=0; kk< (Neighbours[node]).nbre; kk++)


      //      for (kk=0; kk< (Neighbours[node]).nbre; kk++)
	//	for (kk=0;kk< liste[node].nbre; kk++)


	{

	 /*WARNING BE AWARE OF THE MDIFICATIONS: TO SUPPRESS */

	  node1 = (Neighbours[node]).liste[kk];

//	  node1 = liste[node].liste_noeud[kk].num_noeud;
	  
	  neigh = check_parents(node,node1,lens);
	  
//	  neigh =1;

	  if ( ( neigh == -1) && (liste[node1].where < 1 ) )
	    {
	      nedges = Neighbours[node].nbre -1 -ncn(node,node1,lens);
	      for ( kkk=0; kkk < Parents[node].nbre-newP; kkk++)
		{
		  node2 = Parents[node].liste[kkk];
		  nedges = nedges -2*chi(node1,node2,lens);
		  //nedges = nedges + chi(node1,node2,lens);
		}
	    //  nedges =0;
	      if (( nedges ==0 ) ) 
		{
		  check_size(&((Parents[node]).liste),(Parents[node]).nbre,pack);
//		  ((Parents[node]).liste_noeud[(Parents[node]).nbre++]).num_noeud = ((Neighbours[node]).liste_noeud[kk]).num_noeud;
		  ((Parents[node]).liste[(Parents[node]).nbre++]) = node1;
		  (*lens)[1] = Parents;
		  newP++;
		}
	    }
	}
    }
  
  (*lens)[1] = Parents;
  (*lens)[0] = Neighbours;

  kill(F_nodes);
  kill(Neighbours);
  kill(Parents);

}

/********************************************************/


  
