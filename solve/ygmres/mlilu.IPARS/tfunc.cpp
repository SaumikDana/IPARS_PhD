#ifdef _DEBUG_
typedef typ = double;
//template<class typ>


void MLILU(typ **Ui, typ **fi, int **Res_Nodes[4], int *Res_size[3], int ***Res_Parents,  int level, typ **Ki, typ **LU, int dim,int init_lev, int gamma, double delta)
{

  typ *di = new typ[dim];
  typ *di_1 = NULL;
  typ *vi_1 = NULL;
  int size_begin, size_end; 
  int old_dim = dim;
  
   size_begin = ((*Res_Nodes)[2])[level];
   size_end = ((*Res_Nodes)[2])[level+1]; 
   dim = size_end - size_begin ;
  if ( level == init_lev ) // C_nodes
    {
      cout << "BEGIN " << size_begin << "\n";
      cout << "END " << size_end << "\n";
      direct_solve(Ui,fi, Ki, size_begin, size_end, dim);
    }
  else
    {
      // Construct Ki+1

      dim = size_end - size_begin;
      smooth(Ui,fi,Ki,dim, size_begin, size_end);
      matvec_add(Ki,Ui,fi,&di,dim, size_begin, size_end);
      (di_1) = new typ[old_dim-dim+1];
      di_1 = di + dim;

      if ( level != 0 )
	{
	  invert_L(LU, &di, size_begin, size_end, dim);
	}
      construct_K_L_U(size_begin,size_end,Ki,LU,Res_Nodes,Res_Parents,Res_size,dim, delta);
      vi_1 = (typ *) calloc(old_dim-dim+1, sizeof(typ));
      for (int z=0; z< gamma; z++) 
	{
	  MLILU(&vi_1, &di_1,  Res_Nodes, Res_size, Res_Parents,level+1, Ki, LU, dim,init_lev,gamma,delta);
	}

      for ( int ii=dim; ii< old_dim ; ii++)
	{
	  di[ii] = vi_1[ii-old_dim];
	}
      invert_U(LU, Ui, &di,size_begin, size_end, dim);
      smooth(Ui,fi,Ki,dim, size_begin, size_end);
    }
  
}














void direct_solve( typ **Ui, typ **fi, typ **Ki, int size_begin, int size_end, int dim)
{
  // Direct solve is so far a simple Gauss-Seidel solver
  

  int i0;
 
  for ( int i = size_begin; i< size_end; i++)
    {
      i0 = i - size_begin;
      (*Ui)[i0] = (*fi)[i0] / (*Ki)[i+dim*i];
    }
  
}
#endif
