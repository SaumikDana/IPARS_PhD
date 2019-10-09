#define _STRUCT_

typedef double typ;


typedef struct
{
  int num_noeud;
  int post;
  int edge;
  // where stands for the type of nodes: -1 the standart list
  //  0 the C list
  // 1 the F list
}noeud;


struct nlamb
{
  int val;
  int nbre;
  int *liste ;
};


typedef struct
{
  int nbre;
  int where;
//  int restant;
//  int *liste_restant;
  noeud *liste_noeud;
}spec_noeud;

typedef struct
{
  int nbre;
  int *liste;
}ensemble;


typedef struct 
{
  int v1;
  int v2;
  int nu;
  int eta;
  int pos1;
  int pos2;
}couple;

struct triplet
{
  int v1;
  int v2;
  int v3;
  int nu;
  int pos1;
  int pos2;
  int pos3;
};

typedef struct 
{
  int noeud;
  int pos;
  int ncn;
}refr;

