#ifndef SMALL
#define SMALL 1.0e-16
#endif
#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))
#define abs(a) ((a) < 0 ? -(a) : (a))

template<class T>
class list {
private:
  int size;
  int res;
  int memstep;
  T *memb;
public:
  list() { res=size=0; }
  list(int order, int reserv, int step) { init(order,reserv,step); }
  list(int nsize, T *a) { init(nsize,a); }
  virtual ~list() { del(); }
  void init(int order, int reserv, int step);
  void init(int nsize, T *a) { size=nsize; res=0; memb=a; }
  void del(void) { if (res) delete [] memb; res=0; }
  void clear(void);
  int getsize(void) { return size; }
  T* getptr(void) { return memb; }
  list<T>& operator=(list<T>& l);
  void add(T data);
  void add(list<T>& l2) { add(l2.size,l2.memb,size+l2.size); }
  void add(int num, T* table, int maxadd);
  void del(int i);
  T& operator()(int i);
  int locate(T data);
  int which(T data);
  int operator==(list<T>& l);
};

template<class T>
void list<T>::init(int order, int reserv, int step)
{
  #ifdef DEBUG
  if (reserv < 1) {
    cerr << "Must reserve memory for list!" << endl;
    return;
  }
  if (step < 1) {
    cerr << "Memory step must be positive in (list) init(i,i,i)" << endl;
    return;
  }
  #endif
  size=0; res=reserv; memstep=step;
  memb=new T[res];
  #ifdef DEBUG
  if (!memb) cerr << "Memory allocation error for list!" << endl;
  #endif
}

template<class T>
void list<T>::clear(void)
{
  size=0;
}

template<class T>
list<T>& list<T>::operator=(list<T>& l)
{
  int i;
  if (res) del();
  size=l.size; res=l.size; memstep=l.memstep;
  memb=new T[res];
  #ifdef DEBUG
  if (!memb) cerr << "Memory allocation error for (list) =" << endl;
  #endif
  for (i=0; i < size; i ++) memb[i]=l.memb[i];
  return *this;
}

template<class T>
void list<T>::add(T data)
{
  int i, l, u, v;
  T *aux;
  if (size == 0) {
    memb[0]=data;
    size ++;
    return;
  }
  if (size >= res) {
    res += memstep;
    aux=new T[res];
    #ifdef DEBUG
    if (!aux) {
      cerr << "Memory allocation error in (list) add!";
      return;
    }
    #endif
    for (i=0; i < size; i ++) {
      aux[i]=memb[i];
    }
    delete [] memb;
    memb=aux;
  }

  l=0; u=size-1;
  if (memb[l] > data) {
    for (i=size-1; i >= 0; i --) {
      memb[i+1]=memb[i];
    }
    memb[0]=data;
    size ++;
    return;
  }
  if (memb[u] < data) {
    memb[size]=data;
    size ++;
    return;
  }
  if (memb[l] == data || memb[u] == data) return;
  while (u-l > 1) {
    v=(u+l) >> 1;
    if (memb[v] > data) {
      u=v;
    }
    else {
      l=v;
      if (memb[l] == data) return;
    }
  }
  for (i=size-1; i > l; i --) {
    memb[i+1]=memb[i];
  }
  memb[l+1]=data;
  size ++;
}

template<class T>
void list<T>::add(int num, T *table, int maxadd)
{
  int i, j, k, tot;
  T* target;
  i=j=k=0;
  while ((i < size || j < num) && k < maxadd) {
    if (j >= num) { i ++; continue; }
    if (i >= size) { j ++; k ++; continue; }
    if (memb[i] < table[j]) { i ++; continue; }
    if (table[j] < memb[i]) { j ++; k ++; continue; }
    i ++; j ++;
  }
  tot=size+k;
  if (k == 0) return;
  if (tot > res) {
    res=max(tot,res+memstep);
    target=new T[res];
  }
  else target=memb;
  k=tot-1; i=size-1; j --;
  while (i >= 0 || j >= 0) {
    if (j < 0) {
      target[k]=memb[i]; k --; i --; continue;
    }
    if (i < 0) {
      target[k]=table[j]; k --; j --; continue;
    }
    if (memb[i] > table[j]) {
      target[k]=memb[i]; k --; i --; continue;
    }
    if (table[j] > memb[i]) {
      target[k]=table[j]; k --; j --; continue;
    }
    target[k]=memb[i]; k --; i --; j --;
  }
  size=tot;
  if (target != memb) { delete [] memb; memb=target; }
}

template<class T>
void list<T>::del(int i)
{
  int j;
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (list) del(i)!" << endl;
    return;
  }
  #endif
  if (i == size) {
    size --;
    return;
  }
  for (j=i; j < size-1; j ++) {
    memb[j]=memb[j+1];
  }
  size --;
}

template<class T>
T& list<T>::operator()(int i)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (list) ()(i)!" << endl;
    return memb[0];
  }
  #endif
  return memb[i];
}

template<class T>
int list<T>::locate(T data)
{
  int i, l, u, v;
  if (size == 0) {
    return 0;
  }
  l=0; u=size-1;
  if (memb[l] > data || memb[l] == data) return 0;
  if (memb[u] < data) return size;
  if (memb[u] == data) return u;
  while (u-l > 1) {
    v=(u+l) >> 1;
    if (memb[v] > data) {
      u=v;
    }
    else {
      l=v;
      if (memb[l] == data) return l;
    }
  }
  return u;
}

template<class T>
int list<T>::which(T data)
{
  int i;
  i=locate(data);
  if (i >= size) return -1;
  if (memb[i] == data) return i;
  return -1;
}

template<class T>
int list<T>::operator==(list<T>& l)
{
  int i, j, a;
  if (size != l.size) return 0;

  for (i=0; i < size; i ++) {
    if (memb[i] != l.memb[i]) return 0;
  }
  return 1;
}

template<class T>
class vector {
private:
  int size;
  int mem;
  T *values;
public:
  vector() { size=mem=0; }
  vector(int nsize) { init(nsize); }
  vector(int nsize, T *a) { init(nsize,a); }
  ~vector() { del(); }
  void init(int nsize);
  void init(int nsize, T *a);
  int init(void) { return mem; }
  void del(void) { if (mem) delete [] values; size=mem=0; }

  inline T& operator()(int i);
  void resize(int nsize);
  inline void clear(void);
  inline int getsize(void);
  inline T *getptr(void) { return values; }
  inline T *getptr(int i) { return values+i; }
  inline void put(int i, T data);
  inline void add(int i, T data);
  inline void sub(int i, T data);
  inline void mul(int i, T data);
  inline void div(int i, T data);
  inline void operator=(T data);
  inline void operator=(vector<T>& a);
  inline T dot(vector& m);
};

template <class T>
void vector<T>::init(int nsize)
{
  mem=size=nsize; values=new T[size];
  #ifdef DEBUG
  if (!values && size > 0) cerr << "Memory allocation error for vector!" << endl;
  #endif
}

template <class T>
void vector<T>::init(int nsize, T *a)
{
  size=nsize; mem=0; values=a;
}

template <class T>
inline T& vector<T>::operator()(int i)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in vector () !" << endl;
    return values[0];
  }
  #endif
  return values[i];
}

template <class T>
void vector<T>::resize(int nsize)
{
  if (mem >= nsize) {
    size=nsize;
    return;
  }
  del();
  values=new T[nsize];
  mem=size=nsize;
  #ifdef DEBUG
  if (!values && size > 0) cerr << "Memory allocation error for vector!" << endl;
  #endif
}

template <class T>
inline void vector<T>::clear(void)
{
  int i;
  i=0;
  if (size & 1) {
    values[i]=T(0.0); i ++;
  }
  if (size & 2) {
    values[i]=T(0.0); i ++;
    values[i]=T(0.0); i ++;
  }
  while (i < size) {
    values[i]=T(0.0); i ++;
    values[i]=T(0.0); i ++;
    values[i]=T(0.0); i ++;
    values[i]=T(0.0); i ++;
  }
}

template <class T>
inline int vector<T>::getsize(void)
{
  return size;
}

template <class T>
inline void vector<T>::put(int i, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (vector) put" << endl;
    return;
  }
  #endif
  values[i]=data;
}

template <class T>
inline void vector<T>::add(int i, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (vector) add" << endl;
    return;
  }
  #endif
  values[i] += data;
}

template <class T>
inline void vector<T>::sub(int i, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (vector) sub" << endl;
    return;
  }
  #endif
  values[i] -= data;
}

template <class T>
inline void vector<T>::mul(int i, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (vector) mul" << endl;
    return;
  }
  #endif
  values[i] *= data;
}

template <class T>
inline void vector<T>::div(int i, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (vector) div" << endl;
    return;
  }
  #endif
  values[i] /= data;
}

template <class T>
inline void vector<T>::operator=(vector<T>& a)
{
  int i;
  #ifdef DEBUG
  if (size != a.size) {
    cerr << "Bad sizes in (vector)= !" << endl;
    return;
  }
  #endif
  i=0;
  if (size & 1) {
    values[i]=a.values[i]; i ++;
  }
  if (size & 2) {
    values[i]=a.values[i]; i ++;
    values[i]=a.values[i]; i ++;
  }
  while (i < size) {
    values[i]=a.values[i]; i ++;
    values[i]=a.values[i]; i ++;
    values[i]=a.values[i]; i ++;
    values[i]=a.values[i]; i ++;
  }
}

template <class T>
inline void vector<T>::operator=(T data)
{
  int i;
  i=0;
  if (size & 1) {
    values[i]=data; i ++;
  }
  if (size & 2) {
    values[i]=data; i ++;
    values[i]=data; i ++;
  }
  while (i < size) {
    values[i]=data; i ++;
    values[i]=data; i ++;
    values[i]=data; i ++;
    values[i]=data; i ++;
  }
}

template <class T>
inline T vector<T>::dot(vector<T>& m)
{
  int i;
  T sum;
  #ifdef DEBUG
  if (size != m.size) {
    cerr << "Wrong sizes in dot" << endl;
    return 0;
  }
  #endif
  sum=T(0.0);
  i=0;
  if (size & 1) {
    sum += values[i]*m.values[i]; i ++;
  }
  if (size & 2) {
    sum += values[i]*m.values[i]; i ++;
    sum += values[i]*m.values[i]; i ++;
  }
  while (i < size) {
    sum += values[i]*m.values[i]; i ++;
    sum += values[i]*m.values[i]; i ++;
    sum += values[i]*m.values[i]; i ++;
    sum += values[i]*m.values[i]; i ++;
  }
  return sum;
}

template<class T>
class sparse_vector {
 private:
  int size;
  int nz;
  int mem;
  int memstep;
  int *indexes;
  T *values;
  T small;
  int locate(int i);
  void insert(int i, int k, T data);
 public:
  sparse_vector() { nz=mem=0; small=T(SMALL); }
  sparse_vector(int ns, int nm, int nms) { init(ns,nm,nms); }
  ~sparse_vector() { if (mem) { delete [] indexes; delete [] values; } }
  void init(int ns, int nm, int nms);
  void setsmall(T ns) { small=ns; }
  T getsmall(void) { return small; }
  void clear(void) { nz=0; }
  int getsize(void) { return size; }
  int getnz(void) { return nz; }
  int& getindex(int i) { return indexes[i]; }
  T& getvalue(int i) { return values[i]; }
  void del(int i);
  void put(int i, T data);
};

template<class T>
void sparse_vector<T>::init(int ns, int nm, int nms)
{
  #ifdef DEBUG
  if (ns < 0) {
    cerr << "Negative size in (sparse_vector) init(i,i,i)" << endl;
    return;
  }
  if (nm < 1) {
    cerr << "Nonpositive memory in (sparse_vector) init(i,i,i)" << endl;
    return;
  }
  if (nms < 1) {
    cerr << "Nonpositive memory step in (sparse_vector) init(i,i,i)" << endl;
    return;
  }
  #endif
  small=T(SMALL);
  nz=0;
  size=ns; mem=nm; memstep=nms;
  indexes=new int[mem];
  values=new T[mem];
}

template <class T>
int sparse_vector<T>::locate(int i)
{
  int u, l, v;
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (sparse_vector) locate(i)" << endl;
    return -1;
  }
  #endif
  if (nz == 0) return 0;
  if (indexes[0] > i) return 0;
  if (indexes[nz-1] < i) return nz;
  l=0; u=nz-1;
  if (indexes[l] == i) return l;
  if (indexes[u] == i) return u;
  while (u-l > 1) {
    v=(u+l) >> 1;
    if (indexes[v] > i) {
      u=v;
    }
    else {
      l=v;
      if (indexes[l] == i) return l;
    }
  }
  if (indexes[l] == i) return l;
  return u;
}

template <class T>
void sparse_vector<T>::insert(int i, int k, T data)
{
  int l;
  T *auxv;
  int *auxi;
  if (nz == mem) {
    auxv=new T[mem+memstep];
    auxi=new int[mem+memstep];
    for (l=0; l < k; l ++) {
      auxv[l]=values[l];
      auxi[l]=indexes[l];
    }
    auxv[k]=data; auxi[k]=i;
    for (l=k+1; l <= nz; l ++) {
      auxv[l]=values[l-1];
      auxi[l]=indexes[l-1];
    }
    delete [] values;
    delete [] indexes;
    values=auxv;
    indexes=auxi;
    mem += memstep;
    nz ++;
    return;
  }
  for (l=nz; l > k; l --) {
    values[l]=values[l-1];
    indexes[l]=indexes[l-1];
  }
  values[k]=data; indexes[k]=i;
  nz ++;
}

template <class T>
void sparse_vector<T>::del(int i)
{
  int j;
  #ifdef DEBUG
  if (i < 0 || i >= nz) {
    cerr << "Bad index in (sparse_vector) del(i)" << endl;
    return;
  }
  #endif
  for (j=i; j < nz-1; j ++) {
    values[j]=values[j+1];
    indexes[j]=indexes[j+1];
  }
  nz --;
}

template <class T>
void sparse_vector<T>::put(int i, T data)
{
  int k;
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Invalid index in (sparse_vector) put(i,r)" << endl;
    return;
  }
  #endif
  k=locate(i);
  if (k < nz && indexes[k] == i) {
    if (abs(data) < abs(small)) {
      del(k);
      return;
    }
    values[k]=data;
    return;
  }
  if (abs(data) < abs(small)) return;
  insert(i,k,data);
}

template<class T> class full_matrix {
private:
  int rows;
  int cols;
  T *values;
public:
  full_matrix() { rows=cols=0; }
  full_matrix(int row, int col) { init(row,col); }
  ~full_matrix() { del(); }
  void init(int row, int col);
  void del(void) { if (rows) delete [] values; rows=cols=0; }
  inline void clear(void);
  inline T& operator()(int i, int j);
  inline void put(int i, int j, T data);
  inline int getrows(void) { return rows; }
  inline int getcols(void) { return cols; }
  inline void operator=(full_matrix<T>& b);
  int lufacp(vector<int>& perm);
  void lusolvep(vector<T>& b, vector<int>& perm);
};

template <class T>
void full_matrix<T>::init(int row, int col)
{
  rows=row; cols=col; values=new T[rows*cols];
  #ifdef DEBUG
  if (!values) cerr << "Memory allocation error in full_matrix" << endl;
  #endif
}

template <class T>
inline void full_matrix<T>::clear(void)
{
  int i;
  for (i=0; i < rows*cols; i ++) values[i]=T(0.0);
}

template <class T>
inline T& full_matrix<T>::operator()(int i, int j)
{
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (full_matrix) ()" << endl;
    return values[0];
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (full_matrix) ()" << endl;
    return values[0];
  }
  #endif
  return values[i*cols+j];
}

template <class T>
inline void full_matrix<T>::put(int i, int j, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (full_matrix) put(i,i,r)!" << endl;
    return;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (full_matrix) put(i,i,r)!" << endl;
    return;
  }
  #endif
  values[i*cols+j]=data;
}

template <class T>
inline void full_matrix<T>::operator=(full_matrix<T>& b)
{
  int i;
  #ifdef DEBUG
  if (rows != b.rows || cols != b.cols) {
    cerr << "Wrong sizes in (full_matrix) =" << endl;
  }
  #endif
  for (i=0; i < rows*cols; i ++) {
    values[i]=b.values[i];
  }
}

template <class T>
int full_matrix<T>::lufacp(vector<int>& perm)
{
  int i, j, k, p;
  T swp;
  #ifdef DEBUG
  if (cols != rows) {
    cerr << "Wrong sizes in (full_matrix) lufacp(V)" << endl;
    return 1;
  }
  if (perm.getsize() < rows-1) {
    cerr << "Too short permutation vector in (full_matrix) lufacp(V)" << endl;
    return 1;
  }
  #endif  
  for (i=0; i < cols-1; i ++) {
    p=i;
    for (k=i+1; k < rows; k ++) {
      if (abs(values[k*cols+i]) > abs(values[p*cols+i])) p=k;
    }
    perm.put(i,p);
    swp=values[i*cols+i]; values[i*cols+i]=values[p*cols+i]; values[p*cols+i]=swp;
    if (values[i*cols+i] == T(0.0)) return 1;
    for (k=i+1; k < cols; k ++) values[k*cols+i] /= values[i*cols+i];
    for (j=i+1; j < cols; j ++) {
      swp=values[i*cols+j]; values[i*cols+j]=values[p*cols+j]; values[p*cols+j]=swp;
      for (k=i+1; k < cols; k ++) {
	values[k*cols+j] -= values[k*cols+i]*values[i*cols+j];
      }
    }
  }
  for (i=0; i < cols; i ++) {
    values[i*cols+i]=T(1.0)/values[i*cols+i];
  }
  return 0;
}

template <class T>
void full_matrix<T>::lusolvep(vector<T>& b, vector<int>& perm)
{
  int i, j;
  T swp;
  #ifdef DEBUG
  if (cols != rows || cols != b.getsize()) {
    cerr << "Wrong size in (full_matrix) lusolvep" << endl;
    return;
  }
  #endif
  for (i=0; i < cols-1; i ++) {
    swp=b(i); b.put(i,b(perm(i))); b.put(perm(i),swp);
    for (j=i+1; j < rows; j ++) {
      b.sub(j,values[j*cols+i]*b(i));
    }
  }
  for (i=cols-1; i >= 0; i --) {
    #ifdef DEBUG
    if (values[i*cols+i] == T(0.0)) {
      cerr << "Vanished diagonal in (full_matrix) lusolvep(V,V)!" << endl;
      return;
    }
    #endif
    b.mul(i,values[i*cols+i]);
    for (j=0; j < i; j ++) {
      b.sub(j,values[j*cols+i]*b(i));
    }
  }
}

template<class T> class band_matrix {
private:
  int rows;
  int cols;
  int left;
  int right;
  T *values;
public:
  band_matrix() { rows=0; }
  band_matrix(int row, int col, int le, int ri) { init(row,col,le,ri); }
  ~band_matrix() { if (rows) delete [] values; }
  void init(int row, int col, int left, int right);
  void clear(void);
  T operator()(int i, int j);
  void put(int i, int j, T data);
  int getrows(void) { return rows; }
  int getcols(void) { return cols; }
  void operator=(band_matrix& b);
  int lufac();
  void lusolve(vector<T>& b);
};

template <class T>
void band_matrix<T>::init(int nrows, int ncols, int nleft, int nright)
{
  rows=nrows; cols=ncols; left=nleft; right=nright;
  values=new T[rows*(left+right+1)];
  #ifdef DEBUG
  if (!values) {
    cerr << "Memory allocation error in (band_matrix) init(i,i,i,i)" << endl;
  }
  #endif
}

template <class T>
void band_matrix<T>::clear(void)
{
  int i;
  for (i=0; i < rows*(left+right+1); i ++) values[i]=0;
}

template <class T>
T band_matrix<T>::operator()(int i, int j)
{
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (band_matrix) (i,i)" << endl;
    return T(0.0);
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (band_matrix) (i,i)" << endl;
    return T(0.0);
  }
  #endif
  if (i-j > left || j-i > right) return T(0.0);
  return values[i*(left+right)+left+j];
}

template <class T>
void band_matrix<T>::put(int i, int j, T data)
{
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (band_matrix) put" << endl;
    return;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (band_matrix) put" << endl;
    return;
  }
  if (i-j > left || j-i > right) {
    cerr << "Band pattern error in (band_matrix) put" << endl;
    return;
  }
  #endif
  values[i*(left+right)+left+j]=data;
}

template <class T>
void band_matrix<T>::operator=(band_matrix<T>& b)
{
  int i;
  if (rows != b.rows || cols != b.cols ||
      left != b.left || right != b.right) {
    resize(b.rows, b.cols, b.left, b.right);
  }
  for (i=0; i < rows*(left+right+1); i ++) values[i]=b.values[i];
}

template<class T>
int band_matrix<T>::lufac(void)
{
  int i, j, k, l, m, err;
  #ifdef DEBUG
  if (cols != rows) {
    cerr << "Wrong size in (band_matrix) lufac" << endl;
    return 1;
  }
  #endif
  err=0;
  for (i=0; i < cols-1; i ++) {
    if (abs(values[i*(left+right+1)+left]) < sqrt(SMALL)) {
      values[i*(left+right+1)+left]=sqrt(SMALL); err=1;
    }
    values[i*(left+right+1)+left]=T(1.0)/values[i*(left+right+1)+left];
    for (k=i+1; k < min(rows,i+left+1); k ++) {
      if (abs(values[k*(left+right)+i+left]) < SMALL) continue;
      values[k*(left+right)+i+left] *= values[i*(left+right+1)+left];
      j=i+1; l=min(cols,i+right+1);
      m=l-j;
      if (m & 1) {
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
      }
      if (m & 2) {
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
      }
      while (j < l) {
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
	values[k*(left+right)+left+j] -=
	  values[k*(left+right)+left+i]*values[i*(left+right)+left+j];
	j ++;
      }
    }
  }
  if (abs(values[(rows-1)*(left+right+1)+left]) < sqrt(SMALL)) {
    values[(rows-1)*(left+right+1)+left]=sqrt(SMALL); err=1;
  }
  values[(rows-1)*(left+right+1)+left]=
    T(1.0)/values[(rows-1)*(left+right+1)+left];
  return err;
}

template <class T>
void band_matrix<T>::lusolve(vector<T>& b)
{
  int i, j, k, l;
  #ifdef DEBUG
  if (cols != rows || cols != b.getsize()) {
    cerr << "Wrong size in (band_matrix) lusolve" << endl;
    return;
  }
  #endif
  for (i=1; i < rows; i ++) {
    j=max(0,i-left);
    k=i-j;
    if (k & 1) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
    }
    if (k & 2) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
    }
    while (j < i) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j ++;
    }
  }
  for (i=rows-1; i >= 0; i --) {
    j=min(cols-1,i+right);
    k=j-i;
    if (k & 1) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
    }
    if (k & 2) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
    }
    while (j > i) {
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
      b.sub(i,values[i*(left+right)+left+j]*b(j));
      j --;
    }
    b.mul(i,values[i*(left+right+1)+left]);
  }
}

template<class T> class sparse_matrix {
private:
  int rows;
  int cols;
  int memstep;
  int *nz;
  int *mem;
  int **indexes;
  T **values;
  T small;
  int locate(int i, int j);
  void insert(int i, int j, int k, T data);
public:
  sparse_matrix() { rows=0; small=SMALL; }
  sparse_matrix(int row, int col, int res, int mems) { init(row,col,res,mems); }
  sparse_matrix(int row, int col, int *ia, int *ja, T *a) { init(row,col,ia,ja,a); }
  ~sparse_matrix() { del(); }
  void init(int row, int col, int res, int memstep);
  void init(int row, int col, int *ia, int *ja, T *a);
  void del(void);
  T getsmall(void) { return small; }
  void setsmall(T sm) { small=sm; }
  void clear(void);
  int getloc(int i, int j);
  T operator()(int i, int j);
  T put(int i, int j, T data);
  T add(int i, int j, T data);
  T sub(int i, int j, T data);
  T mul(int i, int j, T data);
  T div(int i, int j, T data);
  int getrows(void) { return rows; }
  int getcols(void) { return cols; }
  int getnz(int i) { return nz[i]; }
  int getindex(int i, int j) { return indexes[i][j]; }
  int* getindex(int i) { return indexes[i]; }
  T& getvalue(int i, int j) { return values[i][j]; }
};

template<class T>
void sparse_matrix<T>::init(int nrows, int ncols, int res, int step)
{
  int i;
  #ifdef DEBUG
  if (res < 1) {
    cerr << "You must reserve mem for sparse matrix" << endl;
    return;
  }
  if (step < 1) {
    cerr << "Memory step must be positive in sparse matrix" << endl;
    return;
  }
  #endif
  rows=nrows; cols=ncols; memstep=step;
  nz=new int[rows];
  mem=new int[rows];
  indexes=new int*[rows];
  values=new T*[rows];
  for (i=0; i < rows; i ++) {
    nz[i]=0; mem[i]=res;
    indexes[i]=new int[mem[i]];
  }
  for (i=0; i < rows; i ++) values[i]=new T[mem[i]];
  small=T(SMALL);
}

template<class T>
void sparse_matrix<T>::init(int row, int col, int *ia, int *ja, T *a)
{
  int i, j;
  rows=row; cols=col; memstep=0;
  nz=new int[rows];
  indexes=new int*[rows];
  values=new T*[rows];
  for (i=0; i < rows; i ++) {
    indexes[i]=&ja[ia[i]];
    values[i]=&a[ia[i]];
    nz[i]=ia[i+1]-ia[i];
  }
  small=SMALL;
}

template<class T>
void sparse_matrix<T>::del(void)
{
  if (rows == 0) return;
  if (memstep > 0) {
    for (int i=0; i < rows; i ++) {
      delete [] indexes[i];
      delete [] values[i];
    }
    delete [] mem;
  }
  delete [] indexes;
  delete [] values;
  delete [] nz;
  rows=0;
}

template<class T>
void sparse_matrix<T>::clear(void)
{
  int i;
  for (i=0; i < rows; i ++) {
    nz[i]=0;
  }
}

template<class T>
int sparse_matrix<T>::locate(int i, int j)
{
  int u, l, v;
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (sparse_matrix) locate(i,i)" << endl;
    return -1;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (sparse_matrix) locate(i,i)" << endl;
    return -1;
  }
  #endif
  if (nz[i] == 0) return 0;
  if (indexes[i][0] > j) return 0;
  if (indexes[i][nz[i]-1] < j) return nz[i];
  l=0; u=nz[i]-1;
  if (indexes[i][l] == j) return l;
  if (indexes[i][u] == j) return u;
  while (u-l > 1) {
    v=(u+l) >> 1;
    if (indexes[i][v] > j) {
      u=v;
    }
    else {
      l=v;
      if (indexes[i][l] == j) return l;
    }
  }
  if (indexes[i][l] == j) return l;
  return u;
}

template<class T>
int sparse_matrix<T>::getloc(int i, int j)
{
  int k;
  k=locate(i,j);
  if (k == nz[i]) return -1;
  if (indexes[i][k] != j) return -1;
  return k;
}

template<class T>
T sparse_matrix<T>::operator()(int i, int j)
{
  int k;
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (sparse_matrix) (i,j)" << endl;
    return -1;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (sparse_matrix) (i,j)" << endl;
    return -1;
  }
  #endif
  k=locate(i,j);
  if (k == nz[i]) return T(0.0);
  if (indexes[i][k] != j) return T(0.0);
  return values[i][k];
}

template<class T>
void sparse_matrix<T>::insert(int i, int j, int k, T data)
{
  int l;
  T *auxv;
  int *auxi;
  if (nz[i] == mem[i]) {
    auxv=new T[mem[i]+memstep];
    auxi=new int[mem[i]+memstep];
    for (l=0; l < k; l ++) {
      auxv[l]=values[i][l];
      auxi[l]=indexes[i][l];
    }
    auxv[k]=data; auxi[k]=j;
    for (l=k+1; l <= nz[i]; l ++) {
      auxv[l]=values[i][l-1];
      auxi[l]=indexes[i][l-1];
    }
    delete [] values[i];
    delete [] indexes[i];
    values[i]=auxv;
    indexes[i]=auxi;
    mem[i] += memstep;
    nz[i] ++;
    return;
  }
  for (l=nz[i]; l > k; l --) {
    values[i][l]=values[i][l-1];
    indexes[i][l]=indexes[i][l-1];
  }
  values[i][k]=data; indexes[i][k]=j;
  nz[i] ++;
}

template<class T>
T sparse_matrix<T>::put(int i, int j, T data)
{
  int k, l;
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (sparse_matrix) put(i,j,r)" << endl;
    return -1;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (sparse_matrix) put(i,j,r)" << endl;
    return -1;
  }
  #endif
  k=locate(i,j);
  if (k < nz[i] && indexes[i][k] == j) {
    if (abs(data) < abs(small)) {
      for (l=k; l < nz[i]-1; l ++) {
	values[i][k]=values[i][k+1];
	indexes[i][k]=indexes[i][k+1];
      }
      return T(0.0);
    }
    values[i][k]=data;
    return data;
  }
  if (abs(data) < abs(small)) return T(0.0);
  insert(i,j,k,data);
  return data;
}

template<class T>
T sparse_matrix<T>::add(int i, int j, T data)
{
  int k, l;
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (sparse_matrix) add(i,j,r)" << endl;
    return -1;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (sparse_matrix) add(i,j,r)" << endl;
    return -1;
  }
  #endif
  k=locate(i,j);
  if (k < nz[i] && indexes[i][k] == j) {
    if (abs(values[i][k]+data) < abs(small)) {
      for (l=k; l < nz[i]-1; l ++) {
	values[i][l]=values[i][l+1];
	indexes[i][l]=indexes[i][l+1];
      }
      nz[i] --;
      return T(0.0);
    }
    values[i][k] += data;
    return values[i][k];
  }
  if (abs(data) < abs(small)) return T(0.0);
  insert(i,j,k,data);
  return data;
}

template<class T>
T sparse_matrix<T>::sub(int i, int j, T data)
{
  return add(i,j,-data);
}

template<class T>
T sparse_matrix<T>::mul(int i, int j, T data)
{
  int k, l;
  #ifdef DEBUG
  if (i < 0 || i >= rows) {
    cerr << "Invalid row in (sparse_matrix) mul(i,j,r)" << endl;
    return -1;
  }
  if (j < 0 || j >= cols) {
    cerr << "Invalid column in (sparse_matrix) mul(i,j,r)" << endl;
    return -1;
  }
  #endif
  k=locate(i,j);
  if (k < nz[i] && indexes[i][k] == j) {
    if (abs(values[i][k]*data) < abs(small)) {
      for (l=k; l < nz[i]-1; l ++) {
	values[i][l]=values[i][l+1];
	indexes[i][l]=indexes[i][l+1];
      }
      return T(0.0);
    }
    values[i][k] *= data;
    return values[i][k];
  }
  return T(0.0);
}

template<class T>
T sparse_matrix<T>::div(int i, int j, T data)
{
  return mul(i,j,T(1.0)/data);
}

template<class Mat, class T>
void mult(vector<T>& b, Mat& A, vector<T>& x)
{
  int i, j;

  #ifdef DEBUG
  if (b.getsize() != A.getrows() || x.getsize() != A.getcols()) {
    cerr << "Bad sizes in mult(V,M,V)!" << endl;
    return;
  }
  #endif
  b.clear();
  addmult(b,A,x);
}

template<class Mat, class T>
void addmult(vector<T>& b, Mat& A, vector<T>& x)
{
  int i, j;

  #ifdef DEBUG
  if (b.getsize() != A.getrows() || x.getsize() != A.getcols()) {
    cerr << "Bad sizes in addmult(V,M,V)!" << endl;
    return;
  }
  #endif

  for (i=0; i < A.getrows(); i ++) {
    j=0;
    if (A.getnz(i) & 1) {
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
    }
    if (A.getnz(i) & 2) {
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
    }
    while (j < A.getnz(i)) {
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
      b.add(i,A.getvalue(i,j)*x(A.getindex(i,j))); j ++;
    }
  }
}

template<class Mat, class T>
void addmult(vector<T>& b, vector<T>& x, Mat& A)
{
  int i, j;

  #ifdef DEBUG
  if (b.getsize() != A.getcols() || x.getsize() != A.getrows()) {
    cerr << "Bad sizes in addmult(V,V,M)!" << endl;
    return;
  }
  #endif
  for (i=0; i < A.getrows(); i ++) {
    j=0;
    if (A.getnz(i) & 1) {
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
    }
    if (A.getnz(i) & 2) {
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
    }
    while (j < A.getnz(i)) {
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
      b.add(A.getindex(i,j),A.getvalue(i,j)*x(i)); j ++;
    }
  }
}

template<class Mat, class T>
void residual(vector<T>& r, Mat& A, vector<T>& x, vector<T>& b)
{
  int i;

  #ifdef DEBUG
  if (b.getsize() != A.getrows() || x.getsize() != A.getcols()) {
    cerr << "Bad sizes in residual(V,M,V,V)!" << endl;
    return;
  }
  if (r.getsize() != b.getsize()) {
    cerr << "Bad sizes in residual(V,M,V,V)!" << endl;
    return;
  }
  #endif
  mult(r,A,x);
  i=0;
  if (r.getsize() & 1) {
    r.put(i,b(i)-r(i)); i ++;
  }
  if (r.getsize() & 2) {
    r.put(i,b(i)-r(i)); i ++;
    r.put(i,b(i)-r(i)); i ++;
  }
  while (i < r.getsize()) {
    r.put(i,b(i)-r(i)); i ++;
    r.put(i,b(i)-r(i)); i ++;
    r.put(i,b(i)-r(i)); i ++;
    r.put(i,b(i)-r(i)); i ++;
  }
}

template<class Real, class Matrix, class Vector>
int sor(Matrix& A, Vector& x, Vector& b, Real omega, Real& eps, int maxit)
{
  Real alpha, beta;
  int i, j, it;

  #ifdef DEBUG
  if (A.hermit()) {
    cerr << "Hermitian matrices not supported in sor(M,V,V,r,r,i)!" << endl;
    return -1;
  }
  #endif
  alpha=2.0*eps;
  for (it=0; it < maxit && !(abs(alpha) < abs(eps)); it ++) {
    alpha=0.0;
    for (i=0; i < x.getsize(); i ++) {
      beta=b(i);
      j=0;
      if (A.getnz(i) & 1) {
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
      }
      if (A.getnz(i) & 2) {
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
      }
      while (j < A.getnz(i)) {
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
	beta -= A.getvalue(i,j)*x(A.getindex(i,j)); j ++;
      }
      alpha += beta*beta;
      beta=omega*beta/A(i,i);
      x.add(i, beta);
    }
    alpha=sqrt(alpha);
  }
  if (it < maxit-1) return it;
  return -it;
}

template<class T>
class graphnode {
 private:
 public:
  T tag;
  list<int> links;
  graphnode() { links.init(1,10,10); }
  graphnode(int m, int s) { links.init(1,m,s); }
  ~graphnode() { }
  void operator=(graphnode<T>& n) { tag=n.tag; links=n.links; }
};

template<class T>
class graph {
 private:
  int size, res;
  graphnode<T> *nodes;
 public:
  graph() { size=res=0; }
  graph(int sz) { init(sz); }
  graph(int sz, int *ig, int *jg) { init(sz,ig,jg); }
  ~graph() { del(); }
  void init(int sz) { size=0; res=sz; nodes=new graphnode<T>[sz]; }
  void init(int sz, int *ig, int *jg);
  void del(void) { if (res) delete [] nodes; size=res=0; }
  int getsize(void) { return size; }
  graphnode<T>& operator()(int i);
  void add(graphnode<T>& n);
  void add(T t, list<int>& l);
  void add(T t);
  void del(int i);
  void addlink(int i, int j);
};

template<class T>
void graph<T>::init(int sz, int *ig, int *jg)
{
  int i;
  size=res=sz;
  nodes=new graphnode<T>[size];
  for (i=0; i < size; i ++) nodes[i].links.init(ig[i+1]-ig[i],&jg[ig[i]]);
}

template<class T>
graphnode<T>& graph<T>::operator()(int i)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (graph) (i)" << endl;
    return nodes[0];
  }
  #endif
  return nodes[i];
}

template<class T>
void graph<T>::add(graphnode<T>& n)
{
  #ifdef DEBUG
  if (size >= res) {
    cerr << "Graph full in (graph) add(N)" << endl;
    return;
  }
  #endif
  nodes[size].tag=n.tag;
  nodes[size].links.clear();
  nodes[size].links.add(n.links);
  size ++;
}

template<class T>
void graph<T>::add(T t, list<int>& l)
{
  #ifdef DEBUG
  if (size >= res) {
    cerr << "Graph full in (graph) add(N)" << endl;
    return;
  }
  #endif
  nodes[size].tag=t;
  nodes[size].links.clear();
  nodes[size].links.add(l);
  size ++;
}

template<class T>
void graph<T>::add(T t)
{
  #ifdef DEBUG
  if (size >= res) {
    cerr << "Graph full in (graph) add(N)" << endl;
    return;
  }
  #endif
  nodes[size].tag=t;
  nodes[size].links.clear();
  size ++;
}

template<class T>
void graph<T>::del(int i)
{
  int j, k;
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (graph) del(i)" << endl;
    return;
  }
  #endif
  for (j=0; j < size; j ++) {
    k=nodes[j].links.which(i);
    if (k >= 0) nodes[j].links.del(k);
  }
  for (j=i+1; j < size; j ++) {
    nodes[j-1]=nodes[j];
  }
  size --;
}

template<class T>
void graph<T>::addlink(int i, int j)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (graph) addlink(i,i)" << endl;
    return;
  }
  #endif
  nodes[i].links.add(j);
}

template<class M, class V>
class ilu_precond {
private:
  int *nz, *diap, **index;
  M **value;
  int dim;
  M small;
  int locate(int i, int j);
public:
  ilu_precond() { dim=0; }
  ilu_precond(sparse_matrix<M>& m) { init(m); }
  ilu_precond(sparse_matrix<M>& m, int lev, int fill) { init(m,lev,fill); }
  ~ilu_precond() { del(); }
  void init(sparse_matrix<M>& m);
  void init(sparse_matrix<M>& m, int lev, int fill);
  void del(void);
//void reinit(sparse_matrix<M>& m) { ilu.del(); init(m); } 
//the above line does not compile with gcc 3.4.3
  void reinit(sparse_matrix<M>& m) { del(); init(m); }
  void coresolve(vector<V>& dest);
  void solve(sparse_matrix<M>& m, vector<V>& dest, vector<V>& src) {
    dest=src; coresolve(dest);
  }
};

template<class M, class V>
int ilu_precond<M,V>::locate(int i, int j)
{
  int u, l, v;
  if (nz[i] == 0) return -1;
  if (index[i][0] > j) return -1;
  if (index[i][nz[i]-1] < j) return -1;
  l=0; u=nz[i]-1;
  if (index[i][l] == j) return l;
  if (index[i][u] == j) return u;
  while (u-l > 1) {
    v=(u+l) >> 1;
    if (index[i][v] > j) {
      u=v;
    }
    else {
      l=v;
      if (index[i][l] == j) return l;
    }
  }
  if (index[i][l] == j) return l;
  return -1;
}

template<class M, class V>
void ilu_precond<M,V>::init(sparse_matrix<M>& m)
{
  int i, j, k, l, p;
  M mul;
  #ifdef DEBUG
  if (m.getrows() != m.getcols()) {
    cerr << "Not square matrix for ilu_precond" << endl;
    dim=0; return;
  }
  #endif
  small=m.getsmall();
  dim=m.getrows();
  nz=new int[dim];
  diap=new int[dim];
  index=new int*[dim];
  value=new M*[dim];
  for (i=0; i < dim; i ++) {
    if (m.getloc(i,i) < 0) nz[i]=m.getnz(i)+1;
    else nz[i]=m.getnz(i);
    index[i]=new int[nz[i]];
    value[i]=new M[nz[i]];
  }
  for (i=0; i < dim; i ++) {
    if (nz[i] == m.getnz(i)) {
      for (j=0; j < nz[i]; j ++) {
	index[i][j]=m.getindex(i,j);
	value[i][j]=m.getvalue(i,j);
      }
    }
    else {
      j=0;
      while (j < m.getnz(i) && m.getindex(i,j) < i) {
	index[i][j]=m.getindex(i,j);
	value[i][j]=m.getvalue(i,j);
	j ++;
      }
      index[i][j]=i;
      value[i][j]=sqrt(small);
      j ++;
      while (j < nz[i]) {
	index[i][j]=m.getindex(i,j-1);
	value[i][j]=m.getvalue(i,j-1);
	j ++;
      }
    }
  }
  diap[0]=locate(0,0);
  if (abs(value[0][diap[0]]) < small) value[0][diap[0]]=small;
  value[0][diap[0]]=M(1.0)/value[0][diap[0]];
  for (i=1; i < dim; i ++) {
    for (k=0; k < nz[i] && (p=index[i][k]) < i; k ++) {
      value[i][k] *= value[p][diap[p]];
      mul=value[i][k];
      j=0; l=diap[p]+1;
      while (j < nz[i] && l < nz[p]) {
	if (index[i][j] == index[p][l]) {
	  value[i][j] -= mul*value[p][l];
	  j ++; l ++; continue;
	}
	if (index[i][j] < index[p][l]) {
	  j ++; continue;
	}
	l ++;
      }
    }
    diap[i]=locate(i,i);
    if (abs(value[i][diap[i]]) < small) value[i][diap[i]]=small;
    value[i][diap[i]]=M(1.0)/value[i][diap[i]];
  }
}

template<class M, class V>
void ilu_precond<M,V>::init(sparse_matrix<M>& m, int lev, int fill)
{
  int i, j, k, l, p, nnz;
  M mul;
  #ifdef DEBUG
  if (m.getrows() != m.getcols()) {
    cerr << "Not square matrix for ilu_precond" << endl;
    dim=0; return;
  }
  if (lev < 0 || fill < 0) {
    cerr << "Negative parameters not allowed" << endl;
    dim=0; return;
  }
  #endif
  if (lev == 0 || fill == 0) { init(m); return; }
  small=m.getsmall();
  dim=m.getrows();
  nz=new int[dim];
  diap=new int[dim];
  index=new int*[dim];
  value=new M*[dim];

  list<int> ri(1,max(10,2*m.getnz(0)),max(5,m.getnz(0)));
  list<int> ra(1,max(10,2*m.getnz(0)),max(5,m.getnz(0)));

  if (m.getnz(0) == 0 || m.getindex(0,0) != 0) {
    nz[0]=m.getnz(0)+1;
    index[0]=new int[nz[0]];
    value[0]=new M[nz[0]];
    index[0][0]=0; value[0][0]=sqrt(small);
    for (i=0; i < m.getnz(0); i ++) {
      index[0][i+1]=m.getindex(0,i);
      value[0][i+1]=m.getvalue(0,i);
    }
    diap[0]=0;
  }
  else {
    nz[0]=m.getnz(0);
    index[0]=new int[nz[0]];
    value[0]=new M[nz[0]];
    for (i=0; i < m.getnz(0); i ++) {
      index[0][i]=m.getindex(0,i);
      value[0][i]=m.getvalue(0,i);
    }
    diap[0]=0;
  }
  value[0][diap[0]]=M(1.0)/value[0][diap[0]];
  for (i=1; i < dim; i ++) {
    nnz=m.getnz(i)+fill;
    ri.clear(); ra.clear();
    ri.add(m.getnz(i),m.getindex(i),m.getnz(i));
    k=0;
    while (ri.getsize() < nnz && k < lev && ri.getsize() > ra.getsize()) {
      ra=ri;
      for (l=0; l < ra.getsize() && ri.getsize() < nnz; l ++) {
	j=nnz-ri.getsize();
	ri.add(nz[ra(l)]-diap[ra(l)]-1,&index[ra(l)][diap[ra(l)]+1],j);
      }
      k ++;
    }
    ri.add(i);
    nz[i]=ri.getsize();
    index[i]=new int[nz[i]];
    value[i]=new M[nz[i]];
    j=k=0;
    while (j < ri.getsize()) {
      if (k >= m.getnz(i) || ri(j) < m.getindex(i,k)) {
	index[i][j]=ri(j); value[i][j]=M(0.0); j ++; continue;
      }
      index[i][j]=ri(j); value[i][j]=m.getvalue(i,k); j ++; k ++;
    }
    for (k=0; k < nz[i] && (p=index[i][k]) < i; k ++) {
      value[i][k] *= value[p][diap[p]];
      mul=value[i][k];
      j=0; l=diap[p]+1;
      while (j < nz[i] && l < nz[p]) {
	if (index[i][j] == index[p][l]) {
	  value[i][j] -= mul*value[p][l];
	  j ++; l ++; continue;
	}
	if (index[i][j] < index[p][l]) {
	  j ++; continue;
	}
	l ++;
      }
    }
    diap[i]=locate(i,i);
    if (abs(value[i][diap[i]]) < abs(sqrt(small))) {
      value[i][diap[i]]=sqrt(small);
    }
    value[i][diap[i]]=M(1.0)/value[i][diap[i]];
  }
}

template<class M, class V>
void ilu_precond<M,V>::del(void)
{
  int i;
  if (!dim) return;
  for (i=0; i < dim; i ++) {
    delete [] index[i];
    delete [] value[i];
  }
  delete [] nz;
  delete [] diap;
  delete [] index;
  delete [] value;
  dim=0;
}

template<class M, class V>
void ilu_precond<M,V>::coresolve(vector<V>& x)
{
  int i, j, k;
  #ifdef DEBUG
  if (x.getsize() != dim) {
    cerr << "Wrong size in (ilu_precond) solve!" << endl;
    return;
  }
  #endif
  for (i=1; i < dim; i ++) {
    j=0;
    if (diap[i] & 1) {
      x(i) -= value[i][j]*x(index[i][j]); j ++;
    }
    if (diap[i] & 2) {
      x(i) -= value[i][j]*x(index[i][j]); j ++;
      x(i) -= value[i][j]*x(index[i][j]); j ++;
    }
    while (j < diap[i]) {
      x(i) -= value[i][j]*x(index[i][j]); j ++;
      x(i) -= value[i][j]*x(index[i][j]); j ++;
      x(i) -= value[i][j]*x(index[i][j]); j ++;
      x(i) -= value[i][j]*x(index[i][j]); j ++;
    }
  }
  for (i=dim-1; i >= 0; i --) {
    j=nz[i]-1;
    if (j-diap[i] & 1) {
      x(i) -= value[i][j]*x(index[i][j]); j --;
    }
    if (j-diap[i] & 2) {
      x(i) -= value[i][j]*x(index[i][j]); j --;
      x(i) -= value[i][j]*x(index[i][j]); j --;
    }
    while (j > diap[i]) {
      x(i) -= value[i][j]*x(index[i][j]); j --;
      x(i) -= value[i][j]*x(index[i][j]); j --;
      x(i) -= value[i][j]*x(index[i][j]); j --;
      x(i) -= value[i][j]*x(index[i][j]); j --;
    }
    x(i) *= value[i][j];
  }
}

template<class Mat, class T, class Fac>
void facsmooth(Mat& A, vector<T>& x, vector<T>& b,
	       vector<T>& r, Fac& P, T omega, int it)
{
  int i;
  for (i=0; i < it; i ++) {
    residual(r,A,x,b);
    P.coresolve(r);
    x.addmlt(omega,r);
  }
}

class nodeorder {
 public:
  int num, add;
  nodeorder() { }
  nodeorder(int i) { num=i; add=0; }
  nodeorder(int i, int a) { num=i; add=a; }
  ~nodeorder() { }
  void operator=(nodeorder n) { num=n.num; add=n.add; }
  int operator<(nodeorder& n) {
    if (add < n.add) return 1;
    if (add > n.add) return 0;
    return num < n.num;
  }
  int operator>(nodeorder& n) {
    if (add > n.add) return 1;
    if (add < n.add) return 0;
    return num > n.num;
  }
  int operator==(nodeorder& n) { return num == n.num; }
};

class gmg_heap {
 private:
  nodeorder *values;
  int res, size;
  vector<int> *stat;
 public:
  void shiftup(int i);
  void shiftdown(int i);
  gmg_heap() { res=size=0; }
  gmg_heap(vector<int>& v) { init(v); }
  ~gmg_heap() { if (res) delete [] values; res=size=0; }
  void init(vector<int>& v);
  void del(void) { if (res) delete [] values; res=size=0; }
  inline int getsize(void) { return size; }
  nodeorder& operator()(int i);
  void add(nodeorder data);
  void heapify(void);
  void del(int i);
};

void gmg_heap::shiftup(int i)
{
  nodeorder tmp;
  while (i > 0 && values[i] < values[(i-1) >> 1]) {
    tmp=values[i];
    values[i]=values[(i-1) >> 1];
    values[(i-1) >> 1]=tmp;
    (*stat)(values[i].num)=i;
    (*stat)(values[(i-1) >> 1].num)=(i-1) >> 1;
    i=(i-1) >> 1;
  }
}

void gmg_heap::shiftdown(int i)
{
  int ms;
  nodeorder tmp;
  do {
    if ((i+1) << 1 >= size) ms=((i+1) << 1)-1;
    else {
      if (values[((i+1) << 1)-1] < values[(i+1) << 1]) ms=((i+1) << 1)-1;
      else ms=(i+1) << 1;
    }
    if (ms >= size || values[ms] > values[i]) break;
    tmp=values[i]; values[i]=values[ms]; values[ms]=tmp;
    (*stat)(values[i].num)=i;
    (*stat)(values[ms].num)=ms;
    i=ms;
  } while(1);
}

void gmg_heap::init(vector<int>& v)
{
  values=new nodeorder[v.getsize()];
  #ifdef DEBUG
  if (!values) {
    cerr << "Can't allocate memory for gmg_heap!" << endl;
    return;
  }
  #endif
  res=v.getsize();
  size=0;
  stat=&v;
}

nodeorder& gmg_heap::operator()(int i)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (gmg_heap) (i)!" << endl;
    return values[0];
  }
  #endif
  return values[i];
}

void gmg_heap::add(nodeorder data)
{
  #ifdef DEBUG
  if (size >= res) {
    cerr << "Heap full in add(T)!" << endl;
    return;
  }
  #endif
  values[size]=data;
  size ++;
}

void gmg_heap::heapify(void)
{
  int i;
  for (i=size-1; i >= 0; i --) shiftdown(i);
}

void gmg_heap::del(int i)
{
  #ifdef DEBUG
  if (i < 0 || i >= size) {
    cerr << "Bad index in (gmg_heap) del(i)!" << endl;
    return;
  }
  #endif
  size --;
  values[i]=values[size];
  (*stat)(values[i].num)=i;
  shiftdown(i);
}

typedef int gtag;

template<class T>
class gmg {
 private:
  int maxlvl, lvl, mu, bm, iinit, iter;
  T norm;
  sparse_matrix<T> *Ac, *res, *sAc, *sres;
  ilu_precond<T,T> *LU;
  full_matrix<T> Bf;
  band_matrix<T> Bb;
  vector<T> *xc, *fc;
  vector<T> r, relax, corrfac;
  vector<int> perm;
  graph<gtag> *graphs;
  list<int> *rowi;
  void calcA(sparse_matrix<T>& Ac, sparse_matrix<T>& res,
	     sparse_matrix<T>& A);
  void recalcA(sparse_matrix<T>& Ac, sparse_matrix<T>& res,
	       sparse_matrix<T>& A);
  void coarsen(graph<gtag>& grf, graph<gtag>& grc, sparse_matrix<T>& res,
	       list<int>& centr);
  void cycle(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
	     T eps, int maxit, int clvl);
  void ilucycle(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		T eps, int maxit, int clvl);
 public:
  gmg() { maxlvl=10; lvl=-1; mu=2; iinit=iter=0; }
  gmg(sparse_matrix<T>& A, graph<gtag>& gr, int mindim) {
    maxlvl=10; lvl=-1; mu=2; iinit=iter=0;
    init(A,gr,mindim);
  }
  ~gmg() { del(); }
  void init(sparse_matrix<T>& A, graph<gtag>& gr, int mindim);
  void reinit(sparse_matrix<T>& A);
  void del(void);
  int getlevels(void) { return lvl; }
  void setmu(int m) { mu=m; }
  int getmu(void) { return mu; }
  void setrelax(T om) { relax=om; }
  void setrelax(int l, T om) { relax.put(l,om); }
  T getrelax(int l) { return relax(l); }
  void setcorrfac(T om) { corrfac=om; }
  void setcorrfac(int l, T om) { corrfac.put(l,om); }
  T getcorrfac(int l) { return corrfac(l); }
  int getiter(void) { return iter; }
  T getnorm(void) { return norm; }
  sparse_matrix<T>& getsmat(int i) { return Ac[i]; }
  full_matrix<T>& getfmat(void) { return Bf; }
  band_matrix<T>& getbmat(void) { return Bb; }
  void solve(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
	     T eps, int maxit);
  void ilusolve(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		T eps, int maxit);
  void iluinit(sparse_matrix<T>& A);
  void iluinit(sparse_matrix<T>& A, int lev, int fill);
};

template<class T>
void gmg<T>::calcA(sparse_matrix<T>& Ac, sparse_matrix<T>& res,
		   sparse_matrix<T>& A)
{
  int i, j, k, l, it, jt;
  T val;
  list<int> n(1,10,10);

  sparse_vector<T> tmp(res.getrows(),5,4);
  tmp.setsmall(A.getsmall());
  vector<int> nzi(res.getrows());
  nzi.clear();
  for (i=0; i < A.getrows(); i ++) {
    tmp.clear();
    n.clear();
    for (j=0; j < A.getnz(i); j ++) {
      n.add(rowi[A.getindex(i,j)]);
    }
    for (jt=0; jt < n.getsize(); jt ++) {
      j=n(jt);
      val=0.0;
      k=l=0;
      while (k < A.getnz(i) && l < res.getnz(j)) {
	if (A.getindex(i,k) == res.getindex(j,l)) {
	  val += A.getvalue(i,k)*res.getvalue(j,l);
	  k ++;
	  l ++;
	}
	else {
	  if (A.getindex(i,k) < res.getindex(j,l)) k ++;
	  else l ++;
	}
      }
      tmp.put(j,val);
    }
    for (it=0; it < rowi[i].getsize(); it ++) {
      for (jt=0; jt < tmp.getnz(); jt ++) {
	Ac.add(rowi[i](it),tmp.getindex(jt),tmp.getvalue(jt)*
	       res.getvalue(rowi[i](it),nzi(rowi[i](it))));
      }
      nzi.add(rowi[i](it),1);
    }
  }
  delete [] rowi;
}

template<class T>
void gmg<T>::recalcA(sparse_matrix<T>& Ac, sparse_matrix<T>& res,
		     sparse_matrix<T>& A)
{
  int i, j, k, l, it, jt;
  T val;
  list<int> n(1,10,10);
  rowi=new list<int>[res.getcols()];
  for (i=0; i < res.getcols(); i ++) rowi[i].init(1,5,10);
  for (i=0; i < res.getrows(); i ++) {
    for (j=0; j < res.getnz(i); j ++) {
      rowi[res.getindex(i,j)].add(i);
    }
  }

  sparse_vector<T> tmp(res.getrows(),5,4);
  tmp.setsmall(A.getsmall());
  vector<int> nzi(res.getrows());
  nzi.clear();
  for (i=0; i < A.getrows(); i ++) {
    tmp.clear();
    n.clear();
    for (j=0; j < A.getnz(i); j ++) {
      n.add(rowi[A.getindex(i,j)]);
    }
    for (jt=0; jt < n.getsize(); jt ++) {
      j=n(jt);
      val=0.0;
      k=l=0;
      while (k < A.getnz(i) && l < res.getnz(j)) {
	if (A.getindex(i,k) == res.getindex(j,l)) {
	  val += A.getvalue(i,k)*res.getvalue(j,l);
	  k ++;
	  l ++;
	}
	else {
	  if (A.getindex(i,k) < res.getindex(j,l)) k ++;
	  else l ++;
	}
      }
      tmp.put(j,val);
    }
    for (it=0; it < rowi[i].getsize(); it ++) {
      for (jt=0; jt < tmp.getnz(); jt ++) {
	Ac.add(rowi[i](it),tmp.getindex(jt),tmp.getvalue(jt)*
	       res.getvalue(rowi[i](it),nzi(rowi[i](it))));
      }
      nzi.add(rowi[i](it),1);
    }
  }
  delete [] rowi;
}

template<class T>
void gmg<T>::coarsen(graph<gtag>& grf, graph<gtag>& grc, 
		     sparse_matrix<T>& res, list<int>& centr)
{
  int i, j, k, l;
  nodeorder n, o;
  vector<int> index(grf.getsize());
  gmg_heap U(index);

  for (i=0; i < grf.getsize(); i ++) {
    n.num=i;
    n.add=grf(i).links.getsize();
    U.add(n);
    index(i)=i;
  }
  U.heapify();

  // Pick the centroids of the new basis functions
  centr.clear();
  while (U.getsize() > 0) {
    n=U(0);
    U.del(0);
    index(n.num)=-1;
    centr.add(n.num);
    for (i=0; i < grf(n.num).links.getsize(); i ++) {
      if (index(grf(n.num).links(i)) < 0) continue;
      o=U(index(grf(n.num).links(i)));
      U.del(index(grf(n.num).links(i)));
      index(o.num)=-1;
      for (j=0; j < grf(o.num).links.getsize(); j ++) {
	if (index(grf(o.num).links(j)) < 0) continue;
	U(index(grf(o.num).links(j))).add --;
	U.shiftup(index(grf(o.num).links(j)));
      }
    }
  }
  U.del(); index.del();

  // Form the new basis functions
  res.init(centr.getsize(),grf.getsize(),5,5);
  for (i=0; i < centr.getsize(); i ++) {
    for (j=0; j < grf(centr(i)).links.getsize(); j ++) {
      k=centr.which(grf(centr(i)).links(j));
      if (k >= 0) continue;
      if (grf(centr(i)).tag >= 0 && grf(grf(centr(i)).links(j)).tag < 0) continue;
      res.put(i,grf(centr(i)).links(j),T(1.0));
    }
    res.put(i,centr(i),T(1.0));
  }
  r.resize(res.getcols());
  r.clear();
  for (i=0; i < res.getrows(); i ++) {
    for (j=0; j < res.getnz(i); j ++) {
      r.add(res.getindex(i,j),res.getvalue(i,j));
    }
  }
  for (i=0; i < r.getsize(); i ++) if (r(i) > SMALL) r.put(i,T(1.0)/r(i));
  for (i=0; i < res.getrows(); i ++) {
    for (j=0; j < res.getnz(i); j ++) {
      res.getvalue(i,j)=r(res.getindex(i,j));
    }
  }

  rowi=new list<int>[res.getcols()];
  for (i=0; i < res.getcols(); i ++) rowi[i].init(1,5,10);
  for (i=0; i < res.getrows(); i ++) {
    for (j=0; j < res.getnz(i); j ++) {
      rowi[res.getindex(i,j)].add(i);
    }
  }

  // Create the coarse graph
  grc.init(centr.getsize());
  for (i=0; i < centr.getsize(); i ++) {
    grc.add(grf(centr(i)).tag);
  }
  for (j=0; j < res.getcols(); j ++) {
    for (i=0; i < rowi[j].getsize(); i ++) {
      for (k=0; k < rowi[j].getsize(); k ++) {
	grc.addlink(rowi[j](i),rowi[j](k));
      }
    }
  }
}

template<class T>
void gmg<T>::cycle(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		   T eps, int maxit, int clvl)
{
  int i, it;
  T beta, zero;
  if (clvl == lvl) {
    xc[lvl]=fc[lvl];
    if (bm) Bb.lusolve(xc[lvl]);
    else Bf.lusolvep(xc[lvl],perm);
    return;
  }
  if (clvl == -1) {
    beta=1.0;
    for (it=1; it <= maxit; it ++) {
      zero=T(0.0); sor(A,x,f,relax(0),zero,2*mu);
      r.resize(A.getrows());
      residual(r,A,x,f);
      norm=sqrt(r.dot(r));
      if (!(norm > eps)) { iter=it; return; }
      beta=norm;
      mult(fc[0],res[0],r);
      xc[0].clear();
      if (lvl > 0) {
	for (i=0; i < mu; i ++) cycle(A,x,f,eps,maxit,0);
      }
      else cycle(A,x,f,eps,maxit,0);
      addmult(x,xc[0],res[0]);
    }
    iter=-maxit;
    return;
  }
  zero=T(0.0); sor(Ac[clvl],xc[clvl],fc[clvl],relax(clvl+1),zero,1);
  r.resize(Ac[clvl].getrows());
  residual(r,Ac[clvl],xc[clvl],fc[clvl]);
  mult(fc[clvl+1],res[clvl+1],r);
  xc[clvl+1].clear();
  if (clvl < lvl-1) {
    for (i=0; i < mu; i ++) cycle(A,x,f,eps,maxit,clvl+1);
  }
  else cycle(A,x,f,eps,maxit,clvl+1);
  addmult(xc[clvl],xc[clvl+1],res[clvl+1]);
  zero=T(0.0); sor(Ac[clvl],xc[clvl],fc[clvl],relax(clvl+1),zero,1);
}

template<class T>
void gmg<T>::ilucycle(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		      T eps, int maxit, int clvl)
{
  int i, it;
  T beta;
  if (clvl == lvl) {
    xc[lvl]=fc[lvl];
    if (bm) Bb.lusolve(xc[lvl]);
    else Bf.lusolvep(xc[lvl],perm);
    return;
  }
  if (clvl == -1) {
    beta=1.0;
    for (it=1; it <= maxit; it ++) {
      r.resize(A.getrows());
      facsmooth(A,x,f,r,LU[0],relax(0),1);
      residual(r,A,x,f);
      norm=sqrt(r.dot(r));
      if (!(norm > eps)) { iter=it; return; }
      beta=norm;
      mult(fc[0],res[0],r);
      xc[0].clear();
      if (lvl > 0) {
	for (i=0; i < mu; i ++) ilucycle(A,x,f,eps,maxit,0);
      }
      else ilucycle(A,x,f,eps,maxit,0);
      addmult(x,corrfac(0),xc[0],res[0]);
      r.resize(A.getrows());
      facsmooth(A,x,f,r,LU[0],relax(0),1);
    }
    iter=-maxit;
    return;
  }
  r.resize(Ac[clvl].getrows());
  facsmooth(Ac[clvl],xc[clvl],fc[clvl],r,LU[clvl+1],relax(clvl+1),1);
  residual(r,Ac[clvl],xc[clvl],fc[clvl]);
  mult(fc[clvl+1],res[clvl+1],r);
  xc[clvl+1].clear();
  if (clvl < lvl-1) {
    for (i=0; i < mu; i ++) ilucycle(A,x,f,eps,maxit,clvl+1);
  }
  else ilucycle(A,x,f,eps,maxit,clvl+1);
  addmult(xc[clvl],corrfac(clvl+1),xc[clvl+1],res[clvl+1]);
  r.resize(Ac[clvl].getrows());
  facsmooth(Ac[clvl],xc[clvl],fc[clvl],r,LU[clvl+1],relax(clvl+1),1);
}

template<class T>
void gmg<T>::init(sparse_matrix<T>& A, graph<gtag>& gr, int mindim)
{
  int i, j, k, l, ndim;

  #ifdef DEBUG
  if (A.getrows() != A.getcols()) {
    cerr << "Bad matrix sizes in (gmg) init(M,GR,i,io)" << endl;
    return;
  }
  if (A.getrows() != gr.getsize()) {
    cerr << "Incompatible matrix and graph in (gmg) init(M,GR,i,io)" << endl;
    return;
  }
  #endif
  list<int> centr(1,100,10);

  ndim=A.getrows();

  Ac=new sparse_matrix<T>[maxlvl];
  res=new sparse_matrix<T>[maxlvl];
  xc=new vector<T>[maxlvl];
  fc=new vector<T>[maxlvl];
  graph<gtag> *graphs=new graph<gtag>[maxlvl];
  r.init(A.getrows());

  j=0; l=0;
  for (k=0; k < A.getrows(); k ++) {
    if (j < A.getnz(k)) j=A.getnz(k);
    l += A.getnz(k);
  }
  if (ndim > mindim) {
    coarsen(gr,graphs[0],res[0],centr);
    ndim=res[0].getrows();
    l=l*ndim/(A.getrows()*A.getrows());
    Ac[0].init(ndim,ndim,max(l,5),max(l/2,4)); Ac[0].setsmall(A.getsmall());
    calcA(Ac[0],res[0],A);
    lvl=0;
  }
  for (i=1; i < maxlvl && ndim > mindim; i ++) {
    coarsen(graphs[i-1],graphs[i],res[i],centr);
    if (res[i].getrows() == ndim) {
      res[i].del();
      break;
    }
    ndim=res[i].getrows();
    l=l*ndim/(Ac[i-1].getrows()*Ac[i-1].getrows());
    Ac[i].init(ndim,ndim,max(l,5),max(l/2,4)); Ac[i].setsmall(A.getsmall());
    calcA(Ac[i],res[i],Ac[i-1]);
    lvl=i;
  }

  for (i=0; i <= lvl; i ++) {
    xc[i].init(Ac[i].getrows());
    fc[i].init(Ac[i].getrows());
  }

  if (lvl > -1) {
    k=0;
    for (i=0; i < Ac[lvl].getrows(); i ++) {
      for (j=0; j < Ac[lvl].getnz(i); j ++) {
	k=max(k,abs(i-Ac[lvl].getindex(i,j)));
      }
    }
    if (k < Ac[lvl].getrows() / 4) bm=1;
    else bm=0;
    if (bm) {
      Bb.init(Ac[lvl].getrows(),Ac[lvl].getcols(),k,k);
      Bb.clear();
      for (i=0; i < Ac[lvl].getrows(); i ++) {
	for (j=0; j < Ac[lvl].getnz(i); j ++) {
	  Bb.put(i,Ac[lvl].getindex(i,j),Ac[lvl].getvalue(i,j));
	}
      }
      Bb.lufac();
    }
    else {
      Bf.init(Ac[lvl].getrows(),Ac[lvl].getcols());
      Bf.clear();
      for (i=0; i < Ac[lvl].getrows(); i ++) {
	for (j=0; j < Ac[lvl].getnz(i); j ++) {
	  Bf.put(i,Ac[lvl].getindex(i,j),Ac[lvl].getvalue(i,j));
	}
      }
      perm.init(Bf.getrows());
      Bf.lufacp(perm);
    }
    Ac[lvl].del();
    relax.init(lvl+1); relax=T(1.0);
    corrfac.init(lvl+1); corrfac=T(1.0);
    delete [] graphs;
  }
  else {
    k=0;
    for (i=0; i < A.getrows(); i ++) {
      for (j=0; j < A.getnz(i); j ++) {
	k=max(k,abs(i-A.getindex(i,j)));
      }
    }
    if (k < A.getrows() / 4) bm=1;
    else bm=0;
    if (bm) {
      Bb.init(A.getrows(),A.getcols(),k,k);
      Bb.clear();
      for (i=0; i < A.getrows(); i ++) {
	for (j=0; j < A.getnz(i); j ++) {
	  Bb.put(i,A.getindex(i,j),A.getvalue(i,j));
	}
      }
      Bb.lufac();
    }
    else {
      Bf.init(A.getrows(),A.getcols());
      Bf.clear();
      for (i=0; i < A.getrows(); i ++) {
	for (j=0; j < A.getnz(i); j ++) {
	  Bf.put(i,A.getindex(i,j),A.getvalue(i,j));
	}
      }
      perm.init(Bf.getrows());
      Bf.lufacp(perm);
    }
  }
}

template<class T>
void gmg<T>::reinit(sparse_matrix<T>& A)
{
  int i, j, k, l, ndim;

  #ifdef DEBUG
  if (A.getrows() != A.getcols()) {
    cerr << "Bad matrix sizes in (gmg) init(M,GR,i,io)" << endl;
    return;
  }
  if (A.getrows() != res[0].getcols()) {
    cerr << "Incompatible matrix in (gmg) reinit(M)" << endl;
    return;
  }
  #endif

  if (lvl > -1) {
    Ac[lvl].init(res[lvl].getrows(),res[lvl].getrows(),7,7);
    Ac[0].clear();
    recalcA(Ac[0],res[0],A);
  }
  for (i=1; i <= lvl; i ++) {
    Ac[i].clear();
    recalcA(Ac[i],res[i],Ac[i-1]);
  }

  if (lvl > -1) {
    k=0;
    for (i=0; i < Ac[lvl].getrows(); i ++) {
      for (j=0; j < Ac[lvl].getnz(i); j ++) {
	k=max(k,abs(i-Ac[lvl].getindex(i,j)));
      }
    }
    if (k < Ac[lvl].getrows() / 4) bm=1;
    else bm=0;
    if (bm) {
      Bb.clear();
      for (i=0; i < Ac[lvl].getrows(); i ++) {
	for (j=0; j < Ac[lvl].getnz(i); j ++) {
	  Bb.put(i,Ac[lvl].getindex(i,j),Ac[lvl].getvalue(i,j));
	}
      }
      Bb.lufac();
    }
    else {
      Bf.clear();
      for (i=0; i < Ac[lvl].getrows(); i ++) {
	for (j=0; j < Ac[lvl].getnz(i); j ++) {
	  Bf.put(i,Ac[lvl].getindex(i,j),Ac[lvl].getvalue(i,j));
	}
      }
      Bf.lufacp(perm);
    }
    Ac[lvl].del();
  }
  else {
    k=0;
    for (i=0; i < A.getrows(); i ++) {
      for (j=0; j < A.getnz(i); j ++) {
	k=max(k,abs(i-A.getindex(i,j)));
      }
    }
    if (k < A.getrows() / 4) bm=1;
    else bm=0;
    if (bm) {
      Bb.clear();
      for (i=0; i < A.getrows(); i ++) {
	for (j=0; j < A.getnz(i); j ++) {
	  Bb.put(i,A.getindex(i,j),A.getvalue(i,j));
	}
      }
      Bb.lufac();
    }
    else {
      Bf.clear();
      for (i=0; i < A.getrows(); i ++) {
	for (j=0; j < A.getnz(i); j ++) {
	  Bf.put(i,A.getindex(i,j),A.getvalue(i,j));
	}
      }
      Bf.lufacp(perm);
    }
  }
}

template<class T>
void gmg<T>::del(void)
{
  if (lvl > -1) {
    delete [] Ac;
    delete [] res;
    delete [] xc;
    delete [] fc;
    lvl=-1;
  }
  if (iinit) {
    delete [] LU;
    iinit=0;
  }
}

template<class T>
void gmg<T>::solve(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		   T eps, int maxit)
{
  if (lvl < 0) {
    x=f;
    if (bm) Bb.lusolve(x);
    else Bf.lusolvep(x,perm);
    iter=1; norm=0.0;
    return;
  }
  cycle(A,x,f,eps,maxit,-1);
}

template<class T>
void gmg<T>::ilusolve(sparse_matrix<T>& A, vector<T>& x, vector<T>& f,
		      T eps, int maxit)
{
  if (lvl < 0) {
    x=f;
    if (bm) Bb.lusolve(x);
    else Bf.lusolvep(x,perm);
    iter=1; norm=0.0;
    return;
  }
  ilucycle(A,x,f,eps,maxit,-1);
}

template<class T>
void gmg<T>::iluinit(sparse_matrix<T>& A)
{
  int i;
  if (lvl < 0) return;
  if (iinit) delete [] LU;
  iinit=1;
  LU=new ilu_precond<T,T>[lvl+1];
  LU[0].init(A);
  for (i=1; i <= lvl; i ++) {
    LU[i].init(Ac[i-1]);
  }
}

template<class T>
void gmg<T>::iluinit(sparse_matrix<T>& A, int lev, int fill)
{
  int i;
  if (lvl < 0) return;
  if (iinit) delete [] LU;
  iinit=1;
  LU=new ilu_precond<T,T>[lvl+1];
  LU[0].init(A,lev,fill);
  for (i=1; i <= lvl; i ++) {
    LU[i].init(Ac[i-1],lev,fill);
  }
}
