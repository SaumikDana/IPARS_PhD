// IPACS - Integrated phase field advanced crack simulator 

/**
  This code is licensed under the "GNU GPL version 2 or later". See
  license.txt or https://www.gnu.org/licenses/gpl-2.0.html

  Copyright 2013-2014-2015-2016: Thomas Wick and Timo Heister and Sanghyun Lee

  This code was modified by Sanghyun Lee @2014-2015
  This code was mofified by Sanghyun Lee and Thomas Wick @2016


*/

// Features of the code (V 3.0)
// --------------------
// - Crack in a solid with phase-field
// - A quasi-monolithic approach using an extrapolation
// - primal dual active set strategy
// - predictor-corrector mesh adaptivity
// - 2d + 3d code version updated.
// - fluid-filled with pressure diffraction problem.
//   and coupled by fixed stress split



// Literature
// ----------
// Version V 1.0 is the basis of this code 
// and has been used in the paper
// 
// T. Heister, M. F. Wheeler, T. Wick; 
// A primal-dual active set method and predictor-corrector mesh adaptivity 
// for computing fracture propagation using a phase-field approach,
// Comp. Meth. Appl. Mech. Engrg., Vol. 290 (2015), pp. 466-495
//
//
//
// Version V 2.0 has been used in the conference paper
//
// T. Wick, S. Lee, M. F. Wheeler; 
// 3D Phase-Field for Pressurized Fracture Propagation in Heterogeneous Media
// ECCOMAS and IACM Coupled Problems, May 2015 at San Servolo, Venice/Italy
//
//
//
// Version V 3.0 (this version of the code) has been used in the paper
//
// S. Lee, M. F. Wheeler, T. Wick; 
// Pressure and fluid-driven fracture propagation in porous media 
// using an adaptive finite element phase field model,
// Comp. Meth. Appl. Mech. Engrg., Vol. 305 (2016), pp. 111–132
	

// Version V 3.1 (future version of the code) has been used in the paper
//
// S. Lee, M. F. Wheeler, T. Wick; 	
// Iterative coupling of flow, geomechanics and adaptive phase-field fracture including a level-set crack width approach, 
// in revision for publication to Journal of Computational and Applied Mathematics
// - Detailed fixed-stress computational analysis
// - comparison with other literature values
// - novel and very accurate crack-width computation,
//   by using a level-set function and a FE-representation 
//   of the width


// How to run in parallel in the terminal:
// make && mpirun -n no_of_cores ./step-fsi parameter_file


#include <deal.II/base/quadrature_lib.h>
#include <deal.II/base/logstream.h>
#include <deal.II/base/function.h>
#include <deal.II/base/utilities.h>
#include <deal.II/base/timer.h>
#include <deal.II/base/parameter_handler.h>
#include <deal.II/base/function_parser.h>
#include <deal.II/base/convergence_table.h>
#include <deal.II/base/config.h>
#include <deal.II/base/smartpointer.h>
#include <deal.II/base/index_set.h>

#include <deal.II/lac/block_vector.h>
#include <deal.II/lac/full_matrix.h>
#include <deal.II/lac/block_sparse_matrix.h>
#include <deal.II/lac/block_matrix_array.h>
#include <deal.II/lac/block_matrix_base.h>
#include <deal.II/lac/block_matrix.h>
#include <deal.II/lac/sparse_direct.h>
#include <deal.II/lac/constraint_matrix.h>
#include <deal.II/lac/solver_gmres.h>
#include <deal.II/lac/solver_bicgstab.h>
#include <deal.II/lac/solver_richardson.h>
#include <deal.II/lac/precondition_block.h>

#include <deal.II/grid/tria.h>
#include <deal.II/grid/grid_generator.h>
#include <deal.II/grid/tria_accessor.h>
#include <deal.II/grid/tria_iterator.h>
#include <deal.II/grid/tria_boundary_lib.h>
#include <deal.II/grid/grid_tools.h>
#include <deal.II/grid/grid_in.h>

#include <deal.II/dofs/dof_handler.h>
#include <deal.II/dofs/dof_renumbering.h>
#include <deal.II/dofs/dof_accessor.h>
#include <deal.II/dofs/dof_tools.h>

#include <deal.II/fe/fe_q.h>
#include <deal.II/fe/fe_dgq.h>
#include <deal.II/fe/fe_dgp.h>
#include <deal.II/fe/fe_system.h>
#include <deal.II/fe/fe_values.h>
#include <deal.II/fe/mapping_q1.h>
#include <deal.II/numerics/error_estimator.h>

#include <deal.II/numerics/vector_tools.h>
#include <deal.II/numerics/matrix_tools.h>
#include <deal.II/numerics/data_out.h>
#include <deal.II/numerics/solution_transfer.h>

#include <deal.II/lac/generic_linear_algebra.h>

namespace LA
{
  using namespace dealii::LinearAlgebraTrilinos;
}
#include <deal.II/distributed/tria.h>
#include <deal.II/distributed/grid_refinement.h>
#include <deal.II/distributed/solution_transfer.h>

// mjm
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <iostream>
#include <cmath>
#include <mpi.h>
#include "ipars_dealii.h"  //mjm

using namespace dealii;
using namespace std;



// mjm

class BitmapFile
{
public:
  BitmapFile(const std::string &name);

  double
  get_value(const double x, const double y) const;

private:
  std::vector<double> image_data;
  double hx, hy;
  int nx, ny;

  double
  get_pixel_value(const int i, const int j) const;
};

// The constructor of this class reads in the data that describes
// the obstacle from the given file name.
BitmapFile::BitmapFile(const std::string &name)
  :
  image_data(0),
  hx(0),
  hy(0),
  nx(0),
  ny(0)
{
  std::ifstream f(name.c_str());
  AssertThrow (f, ExcMessage (std::string("Can't read from file <") +
                              name + ">!"));

  std::string temp;
  getline(f, temp);
  f >> temp;
  if (temp[0]=='#')
    getline(f, temp);

  f >> nx >> ny;

  AssertThrow(nx > 0 && ny > 0, ExcMessage("Invalid file format."));

  for (int k = 0; k < nx * ny; k++)
    {
      unsigned int val;
      f >> val;
      image_data.push_back(val / 255.0);
    }

  hx = 1.0 / (nx - 1);
  hy = 1.0 / (ny - 1);
}

// The following two functions return the value of a given pixel with
// coordinates $i,j$, which we identify with the values of a function
// defined at positions <code>i*hx, j*hy</code>, and at arbitrary
// coordinates $x,y$ where we do a bilinear interpolation between
// point values returned by the first of the two functions. In the
// second function, for each $x,y$, we first compute the (integer)
// location of the nearest pixel coordinate to the bottom left of
// $x,y$, and then compute the coordinates $\xi,\eta$ within this
// pixel. We truncate both kinds of variables from both below
// and above to avoid problems when evaluating the function outside
// of its defined range as may happen due to roundoff errors.
double
BitmapFile::get_pixel_value(const int i,
                                 const int j) const
{
  assert(i >= 0 && i < nx);
  assert(j >= 0 && j < ny);
  return image_data[nx * (ny - 1 - j) + i];
}

double
BitmapFile::get_value(const double x,
                           const double y) const
{
  const int ix = std::min(std::max((int) (x / hx), 0), nx - 2);
  const int iy = std::min(std::max((int) (y / hy), 0), ny - 2);

  const double xi  = std::min(std::max((x-ix*hx)/hx, 1.), 0.);
  const double eta = std::min(std::max((y-iy*hy)/hy, 1.), 0.);

  return ((1-xi)*(1-eta)*get_pixel_value(ix,iy)
          +
          xi*(1-eta)*get_pixel_value(ix+1,iy)
          +
          (1-xi)*eta*get_pixel_value(ix,iy+1)
          +
          xi*eta*get_pixel_value(ix+1,iy+1));
}


template <int dim>
class BitmapFunction : public Function<dim>
{
public:
  BitmapFunction(const std::string & filename,
		 double x1_, double x2_, double y1_, double y2_, double minvalue_, double maxvalue_)
    : Function<dim>(1),
      f(filename), x1(x1_), x2(x2_), y1(y1_), y2(y2_), minvalue(minvalue_), maxvalue(maxvalue_)
  {}
  
  virtual
  double value (const Point<dim> &p,
		const unsigned int component = 0) const
  {
    Assert(dim==2, ExcNotImplemented());
    double x = (p(0)-x1)/(x2-x1);
    double y = (p(1)-y1)/(y2-y1);
          return minvalue + f.get_value(x,y)*(maxvalue-minvalue);
  }
private:
  BitmapFile f;
  double x1,x2,y1,y2;
  double minvalue, maxvalue;
};



// Define some tensors for cleaner notation later.
namespace Tensors
{   
  template <int dim> 
  inline
  Tensor<1,dim> 
  get_grad_p (unsigned int q,
	      std::vector<std::vector<Tensor<1,dim> > > old_solution_grads)	 
  {     
    Tensor<1,dim> grad_p;     
    grad_p[0] =  old_solution_grads[q][0][0];
    grad_p[1] =  old_solution_grads[q][0][1];
    if (dim == 3)
      grad_p[2] = old_solution_grads[q][0][2];

    return grad_p;
  }
  

  template <int dim> 
  inline
  double
  get_deviator_norm (const Tensor<2,dim> deviator)	 
  {     

    return std::sqrt(  deviator[0][0] * deviator[0][0] 
		       + deviator[0][1] * deviator[0][1]
		       + deviator[1][0] * deviator[1][0]
		       + deviator[1][1] * deviator[1][1]);
    if(dim == 3){
      cout<<" Wrong get deviator norm: to be fixed" << endl;
    }
    
  }
  
  
  template <int dim>
    inline Tensor<1, dim>
    get_grad_pf (
        unsigned int q,
        const std::vector<std::vector<Tensor<1, dim> > > & old_solution_grads)
    {
      Tensor<1, dim> grad_pf;
      grad_pf[0] = old_solution_grads[q][dim][0];
      grad_pf[1] = old_solution_grads[q][dim][1];
      if (dim == 3)
	  grad_pf[2] = old_solution_grads[q][dim][2];

      return grad_pf;
    }

  template <int dim> 
   inline
   Tensor<2,dim> 
   get_grad_u (unsigned int q,
	       std::vector<std::vector<Tensor<1,dim> > > old_solution_grads)	 
   {   
     Tensor<2,dim> grad_u;
     grad_u[0][0] =  old_solution_grads[q][0][0];
     grad_u[0][1] =  old_solution_grads[q][0][1];
	 
     grad_u[1][0] =  old_solution_grads[q][1][0];
     grad_u[1][1] =  old_solution_grads[q][1][1];
     if (dim == 3)
       {
	 grad_u[0][2] =  old_solution_grads[q][0][2];
	 
	 grad_u[1][2] =  old_solution_grads[q][1][2];
	 
	 grad_u[2][0] =  old_solution_grads[q][2][0];
	 grad_u[2][1] =  old_solution_grads[q][2][1];
	 grad_u[2][2] =  old_solution_grads[q][2][2];
       }
      
     return grad_u;
   }


 template <int dim> 
    inline
    Tensor<2,dim> 
    get_Identity ()
    {   
      Tensor<2,dim> identity;
      identity[0][0] = 1.0;
      identity[1][1] = 1.0; 
      if (dim == 3)
	  identity[2][2] = 1.0; 

      return identity;     
   }

 template <int dim> 
 inline
 Tensor<1,dim> 
 get_u (unsigned int q,
	std::vector<Vector<double> > old_solution_values)
   {
     Tensor<1,dim> u; 
     u[0] = old_solution_values[q](0);
     u[1] = old_solution_values[q](1);
     if (dim == 3)
	 u[2] = old_solution_values[q](2);

     return u;          
   }

 template <int dim> 
   inline
   Tensor<1,dim> 
   get_u_LinU (const Tensor<1,dim> phi_i_u)
   {
     Tensor<1,dim> tmp;  
     tmp[0] = phi_i_u[0];
     tmp[1] = phi_i_u[1];
     if (dim == 3)
       tmp[2] = phi_i_u[2];
     return tmp;    
   }

  template <int dim> 
  inline
  double
  get_divergence_u (const Tensor<2,dim> grad_u)	    
  {
    double tmp;
    if (dim == 2)
      {
	tmp = grad_u[0][0] + grad_u[1][1];
      }
    else if (dim == 3)
      {
	tmp = grad_u[0][0] + grad_u[1][1] + grad_u[2][2];
      }
    
    return tmp;  
  }
  
  
}


// Computing the radius later
template <int dim>
class Function_X: public Function<dim>
{
 public :
  Function_X ();

  virtual double value (const Point<dim> &p,
                        const unsigned int component=0) const;


};

template <int dim>
class Function_Y: public Function<dim>
{
 public :
  Function_Y ();

  virtual double value (const Point<dim> &p,
                        const unsigned int component=0) const;

};



template<int dim>
Function_X<dim>::Function_X
() :
  Function<dim>  (1)
{
}

template<int dim>
Function_Y<dim>::Function_Y
() :
  Function<dim>  (1)
{
}


// Computing the radius later
template <int dim>
double Function_X<dim>::value (const Point<dim> &p,
                                       const unsigned int /*component=0*/) const
{

  double x=p(0);
  double y=p(1);

  double return_value=0.;

  return x;

}


template <int dim>
double Function_Y<dim>::value (const Point<dim> &p,
                                       const unsigned int /*component=0*/) const
{

  double x=p(0);
  double y=p(1);

  double return_value=0.;

  return y;

}




template <int dim>
class Initial_Pressure: public Function<dim>
{
public :
  Initial_Pressure ()
    :
    Function<dim>(2)
  {
  }
  
  virtual double value (const Point<dim> &p,
			const unsigned int component=0) const;
  
  virtual void vector_value (const Point<dim> &p,
			     Vector<double>   &values) const;

};


template <int dim>
void Initial_Pressure<dim>::vector_value (const Point<dim> &p,
					Vector<double>   &values) const
{
  Assert (values.size() == dim,
          ExcDimensionMismatch (values.size(), dim));
  Assert (dim >= 2, ExcNotImplemented());

  double t= this->get_time();
  const double pi=numbers::PI;

  double x = p(0);
  double y = p(1);

 for (unsigned int comp = 0; comp < this->n_components; ++comp)
    values(comp) = Initial_Pressure<dim>::value(p, comp);

}


template <int dim>
double Initial_Pressure<dim>::value (const Point<dim> &p,
				   const unsigned int component) const
{

  double t= this->get_time();
  
  double return_value = 0;
  
  double x = p(0);
  double y = p(1);

  if(component == 0){
    return_value = 0.; //cos(x-y);
    //return_value = exp(-x-y*y);
  }
  else if(component == 1){
    return_value = 0.; //cos(x-y);
    //return_value = exp(-x-y*y);
  }
  else{
    cout<< "ERROR " << component << endl;
    exit(0);
  }

  return return_value;
}


// Class for reading parameters from external input files
class ParameterReader : public Subscriptor
{
  public:
    ParameterReader (
        ParameterHandler &);
    void
    read_parameters (
        const std::string);

  private:
    void
    declare_parameters ();
    ParameterHandler &prm;
};

ParameterReader::ParameterReader (
    ParameterHandler &paramhandler)
    :
        prm(paramhandler)
{
}

void
ParameterReader::read_parameters (
    const std::string parameter_file)
{
  declare_parameters();

  if (!prm.read_input(parameter_file, true))
    AssertThrow(false, ExcMessage("could not read .prm!"));
}

void
ParameterReader::declare_parameters ()
{
 prm.enter_subsection("Optimal parameters");
    {

      // position for first fracture ( 5 (center) - length 1 )
      prm.declare_entry("length 1", "1.0", Patterns::Double(0));

      // position for third fracture ( 5 (center) + length 2 )
      prm.declare_entry("length 2", "1.0", Patterns::Double(0));

    }
 prm.leave_subsection();



  prm.enter_subsection("Global parameters");
    {
      prm.declare_entry("darcy switch", "false", Patterns::Bool());

      prm.declare_entry("input file", "false", Patterns::Bool());

      prm.declare_entry("Global pre-refinement steps", "1",
          Patterns::Integer(0));

      prm.declare_entry("Local pre-refinement steps", "0",
          Patterns::Integer(0));

      prm.declare_entry("Adaptive refinement cycles", "0",
          Patterns::Integer(0));

      prm.declare_entry("Max No of timesteps", "1", Patterns::Integer(0));

      prm.declare_entry("Timestep size", "1.0", Patterns::Double(0));

      prm.declare_entry("Timestep size to switch to", "1.0", Patterns::Double(0));

      prm.declare_entry("Switch timestep after steps", "0", Patterns::Integer(0));

      prm.declare_entry("outer solver", "active set", 
			Patterns::Selection("active set|simple monolithic"));

      prm.declare_entry("test case", "sneddon 2d", Patterns::Selection("sneddon 2d|sneddon 3d|miehe tension|miehe shear|miehe tension 3D|multiple homo|multiple homo 3d|multiple het|multiple het 3d|multiple homo parallel|multiple homo parallel 3d|gupta 3d| hole"));

      prm.declare_entry("ref strategy", "phase field", 
			Patterns::Selection("phase field|fixed preref sneddon|fixed preref sneddon 3D|fixed preref miehe tension|fixed preref miehe shear|fixed preref multiple homo|fixed preref multiple het|global|mix"));

      prm.declare_entry("value phase field for refinement", "0.0", Patterns::Double(0));

      prm.declare_entry("Output filename", "solution_",  Patterns::Anything());
    }
  prm.leave_subsection();

  prm.enter_subsection("Problem dependent parameters");
    {
      prm.declare_entry("K reg", "1.0 * h", Patterns::Anything());

      prm.declare_entry("Eps reg", "1.0 * h", Patterns::Anything());

      prm.declare_entry("use peaceman well model", "false", Patterns::Bool());

      prm.declare_entry("Gamma penalization", "0.0", Patterns::Double(0));

      prm.declare_entry("alpha time", "0.0", Patterns::Double(0));

      prm.declare_entry("Pressure", "0.0", Patterns::Anything());

      prm.declare_entry("Fracture toughness G_c", "1.0", Patterns::Double(0));

      prm.declare_entry("Fracture toughness G_c 2", "1.0", Patterns::Double(0));

      prm.declare_entry("Density solid", "0.0", Patterns::Double(0));

      prm.declare_entry("Poisson ratio nu", "0.0", Patterns::Double(0));

      prm.declare_entry("E modulus", "0.0", Patterns::Double(0));

      prm.declare_entry("Lame mu", "0.0", Patterns::Double(0));

      prm.declare_entry("Lame lambda", "0.0", Patterns::Double(0));

      prm.declare_entry("pressure diff x1", "0.2", Patterns::Double(0));

      prm.declare_entry("pressure diff x2", "0.8", Patterns::Double(0));

      prm.declare_entry("pressure wellbore", "0.0", Patterns::Double(0));

      prm.declare_entry("tol fixed stress", "0.0", Patterns::Double(0));

      prm.declare_entry("tol fixed stress two", "0.0", Patterns::Double(0));

      prm.declare_entry("M Biot", "0.0", Patterns::Double(0));

      prm.declare_entry("alpha Biot coefficient", "0.0", Patterns::Double(0));

      prm.declare_entry("Compressibility Fracture", "0.0", Patterns::Double(0));

      prm.declare_entry("Viscosity Reservoir", "0.0", Patterns::Double(0));

      prm.declare_entry("Viscosity Fracture", "0.0", Patterns::Double(0));

      prm.declare_entry("Permeability Reservoir", "0.0", Patterns::Double(0));

      prm.declare_entry("Density Reservoir", "0.0", Patterns::Double(0));



    }
  prm.leave_subsection();

  prm.enter_subsection("Solver parameters");
    {
      prm.declare_entry("Use Direct Inner Solver", "false",
			Patterns::Bool());
      
      prm.declare_entry("Newton lower bound", "0.0",
          Patterns::Double(0));

      prm.declare_entry("Newton lower bound pressure", "0.0",
          Patterns::Double(0));

      prm.declare_entry("Newton maximum steps", "10",
          Patterns::Integer(0));

      prm.declare_entry("Upper Newton rho", "0.999",
			Patterns::Double(0));

      prm.declare_entry("Line search maximum steps", "5",
			Patterns::Integer(0));
      
      prm.declare_entry("Line search damping", "0.5",
			Patterns::Double(0));

      prm.declare_entry("Decompose stress in rhs", "0.0",
			Patterns::Double(0));

      prm.declare_entry("Decompose stress in matrix", "0.0",
			Patterns::Double(0));

    }
  prm.leave_subsection();

}




// Several classes for initial (phase-field) values
// Here, we prescribe initial (multiple) cracks 
template <int dim>
class InitialValuesGupta : public Function<dim>
{
public:
    InitialValuesGupta (const double min_cell_diameter)
      :
      Function<dim>(dim+1)
  {
    _min_cell_diameter = min_cell_diameter;
  }
  
  virtual double
  value (const Point<dim> &p, const unsigned int component = 0) const;
  
  virtual void
  vector_value (const Point<dim> &p, Vector<double> &value) const;
  
private:
  double _min_cell_diameter;
  
};

template <int dim>
double
InitialValuesGupta<dim>::value (const Point<dim> &p, const unsigned int component) const
{
  
  double width = _min_cell_diameter;
  double height = _min_cell_diameter;///2.0;
  
  double top = 5.0 + height;
  double bottom = 5.0 - height;
  
  // Defining the initial crack(s)
  // 0 = crack
  // 1 = no crack
  
  double return_value = 0.;
  
  if (component == dim) // Only for Phase-Field.
    {
      if ((((p(0) - 5.0) * (p(0) - 5.0) + (p(2) - 5.0) * (p(2) - 5.0)) <=  0.5 * 0.5) &&
	  ((p(1) >= bottom)  && (p(1) <= top))
	  )
	return_value = 0.0;
      else
	return_value = 1.0;
    }
    else 
      return_value = 0.;
  
  
  return return_value;    
}



template <int dim>
  void
  InitialValuesGupta<dim>::vector_value (
      const Point<dim> &p, Vector<double> &values) const
  {
    for (unsigned int comp = 0; comp < this->n_components; ++comp)
      values(comp) = InitialValuesGupta<dim>::value(p, comp);
  }




template <int dim>
class Function_G_c: public Function<dim>
{
 public :
  Function_G_c (const double G_c, const double G_c_2);

  virtual double value (const Point<dim> &p,
                        const unsigned int component=0) const;

private :
  double _G_c, _G_c_2;

};



template<int dim>
Function_G_c<dim>::Function_G_c
(const double G_c, const double G_c_2) : Function<dim>  (1)
{
  _G_c = G_c;
  _G_c_2=G_c_2;
}


template <int dim>
double Function_G_c<dim>::value (const Point<dim> &p,
                                 const unsigned int /*component=0*/) const
{

  double x=p(0);
  double y=p(1);

  double return_value=0.;


  // Different values for G_c (heterogeneous G_c)
  if( y >= 3 ) 
    return_value = _G_c_2;
  else if( y <=1 )
    return_value = _G_c_2;
  else 
  return_value = _G_c;

  return return_value;

}





// Several classes for initial (phase-field) values
// Here, we prescribe initial (multiple) cracks 
template <int dim>
  class InitialValuesSneddon : public Function<dim>
  {
    public:
      InitialValuesSneddon (
          const double min_cell_diameter)
          :
              Function<dim>(dim+1)
      {
        _min_cell_diameter = min_cell_diameter;
      }

      virtual double
      value (
          const Point<dim> &p, const unsigned int component = 0) const;

      virtual void
      vector_value (
          const Point<dim> &p, Vector<double> &value) const;

    private:
      double _min_cell_diameter;

  };

template <int dim>
  double
  InitialValuesSneddon<dim>::value (
      const Point<dim> &p, const unsigned int component) const
  {
    
    double width = _min_cell_diameter;
    double height = _min_cell_diameter;///2.0;
    double top = 0.;
    double bottom = 0.;
    double radius = 1.;

    if(dim == 2){
      top = 2.0 + height;
      bottom = 2.0 - height;
    }
    else if(dim == 3){
      top = 5.0 + height;
      bottom = 5.0 - height;
    }
    // Defining the initial crack(s)
    // 0 = crack
    // 1 = no crack

    double return_value = 0.;

    if(dim == 2)// actually in 2D.		       
      {
	if (component == dim) // Only for Phase-Field.
	  {
	    if (((p(0) >= 1.8 - width) && (p(0) <= 2.2 + width))
		&& ((p(1) >= bottom) && (p(1) <= top)))
	      return_value = 0.0;
	    else
	      return_value = 1.0;
	  }
	else 
	  return_value = 0.;
      }
    else if(dim == 3) // in 3D
      {
	if (component == dim) // Only for Phase-Field.
	  {
	    if ((((p(0) - 5.0) * (p(0) - 5.0) + (p(2) - 5.0) * (p(2) - 5.0)) <=  radius * radius) &&
		((p(1) >= bottom)  && (p(1) <= top))
		)
	      return_value = 0.0;
	    else
	      return_value = 1.0;
	  }
	else 
	  return_value = 0.;


      }
    
    return return_value;
  }

template <int dim>
  void
  InitialValuesSneddon<dim>::vector_value (
      const Point<dim> &p, Vector<double> &values) const
  {
    for (unsigned int comp = 0; comp < this->n_components; ++comp)
      values(comp) = InitialValuesSneddon<dim>::value(p, comp);
  }



////////////////////////////////////////////////////////////////////////////
// InitialValues for multiple Parallel 
template <int dim>
  class InitialValuesMultipleHomo_Parallel : public Function<dim>
  {
    public:
      InitialValuesMultipleHomo_Parallel (
          const double min_cell_diameter)
          :
              Function<dim>(dim+1)
      {
        _min_cell_diameter = min_cell_diameter;
      }

      virtual double
      value (const Point<dim> &p, const unsigned int component = 0) const;

      virtual void
      vector_value (const Point<dim> &p, Vector<double> &value) const;

    private:
      double _min_cell_diameter;

  };

template <int dim>
double
InitialValuesMultipleHomo_Parallel<dim>::value (
						const Point<dim> &p, const unsigned int component) const
{
  double width = _min_cell_diameter;
  double height = _min_cell_diameter;
  double top = 2.0 + height;
  double bottom = 2.0 - height;
  // Defining the initial crack(s)
  // 0 = crack
  // 1 = no crack
  if (component == dim)
    {
      if( dim == 2){
	if (((p(0) >= 1.5 - width) && (p(0) <= 1.5 + width))
	    && ((p(1) >= 1.5) && (p(1) <= 2.5)))
	  return 0.0;
	else if (((p(0) >= 2.5 - width) && (p(0) <= 2.5 + width))
		 && ((p(1) >= 1.5) && (p(1) <= 2.5)))
	  return 0.0;
	else
	  return 1.0;
      }
      else if(dim == 3){
	// 3D SHLEE
	// This needs to refine too much to be a circle !?
	//
	bool three_parallel = true;
	if(three_parallel == false){
	  
 	  if (
	      (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
	      &&
	      ((p(0) >= 1.5 - height)  && (p(0) <= 1.5 + height))
	      )
	    {
	      return 0.0;
	    }
	  else if(
		  (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
		  &&
		  ((p(0) >= 2.5 - height)  && (p(0) <= 2.5 + height))
		  )
	    {
	      return 0.0; 
	    }
	  /*
	  else if(
		  (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.75 * 0.75) 
		  &&
		  ((p(0) >= 2. - height)  && (p(0) <= 2. + height))
		  )
	    {
	      return 0.0; 
	    }
	  */
	  else
	    return 1.0;
	}
	// if three parallel = TRUE
	else{
	  
	  if (
	      (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  .5 * .5)
	      &&
	      ((p(0) >= 1. - height)  && (p(0) <= 1. + height))
	      )
	    {
	      return 0.0;
	    }
          else if(
		  (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  .5 * .5)
		  &&
		  ((p(0) >= 2. - height)  && (p(0) <= 2. + height))
                  )
	    {
	      return 0.0;
	    }
	  else if(
		  (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  .5 * .5)
		  &&
		  ((p(0) >= 3. - height)  && (p(0) <= 3. + height))
                  )
	    {
	      return 0.0;
	    }
          else
            return 1.0;
	}
      }
    }
  return 0.0;
}


template <int dim>
void
InitialValuesMultipleHomo_Parallel<dim>::vector_value (
						       const Point<dim> &p, Vector<double> &values) const
{
  for (unsigned int comp = 0; comp < this->n_components; ++comp)
    values(comp) = InitialValuesMultipleHomo_Parallel<dim>::value(p, comp);
}




/////////////////////////////////////////////////////////////////////////////


// Class for initial values multiple fractures in a homogeneous material
template <int dim>
  class InitialValuesMultipleHomo : public Function<dim>
  {
    public:
      InitialValuesMultipleHomo (
				 const double min_cell_diameter,
				 const double length_1,
				 const double length_2 )
          :
              Function<dim>(dim+1)
      {
        _min_cell_diameter = min_cell_diameter;
	_length_1 = length_1;
	_length_2 = length_2;
      }

      virtual double
      value (
          const Point<dim> &p, const unsigned int component = 0) const;

      virtual void
      vector_value (
          const Point<dim> &p, Vector<double> &value) const;

    private:
      double _min_cell_diameter;
      double _length_1;
      double _length_2;

  };

template <int dim>
  double
  InitialValuesMultipleHomo<dim>::value (
      const Point<dim> &p, const unsigned int component) const
{

  // TODO: for convergence studies, we fix 
  // _min_cell_diameter for the fracture initialization
  // And also the dirac injection should be scaled maybe
  // And also alpha_eps should be fixed

  double width = 1.*_min_cell_diameter;
  double height = 1.*_min_cell_diameter;
  double top = 2. + height;
  double bottom = 2. - height;
  // Defining the initial crack(s)
  // 0 = crack
  // 1 = no crack

  double center = 2.;
  double center_1 = center - _length_1 ;
  double center_2 = center + _length_2 ;

  double length = 0.2;
  if (component == dim)
    {
      if( dim == 2){
	
	/*
	if (((p(0) >= 1.8) && (p(0) <= 2.2))
	    && ((p(1) >= 2.0 - height) && (p(1) <= 2.0 + height)))
	  return 0.0;
	else
	  return 1.0;
	*/

	if (((p(0) >= center - height) && (p(0) <= center + height))
	    && ((p(1) >= center - length ) && (p(1) <= center + length)))
	  return 0.0;
	else if (((p(0) >= center_1 - height) && (p(0) <= center_1 + height))
	    && ((p(1) >= center - length ) && (p(1) <= center + length)))
	  return 0.0;
	else if (((p(0) >= center_2 - height) && (p(0) <= center_2  + height))
	    && ((p(1) >= center - length ) && (p(1) <= center + length)))
	  return 0.0;
	else
	  return 1.0;

	
	
      }
	
      else if(dim == 3){
	// 3D SHLEE
	// This needs to refine too much to be a circle !?

	if ( (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.25 * 0.25)
             &&
             ((p(0) >= 2. - height)  && (p(0) <= 2. + height))
             )
	  return 0.;
	else
	  return 1.;

/*
	if (
	    (((p(0) - 2.0) * (p(0) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
	    &&
	    ((p(1) >= 3. - height)  && (p(1) <= 3. + height))
	    ){
	  
	  return 0.0;
	}
	else if(
		(((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
		&&
		((p(0) >= 2.5 - height)  && (p(0) <= 2.5 + height))		      
		){
	  
	  return 0.0;
	  
	}
	  
	else
	    return 1.0;
*/	
	
      }
      
    }
  
  return 0.0;
}

template <int dim>
void
InitialValuesMultipleHomo<dim>::vector_value (
					      const Point<dim> &p, Vector<double> &values) const
{
  for (unsigned int comp = 0; comp < this->n_components; ++comp)
    values(comp) = InitialValuesMultipleHomo<dim>::value(p, comp);
}






// Class for initial values multiple fractures in a heterogeneous material
template <int dim>
  class InitialValuesMultipleHet : public Function<dim>
  {
    public:
      InitialValuesMultipleHet (
          const double min_cell_diameter)
          :
              Function<dim>(dim+1)
      {
        _min_cell_diameter = min_cell_diameter;
      }

      virtual double
      value (
          const Point<dim> &p, const unsigned int component = 0) const;

      virtual void
      vector_value (
          const Point<dim> &p, Vector<double> &value) const;

    private:
      double _min_cell_diameter;

  };

template <int dim>
  double
  InitialValuesMultipleHet<dim>::value (
      const Point<dim> &p, const unsigned int component) const
  {
  double width = _min_cell_diameter;
  double height = _min_cell_diameter;
    double top = 2.0 + height;
    double bottom = 2.0 - height;
    // Defining the initial crack(s)
    // 0 = crack
    // 1 = no crack
    if (component == dim)
      {
	if(dim == 2){
/*
	  if (((p(0) >= 2.5 - width/2.0) && (p(0) <= 2.5 + width/2.0))
	      && ((p(1) >= 0.8) && (p(1) <= 1.5)))
	    return 0.0;
	  else if (((p(0) >= 0.5) && (p(0) <= 1.5))
		   && ((p(1) >= 3.0 - height/2.0) && (p(1) <= 3.0 + height/2.0)))
	    return 0.0;
	  else
		return 1.0;
*/

	  if (((p(0) >= 2.5 - width) && (p(0) <= 2.5 + width))
              && ((p(1) >= 1.0) && (p(1) <= 1.5)))
            return 0.0;
          else if (((p(0) >= 1.0) && (p(0) <= 1.5))
                   && ((p(1) >= 2.5 - height) && (p(1) <= 2.5 + height)))
            return 0.0;
          else
	    return 1.0;

	}
	else if(dim == 3){
	  if (
	      (((p(0) - 2.0) * (p(0) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
	      &&
	      ((p(1) >= 3. - height)  && (p(1) <= 3. + height))
	      ){
	    
	    return 0.0;
	  }
	  else if(
		  (((p(1) - 2.0) * (p(1) - 2.0) + (p(2) - 2.0) * (p(2) - 2.0)) <=  0.5 * 0.5) 
		  &&
		  ((p(0) >= 2.5 - height)  && (p(0) <= 2.5 + height))		      
		  ){
	    
	    return 0.0;
	    
	  }
	  
	  else
	    return 1.0;
	  
	  
	}


      }

    return 0.0;
  }

template <int dim>
  void
  InitialValuesMultipleHet<dim>::vector_value (
      const Point<dim> &p, Vector<double> &values) const
  {
    for (unsigned int comp = 0; comp < this->n_components; ++comp)
      values(comp) = InitialValuesMultipleHet<dim>::value(p, comp);
  }


template <int dim>
  class InitialValuesMiehe : public Function<dim>
  {
    public:
      InitialValuesMiehe (
          const double min_cell_diameter)
          :
              Function<dim>(dim+1)
      {
        _min_cell_diameter = min_cell_diameter;
      }

      virtual double
      value (
          const Point<dim> &p, const unsigned int component = 0) const;

      virtual void
      vector_value (
          const Point<dim> &p, Vector<double> &value) const;

    private:
      double _min_cell_diameter;

  };

template <int dim>
  double
  InitialValuesMiehe<dim>::value (
      const Point<dim> &p, const unsigned int component) const
  {
    // Defining the initial crack(s)
    // 0 = crack
    // 1 = no crack
    if (component == dim)
      {
          return 1.0;
      }

    return 0.0;
  }

template <int dim>
  void
  InitialValuesMiehe<dim>::vector_value (
      const Point<dim> &p, Vector<double> &values) const
  {
    for (unsigned int comp = 0; comp < this->n_components; ++comp)
      values(comp) = InitialValuesMiehe<dim>::value(p, comp);
  }


// Several classes for Dirichlet boundary conditions
// for displacements for the single-edge notched test (Miehe 2010)
// Example 2a (Miehe tension)
template <int dim>
class BoundaryParabelTension : public Function<dim>
{
  public:
  BoundaryParabelTension (const double time)
    : Function<dim>(dim+1)
    {
      _time = time;
    }

  virtual double value (const Point<dim>   &p,
			const unsigned int  component = 0) const;

  virtual void vector_value (const Point<dim> &p,
			     Vector<double>   &value) const;

private:
  double _time;

};

// The boundary values are given to component
// with number 0.
template <int dim>
double
BoundaryParabelTension<dim>::value (const Point<dim>  &p,
			     const unsigned int component) const
{
  Assert (component < this->n_components,
	  ExcIndexRange (component, 0, this->n_components));
  

  double dis_step_per_timestep = 1.0;

if (component == 1)
  {
    return ( ((p(1) == 1.0) && (p(0) <= 1.0) && (p(0) >= 0.0))
	     ?
	     (1.0) * _time * dis_step_per_timestep : 0 );

  }



  return 0;
}



template <int dim>
void
BoundaryParabelTension<dim>::vector_value (const Point<dim> &p,
				    Vector<double>   &values) const
{
  for (unsigned int c=0; c<this->n_components; ++c)
    values (c) = BoundaryParabelTension<dim>::value (p, c);
}




// Dirichlet boundary conditions for 
// Miehe's et al. shear test
// Example 2b
template <int dim>
class BoundaryParabelShear : public Function<dim>
{
  public:
  BoundaryParabelShear (const double time)
    : Function<dim>(dim+1)
    {
      _time = time;
    }

  virtual double value (const Point<dim>   &p,
			const unsigned int  component = 0) const;

  virtual void vector_value (const Point<dim> &p,
			     Vector<double>   &value) const;

private:
  double _time;

};

// The boundary values are given to component
// with number 0.
template <int dim>
double
BoundaryParabelShear<dim>::value (const Point<dim>  &p,
			     const unsigned int component) const
{
  Assert (component < this->n_components,
	  ExcIndexRange (component, 0, this->n_components));


  double dis_step_per_timestep = -1.0;

if (component == 0)
  {
    return ( ((p(1) == 1.0) )
	     ?
	     (1.0) * _time * dis_step_per_timestep : 0 );

  }


  return 0;
}



template <int dim>
void
BoundaryParabelShear<dim>::vector_value (const Point<dim> &p,
				    Vector<double>   &values) const
{
  for (unsigned int c=0; c<this->n_components; ++c)
    values (c) = BoundaryParabelShear<dim>::value (p, c);
}



template <int dim>
  class InitialValuesHole : public Function<dim>
  {
  public:
    InitialValuesHole (const double min_cell_diameter)
      :
      Function<dim>(dim+1)
    {
      _min_cell_diameter = min_cell_diameter;
    }
    
    virtual double
    value (const Point<dim> &p, const unsigned int component = 0) const;
    
    virtual void
    vector_value (const Point<dim> &p, Vector<double> &value) const;
    
  private:
    double _min_cell_diameter;
    
};

template <int dim>
double
InitialValuesHole<dim>::value (const Point<dim> &p, const unsigned int component) const
{
  // Defining the initial crack(s)
  // 0 = crack
  // 1 = no crack

  double width = _min_cell_diameter;
  double height = _min_cell_diameter;

  if (component == dim){
     if (((p(0) >= 0. - width) && (p(0) <= 0. + width))
         && ((p(1) >= 0.) && (p(1) <= 0.5)))
        return 0.0;
     else  if (((p(0) >= 0.) && (p(0) <= 0.5))
       && ((p(1) >= 0. - height) && (p(1) <= 0. + height)))
        return 0.0;
     else
        return 1.0;
 }

  
  return 0.0;
}

template <int dim>
void
InitialValuesHole<dim>::vector_value (const Point<dim> &p, Vector<double> &values) const
{
  for (unsigned int comp = 0; comp < this->n_components; ++comp)
    values(comp) = InitialValuesHole<dim>::value(p, comp);
}



template <int dim>
  class InitialValues_Pressure : public Function<dim>
  {
  public:
    InitialValues_Pressure (const double min_cell_diameter)
      :
      Function<dim>(1)
    {
      _min_cell_diameter = min_cell_diameter;
    }
    
    virtual double
    value (const Point<dim> &p, const unsigned int component = 0) const;
        
  private:
    double _min_cell_diameter;
    
};

template <int dim>
double
InitialValues_Pressure<dim>::value (const Point<dim> &p, const unsigned int component) const
{  
  return 0.;
}



// Main program
template <int dim>
class FracturePhaseFieldProblem
{
public:
  
  FracturePhaseFieldProblem (const unsigned int degree, ParameterHandler &);
  void
  run ();
  
private:

  // General functions for initialization
  void set_runtime_parameters ();
  void determine_mesh_dependent_parameters();

  // This function for 
  // heterogeneous computations might only work in sequentiell running
  void make_material_vectors();


  // Pressure functions
  void setup_system_pressure ();
  void assemble_system_matrix_pressure ();   
  void assemble_system_rhs_pressure ();  
  void set_initial_bc_pressure (const double time);
  void set_newton_bc_pressure ();  
  void solve_pressure ();
  void newton_iteration_pressure(const double time);

  // Displacement phase-field functions
  void set_initial_values ();

  void setup_system ();
  void assemble_system (bool residual_only);
  void assemble_nl_residual ();
  
  void assemble_diag_mass_matrix();
  void set_initial_bc (const double time);
  void set_newton_bc ();
  void set_boundary_indicators();
  unsigned int solve ();
  double newton_active_set();
  void update_active_set_and_constraints();


  // Level set functions
  void setup_system_level_set ();
  void assemble_system_level_set ();
  void assemble_system_level_set_by_phasefield ();
  void solve_level_set ();

  // Width functions
  void setup_system_width ();
  void assemble_system_width ();
  void solve_width ();

  void write_solutions();
  void read_solutions();


  // General functions for computing 
  // quantities of interest etc.
  double
  compute_point_value (
		       const DoFHandler<dim> & dofh, const LA::MPI::BlockVector & vector,
		       const Point<dim> & p, const unsigned int component) const;

  void
  output_results () const;
  
  void output_bd(Vector<double> bd_indicator_v);
  void  compute_functional_values ();

  // Compute stresses in Miehe's test (only mechanics)
  void compute_load();
  
  double  compute_cod (const double eval_line);
  
  double compute_energy();
  
  void set_material_ids();
  void compute_stress_per_cell();

  
  bool
  refine_mesh ();

  void
  project_back_phase_field ();
  
  // end of function declarations


  
  MPI_Comm mpi_com;
  
  const unsigned int degree;
  ParameterHandler &prm;
  
  parallel::distributed::Triangulation<dim> triangulation;
  
  FESystem<dim> fe;
  DoFHandler<dim> dof_handler;
  
  // This is for All Combined Constraints
  ConstraintMatrix all_constraints;
  // Only Active Set constraints.
  ConstraintMatrix only_activeset_constraints;
  // This is for newton boundary condition constraint
  ConstraintMatrix constraints_update;
  ConstraintMatrix constraints_hanging_nodes;
  
  LA::MPI::BlockSparseMatrix system_pde_matrix;
  LA::MPI::BlockVector solution, newton_update,
    old_solution, old_old_solution, system_pde_residual;
  LA::MPI::BlockVector old_fixed_stress_solution, tmp_solution;

  LA::MPI::BlockVector system_total_residual;
  
  LA::MPI::BlockVector diag_mass, diag_mass_relevant;


  FESystem<dim>        fe_pressure;
  DoFHandler<dim>      dof_handler_pressure;
  ConstraintMatrix     constraints_pressure;  
  ConstraintMatrix     constraints_level_set;  
  ConstraintMatrix     constraints_width;  
  BlockSparsityPattern      sparsity_pattern_pressure; 
  
  FESystem<dim>      fe_level_set;
  DoFHandler<dim>    dof_handler_level_set;
 
  SparsityPattern      sparsity_pattern_level_set;
  LA::MPI::SparseMatrix         system_matrix_level_set;
 
  LA::MPI::Vector      solution_level_set, old_timestep_solution_level_set;
  LA::MPI::Vector     system_rhs_level_set;


  FESystem<dim>      fe_width;
  DoFHandler<dim>    dof_handler_width;
  SparsityPattern      sparsity_pattern_width;
  LA::MPI::SparseMatrix         system_matrix_width;
 
  LA::MPI::Vector      solution_width;
  LA::MPI::Vector     system_rhs_width;

  LA::MPI::BlockSparseMatrix system_matrix_pressure;   
  
  LA::MPI::BlockVector solution_pressure, 
    newton_update_pressure, 
    old_timestep_solution_pressure, 
    old_fixed_stress_solution_pressure, 
    system_rhs_pressure, tmp_solution_pressure;

  std::vector<IndexSet> partition_pressure;
  std::vector<IndexSet> partition_relevant_pressure;
 
  IndexSet relevant_set_pressure;

  IndexSet locally_owned_level_set;
  IndexSet relevant_set_level_set;

  IndexSet locally_owned_width;
  IndexSet relevant_set_width;


  double beta_fixed_stress, beta_fixed_stress_fracture;
  double gamma_fixed_stress;

  double theta;
  
  double global_fracture_pressure_value;
  Vector<double> solution_fracture_zone;
  Vector<double> solution_fluid_or_structure_mat;
  Vector<double> solution_stress_per_cell;
  Vector<double> permeability_per_cell;

  ConditionalOStream pcout;
  TimerOutput timer;


  double d_old_radius, d_radius;
  int counter_r;
  
  IndexSet active_set;
  
  Function<dim> * func_emodulus;
  Function<dim> * func_emodulus_0;
  Function<dim> * func_emodulus_1;
  Function<dim> * func_emodulus_2;
  Function<dim> * func_emodulus_3;
  Function<dim> * func_emodulus_4;
  
  std::vector<IndexSet> partition;
  std::vector<IndexSet> partition_relevant;
  
  IndexSet relevant_set;

  std::vector<std::vector<bool> > constant_modes;
  
  LA::MPI::PreconditionAMG preconditioner_solid;
  LA::MPI::PreconditionAMG preconditioner_phase_field;
  
  // Global variables for timestepping scheme
// mjm  unsigned int timestep_number;
  int timestep_number;
  unsigned int max_no_timesteps;
  double timestep, timestep_size_2, time;
  unsigned int switch_timestep;
  struct OuterSolverType
  {
    enum Enum{active_set, simple_monolithic};
  };
  typename OuterSolverType::Enum outer_solver;
  
  struct TestCase
  {
    enum Enum{sneddon_2d, sneddon_3d, miehe_tension, miehe_shear, miehe_tension_3D, 
	      multiple_homo,multiple_homo_3d,
	      multiple_het,multiple_het_3d,multiple_homo_parallel,multiple_homo_parallel_3d, 
	      gupta_3d, hole};
  };
  typename TestCase::Enum test_case;
  
  struct RefinementStrategy
  {
    enum Enum{phase_field_ref, fixed_preref_sneddon,  fixed_preref_sneddon_3D , 
	      fixed_preref_miehe_tension,
	      fixed_preref_miehe_shear, fixed_preref_multiple_homo, 
	      multiple_homo_3d, fixed_preref_multiple_het, 
	      global, mix};
  };
  typename RefinementStrategy::Enum refinement_strategy;
  
  bool direct_solver;
  
  double force_structure_x_biot, force_structure_y_biot;
  double force_structure_x, force_structure_y;
  
  // Biot parameters
   double  c_F, viscosity_F;
  double dirac_force_injection;
  double c_biot, alpha_biot, lame_coefficient_biot, K_biot, density_biot;
  
  double gravity_x, gravity_y, volume_source, traction_x, traction_y,
    traction_x_biot, traction_y_biot;
  

  // Well - model parameters
  double R_e; //Equivalent Radius for the well
  double R_w; // Radius for the wellbore
  double R_log_value;
  double h_3; //Thickness of the well
  double Perm_well_11,Perm_well_22,Perm_well_33;
  double max_pressure_well_bore;
  double TOL_Fixed_Stress, TOL_Fixed_Stress_Two;

  // Structure parameters
  double density_structure;
  double lame_coefficient_mu, lame_coefficient_lambda, poisson_ratio_nu;
  
  double cell_diameter;

  FunctionParser<1> func_pressure;
  
  double constant_k, alpha_eps,
    G_c, G_c_2, viscosity_biot;
  
  double E_modulus, E_prime;

  double pressure_diff_x_1, pressure_diff_x_2, pressure_wellbore;

  double min_cell_diameter, norm_part_iterations, value_phase_field_for_refinement;
  
  unsigned int n_global_pre_refine, n_local_pre_refine, n_refinement_cycles;
  
  double lower_bound_newton_residuum, lower_bound_newton_residual_pressure;
  unsigned int max_no_newton_steps;

  unsigned int max_no_line_search_steps;
  double line_search_damping;
  double decompose_stress_rhs, decompose_stress_matrix;
  std::string filename_basis;
  double old_timestep, old_old_timestep;
  bool use_old_timestep_pf;

  //Output File for Load Value
  std::ofstream outputFile;
  std::ofstream outputFile_2;


  std::ofstream outputFile_C;
  std::ofstream outputFile_R;
  std::ofstream outputFile_Perm;

  // TODO: check again bDarcy flag (it is independent of alpha_biot)
  bool bDarcy, bPeaceman_well_model;

  bool b_input_file;

  double maximum_width;
  
  double length_1, length_2;

  Tensor<1,dim,double> point_0;
  Tensor<1,dim,double> point_1;
  Tensor<1,dim,double> point_2;
  Tensor<1,dim,double> point_3;
  Tensor<1,dim,double> point_4;

  };

// The constructor of this class is comparable
// to other tutorials steps, e.g., step-22, and step-31.
template <int dim>
FracturePhaseFieldProblem<dim>::FracturePhaseFieldProblem (
							   const unsigned int degree, ParameterHandler &param)
  :
  mpi_com(MPI_COMM_WORLD),
  degree(degree),
  prm(param),
  triangulation(mpi_com),
  
  // We define two separate finite elements
  // for the displacement-phase-field system
  // and for the pressure and the two
  // systems are coupled via fixed-stress.
  fe(FE_Q<dim>(degree), dim, FE_Q<dim>(degree), 1),
  dof_handler(triangulation),

  fe_pressure(FE_Q<dim>(degree), 1),
  dof_handler_pressure (triangulation),
 
  fe_level_set(FE_Q<dim>(degree),1),
  dof_handler_level_set (triangulation),

  fe_width(FE_Q<dim>(degree),1),
  dof_handler_width (triangulation),
 
  pcout(std::cout, (Utilities::MPI::this_mpi_process(mpi_com) == 0)),
  timer(mpi_com, pcout, TimerOutput::summary, 
  	TimerOutput::cpu_and_wall_times),
  d_old_radius(0.), 
  d_radius(0.),
  counter_r(0)
{
  prm.enter_subsection("Global parameters");
  bDarcy = prm.get_bool("darcy switch");
  b_input_file = prm.get_bool("input file");
  prm.leave_subsection();

  outputFile.open("Load.txt");
  outputFile_2.open("COD.txt");
  
  outputFile_R.open("Rmesh.txt");
  outputFile_C.open("Cmesh.txt");
  outputFile_Perm.open("Perm.txt");

  
}





template <int dim>
void FracturePhaseFieldProblem<dim>::setup_system_pressure ()
{

  
  // We set runtime parameters to drive the problem.
  // These parameters could also be read from a parameter file that
  // can be handled by the ParameterHandler object (see step-19)
  system_matrix_pressure.clear ();
  
  dof_handler_pressure.distribute_dofs (fe_pressure);  

  std::vector<unsigned int> block_component (1,0);
  DoFRenumbering::component_wise (dof_handler_pressure, block_component);

  partition_pressure.clear();
  partition_pressure.push_back(dof_handler_pressure.locally_owned_dofs());
  

  DoFTools::extract_locally_relevant_dofs(dof_handler_pressure, relevant_set_pressure);

  partition_relevant_pressure.clear();
  partition_relevant_pressure.push_back(relevant_set_pressure);

  std::vector<unsigned int> dofs_per_block (1);
  DoFTools::count_dofs_per_block (dof_handler_pressure, 
				  dofs_per_block, 
				  block_component);  
  
  const unsigned int n_p = dofs_per_block[0];
  
  pcout << "DoFs: pressure \t +"
	<< dof_handler_pressure.n_dofs()
	<< std::endl;


  {				 
    constraints_pressure.clear ();
    constraints_pressure.reinit(relevant_set_pressure);
  
    set_newton_bc_pressure ();
  
    DoFTools::make_hanging_node_constraints (dof_handler_pressure,
					     constraints_pressure);
 
    
  }
  constraints_pressure.close ();



  
    
  {
    TrilinosWrappers::BlockSparsityPattern csp(partition_pressure, mpi_com);
    
    DoFTools::make_sparsity_pattern(dof_handler_pressure,
				    csp,
				    constraints_pressure,
				    false,
				    Utilities::MPI::this_mpi_process(mpi_com)); 
    
    csp.compress();
    system_matrix_pressure.reinit (csp);
  }

 

  // Actual solution at time step n
  solution_pressure.reinit (partition_relevant_pressure, mpi_com);


  // Old timestep solution at time step n-1
  old_timestep_solution_pressure.reinit (partition_relevant_pressure, mpi_com);
  
  // Old fixed stress solution at time step n-1
  old_fixed_stress_solution_pressure.reinit (partition_relevant_pressure, mpi_com);

  // Updates for Newton's method
  newton_update_pressure.reinit (partition_relevant_pressure, mpi_com);

  // Residual for  Newton's method
  system_rhs_pressure.reinit (partition_pressure, mpi_com);

  // A tmp solution vector
  tmp_solution_pressure.reinit (partition_relevant_pressure, mpi_com);
  
  solution_stress_per_cell.reinit(triangulation.n_active_cells());
  solution_fluid_or_structure_mat.reinit(triangulation.n_active_cells());
  solution_fracture_zone.reinit(triangulation.n_active_cells());
  
  //cout<<"!!!!!!!!!!!!!!!!!!!!!!!!!"<<endl;
  //cout<<"triangulation.n_active_cells() = "<< triangulation.n_active_cells() << endl;
  permeability_per_cell.reinit(triangulation.n_active_cells());
  //cout<<"!!!!!!!!!!!!!!!!!!!!!!!!!"<<endl;

}



template <int dim>
void FracturePhaseFieldProblem<dim>::setup_system_level_set()
{

  
  // We set runtime parameters to drive the problem.
  // These parameters could also be read from a parameter file that
  // can be handled by the ParameterHandler object (see step-19)
  system_matrix_level_set.clear ();
  
  dof_handler_level_set.distribute_dofs (fe_level_set);  

  locally_owned_level_set = dof_handler_level_set.locally_owned_dofs();

  DoFTools::extract_locally_relevant_dofs(dof_handler_level_set, relevant_set_level_set);

  
  pcout << "DoFs: Level Set \t +"
	<< dof_handler_level_set.n_dofs()
	<< std::endl;


  {				 
    constraints_level_set.clear ();
    constraints_level_set.reinit(relevant_set_level_set);
  
    
    DoFTools::make_hanging_node_constraints (dof_handler_level_set,
					     constraints_level_set);


    // Sanity check
//    VectorTools::interpolate_boundary_values(dof_handler_level_set,
//      0,
//      ZeroFunction<dim>(1),
//      constraints_level_set);


 
    
  }
  constraints_level_set.close ();



  
    
  {
    TrilinosWrappers::SparsityPattern csp(locally_owned_level_set, mpi_com);
    
    DoFTools::make_sparsity_pattern(dof_handler_level_set,
				    csp,
				    constraints_level_set,
				    false,
				    Utilities::MPI::this_mpi_process(mpi_com)); 
    
    csp.compress();
    system_matrix_level_set.reinit (csp);
  }

 

  // Actual solution at time step n 
  solution_level_set.reinit (relevant_set_level_set, mpi_com);

  // Actual solution at time step n 
  old_timestep_solution_level_set.reinit (relevant_set_level_set, mpi_com);

  // Residual for  Newton's method
  system_rhs_level_set.reinit (locally_owned_level_set, mpi_com);


}




template <int dim>
void FracturePhaseFieldProblem<dim>::setup_system_width()
{

  
  // We set runtime parameters to drive the problem.
  // These parameters could also be read from a parameter file that
  // can be handled by the ParameterHandler object (see step-19)
  system_matrix_width.clear ();
  
  dof_handler_width.distribute_dofs (fe_width);  

  locally_owned_width = dof_handler_width.locally_owned_dofs();

  DoFTools::extract_locally_relevant_dofs(dof_handler_width, relevant_set_width);

  
  pcout << "DoFs: Width \t +"
	<< dof_handler_width.n_dofs()
	<< std::endl;


  {				 
    constraints_width.clear ();
    constraints_width.reinit(relevant_set_width);
  
    
    DoFTools::make_hanging_node_constraints (dof_handler_width,
					     constraints_width);


    // Zero width on the other boundaries
    VectorTools::interpolate_boundary_values(dof_handler_width,
      0,
      ZeroFunction<dim>(1),
      constraints_width);


 
    
  }
  constraints_width.close ();



  
    
  {
    TrilinosWrappers::SparsityPattern csp(locally_owned_width, mpi_com);
    
    DoFTools::make_sparsity_pattern(dof_handler_width,
				    csp,
				    constraints_width,
				    false,
				    Utilities::MPI::this_mpi_process(mpi_com)); 
    
    csp.compress();
    system_matrix_width.reinit (csp);
  }

 

  // Actual solution at time step n 
  solution_width.reinit (relevant_set_width, mpi_com);

  // Residual for  Newton's method
  system_rhs_width.reinit (locally_owned_width, mpi_com);


}






// Currently not used but might be useful 
// for other applications to distinguish 
// different parts of the domain.
template <int dim>
void FracturePhaseFieldProblem<dim>::set_material_ids (){
  
double fracture_area = 0.;


  QGauss<dim> quad(degree+2);
  FEValues<dim> fe_values_phase_field (fe, quad, update_values | update_quadrature_points | update_JxW_values);			   
  const unsigned int  n_q_points  = quad.size();

  std::vector<Vector<double> > old_solution_values(n_q_points,
						   Vector<double>(dim+1));
  
  typename DoFHandler<dim>::active_cell_iterator
    cell = dof_handler.begin_active(),
    endc = dof_handler.end();
  
  unsigned int q_counter, q_counter_ref, interface_cell_counter, cell_counter;
  

  
  interface_cell_counter = 0;
  cell_counter = 0;


  for (; cell!=endc; ++cell, cell_counter++)
    if (cell->is_locally_owned())  { 	  	  
    
      fe_values_phase_field.reinit (cell);
      fe_values_phase_field.get_function_values (solution, old_solution_values);

      q_counter = 0;
      q_counter_ref = 0;
      for (unsigned int q=0; q<n_q_points; ++q)
	{
	  
	  double pf = old_solution_values[q](dim);
	  if (pf < 0.1)
	    {

    fracture_area += fe_values_phase_field.JxW (q);
	      // Mark fracture cells
	      cell->set_material_id(1); 
	      //pcout << cell << std::endl;
	      solution_fracture_zone(cell_counter) = 1;
	    }
	}
      
    }

  double sum_fracture_area = 0.;
   sum_fracture_area = Utilities::MPI::sum(fracture_area, mpi_com);

   outputFile_2 << time << " " <<  sum_fracture_area << endl;

  
}



template <int dim>
void
FracturePhaseFieldProblem<dim>::assemble_system_level_set_by_phasefield ()
{

  cout <<"by_phsaefield " << endl;

  LA::MPI::Vector localized_solution(system_rhs_level_set);
  localized_solution.reinit(solution_level_set,false,true);

  localized_solution=0.;

  LA::MPI::BlockVector distributed_solution(partition);
  distributed_solution = solution;


  localized_solution = distributed_solution.block(1);
  localized_solution.add(-0.5);

  /*
  
  typename DoFHandler<dim>::active_cell_iterator cell =
    dof_handler.begin_active(), endc = dof_handler.end();
  std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
  
  for (; cell != endc; ++cell)
    if (cell->is_locally_owned())
      {
	cell->get_dof_indices(local_dof_indices);
	
	for (unsigned int i=0;i<fe.dofs_per_cell;++i)
	  {
	    const unsigned int comp_i = fe.system_to_component_index(i).first;
	    if (comp_i != dim)
	      continue; // only look at phase field

	    if (comp_i == dim){
	      localized_solution(local_dof_indices[i]) = distributed_solution(local_dof_indices[i])-0.5;
	    }
	  }
      }
  */

  solution_level_set.reinit(localized_solution, false, true);


}


template <int dim>
void
FracturePhaseFieldProblem<dim>::assemble_system_level_set ()
{

  system_matrix_level_set = 0;
  system_rhs_level_set = 0;
  
  QGauss<dim>  quadrature_formula(degree+2);
  QGauss<dim-1>  face_quadrature_formula(degree+2);
    
  FEValues<dim> fe_values (fe_level_set, quadrature_formula,
                         update_values | update_gradients | update_JxW_values);


  FEFaceValues<dim> fe_face_values (fe_level_set, face_quadrature_formula, 
				    update_quadrature_points  |				    
				    update_JxW_values);

  const unsigned int   dofs_per_cell = fe_level_set.dofs_per_cell;
  const unsigned int   n_q_points    = quadrature_formula.size();
  const unsigned int n_face_q_points   = face_quadrature_formula.size();

 
  FullMatrix<double>   cell_matrix (dofs_per_cell, dofs_per_cell);
  Vector<double>       cell_rhs (dofs_per_cell);

 
  std::vector<types::global_dof_index> local_dof_indices (dofs_per_cell);

  std::vector<double> old_timestep_level_set_values(n_q_points);
 
  typename DoFHandler<dim>::active_cell_iterator
  cell = dof_handler_level_set.begin_active(),
  endc = dof_handler_level_set.end();


  for (; cell!=endc; ++cell)
    if (cell->subdomain_id() == triangulation.locally_owned_subdomain())
      {
      
      fe_values.reinit (cell);

      cell_matrix = 0;
      cell_rhs = 0;

      fe_values.get_function_values (old_timestep_solution_level_set, old_timestep_level_set_values);


      double force = 0.0;
      if (cell->material_id() == 0)
	{
	  force = -1.0e+1; //-5.0;
	}
      else if (cell->material_id() == 1)
	{
	  //pcout << cell << std::endl;
	  force = 1.0e+1; //+5.0;
	}
      
      
      for (unsigned int q_point=0; q_point<n_q_points; ++q_point)
	for (unsigned int i=0; i<dofs_per_cell; ++i){
	  for (unsigned int j=0; j<dofs_per_cell; ++j)
	    
            cell_matrix(i,j) += (fe_values.shape_value(i,q_point) * fe_values.shape_value (j, q_point)
				 + fe_values.shape_grad (i, q_point) *
                                 fe_values.shape_grad (j, q_point)) 
	      * fe_values.JxW (q_point);
	  
	  cell_rhs(i) += (fe_values.shape_value (i, q_point) *
			  force *
			  fe_values.JxW (q_point));
	  
	  cell_rhs(i) += (fe_values.shape_value (i, q_point) *
			  old_timestep_level_set_values[q_point] *
			  fe_values.JxW (q_point));
	  
	  
	  
	}
      
      if (cell->material_id() == 0)
	{	   
	  for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face)
	    if (cell->neighbor_index(face) != -1)	       
	      if (cell->material_id() !=  cell->neighbor(face)->material_id() &&
		  cell->face(face)->boundary_indicator()!=0
		  // Here we assume that the boundary indicator is '0'
		  )
		{
		  
		  fe_face_values.reinit (cell, face);
		  
		  for (unsigned int q=0; q<n_face_q_points; ++q)
		    {
		      for (unsigned int i=0; i<dofs_per_cell; ++i)
			{
			  for (unsigned int j=0; j<dofs_per_cell; ++j)
			    {
			      // The penalty must be large enough
			      // otherwise the level-set iso function is not 
			      // correctly imposed
			      cell_matrix(i,j) += 1.0e+3 * fe_face_values.JxW(q);
			    }
			}
		      
		    }
		  
		}
	  
	}


      
     

      cell->get_dof_indices (local_dof_indices);

      constraints_level_set.distribute_local_to_global (cell_matrix,
							cell_rhs,
							local_dof_indices,
							system_matrix_level_set,
							system_rhs_level_set);

      }
  
  system_matrix_level_set.compress(VectorOperation::add);
  system_rhs_level_set.compress(VectorOperation::add);
  
  

  
  
  
}

// TODO
// in other loops to get width values


template <int dim>
void
FracturePhaseFieldProblem<dim>::assemble_system_width ()
{ 

  system_matrix_width = 0;
  system_rhs_width = 0;

  // As in DG methods, we need a penalty
  // parameter to enforce interior weakly prescribed Dirichlet values.
  double penalty_parameter = 1.0e+3;

  QGauss<dim>  quadrature_formula(degree+2);
  QGauss<dim-1>  face_quadrature_formula(degree+2);
  
  FEValues<dim> fe_values (fe_width, quadrature_formula,
                         update_values | update_gradients | update_JxW_values);
      
    
  FEFaceValues<dim> fe_face_values (fe_width, face_quadrature_formula, 
				    update_quadrature_points  |				    
				    update_JxW_values);


 FEValues<dim> fe_values_solid(fe, quadrature_formula,
			  update_values | update_quadrature_points | update_JxW_values
			  | update_gradients);

 FEFaceValues<dim> fe_face_values_solid (fe, face_quadrature_formula, 
					 update_quadrature_points  | 
					 update_values | 
					 update_gradients |
				    update_JxW_values);


 FEValues<dim> fe_values_level_set (fe_level_set, quadrature_formula,
				    update_values    |
				    update_quadrature_points  |
				    update_JxW_values |
				    update_gradients);

 FEFaceValues<dim> fe_face_values_level_set (fe_level_set, face_quadrature_formula, 
					     update_quadrature_points  | 
					     update_values | 
					     update_gradients |
					     update_JxW_values);


  const unsigned int   dofs_per_cell = fe_width.dofs_per_cell;
  const unsigned int   n_q_points    = quadrature_formula.size();
  const unsigned int n_face_q_points   = face_quadrature_formula.size();

 
  FullMatrix<double>   cell_matrix (dofs_per_cell, dofs_per_cell);
  Vector<double>       cell_rhs (dofs_per_cell);

 
  std::vector<types::global_dof_index> local_dof_indices (dofs_per_cell);


  std::vector<Vector<double> > old_solution_solid_pff_face_values(n_face_q_points,
						   Vector<double>(dim+1));

  std::vector<std::vector<Tensor<1,dim> > > old_solution_solid_pff_face_grads (n_face_q_points,
								std::vector<Tensor<1,dim> > (dim+1));


  std::vector<Tensor<1,dim> > old_solution_level_set_face_grads(n_face_q_points);
 


  typename DoFHandler<dim>::active_cell_iterator
  cell = dof_handler_width.begin_active(),
  endc = dof_handler_width.end();

  typename DoFHandler<dim>::active_cell_iterator
    cell_level_set = dof_handler_level_set.begin_active();

  typename DoFHandler<dim>::active_cell_iterator
    cell_displacement_phase_field = dof_handler.begin_active();
 
  double maximum_width = solution_width.linfty_norm();

  for (; cell!=endc; ++cell, ++cell_level_set, ++cell_displacement_phase_field)
    if (cell->subdomain_id() == triangulation.locally_owned_subdomain())
      {
	
	fe_values.reinit (cell);
	fe_values_solid.reinit (cell_displacement_phase_field);
	fe_values_level_set.reinit(cell_level_set);
	
	cell_matrix = 0;
	cell_rhs = 0;
	

	
	
	double force = 0.0;
	if (cell->material_id() == 0)
	  {
	    force = 0.0; 
	  }
	else if (cell->material_id() == 1)
	  {
	    // TODO: we could prescribe some force
	    // such that the width is better approximated in the middle
	    // It should be of the order of the aperture value
	    //force = 0.0; 
	    force =  maximum_width * 1e+2;//1.0e-3;
	  }
	
	
	for (unsigned int q_point=0; q_point<n_q_points; ++q_point)
	  for (unsigned int i=0; i<dofs_per_cell; ++i){
	    for (unsigned int j=0; j<dofs_per_cell; ++j)
	      
	      cell_matrix(i,j) += (fe_values.shape_grad (i, q_point) *
				   fe_values.shape_grad (j, q_point)) *
		fe_values.JxW (q_point);

	    
	    cell_rhs(i) += (fe_values.shape_value (i, q_point) *
			    force *
			    fe_values.JxW (q_point));
	    
	    
	  }


	

	
	
	if (cell->material_id() == 0)
	  {	   
	    for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face)
	      if (cell->neighbor_index(face) != -1)	       
		if (cell->material_id() !=  cell->neighbor(face)->material_id() &&
		    cell->face(face)->boundary_indicator()!=0
		    // Here we assume that the boundary indicator is '0'
		    )
		  {
		    
		    fe_face_values.reinit (cell, face);
		    fe_face_values_solid.reinit (cell_displacement_phase_field, face);
		    fe_face_values_level_set.reinit (cell_level_set, face);
		    
		    
		    fe_face_values_solid.get_function_values (solution, old_solution_solid_pff_face_values);
		    fe_face_values_level_set.get_function_gradients (solution_level_set, old_solution_level_set_face_grads);
		    
		    
		    
		    // Get level set values and displacement 
		    // values here and compute the local width 
		    double width_local = 3.0;
		    
		    for (unsigned int q=0; q<n_face_q_points; ++q)
		      {
			
			const Tensor<1,dim> u = Tensors
			  ::get_u<dim> (q, old_solution_solid_pff_face_values);
			
			double aperture = 0.0;
			
			double level_set_norm = 0.0;
			for (unsigned int k=0;k<dim; k++)
			  {
			    level_set_norm += old_solution_level_set_face_grads[q][k] * old_solution_level_set_face_grads[q][k];
			  }
			
			level_set_norm = std::sqrt(level_set_norm);
			
			
			aperture = -2. * u * (old_solution_level_set_face_grads[q]/(level_set_norm + 1.0e-12));
			
			// TODO: check again: but there should be no negative width!!
			if (aperture < 0)
			  aperture = -1.*aperture;
			
			
			for (unsigned int i=0; i<dofs_per_cell; ++i)
			  {

			    for (unsigned int j=0; j<dofs_per_cell; ++j)
			      {
				// The penalty must be large enough
				// otherwise the level-set iso function is not 
				// correctly imposed
				cell_matrix(i,j) += penalty_parameter * fe_values.shape_value (i, q) * fe_values.shape_value (j, q) * fe_face_values.JxW(q);
			      }
			    // The penalty must be large enough
			   // otherwise the level-set iso function is not 
			   // correctly imposed
			    cell_rhs(i) += aperture * penalty_parameter * fe_values.shape_value (i, q) * fe_face_values.JxW(q);
			    
			  }
			
		      }
		    
		  }
	    
	  }
	
	

	
	
	
	cell->get_dof_indices (local_dof_indices);
	
	constraints_width.distribute_local_to_global (cell_matrix,
						      cell_rhs,
						      local_dof_indices,
						      system_matrix_width,
						      system_rhs_width); 
	
      }
  
  system_matrix_width.compress(VectorOperation::add);
  system_rhs_width.compress(VectorOperation::add);
    
  

}





template <int dim>
void 
FracturePhaseFieldProblem<dim>::solve_level_set ()
{ 

  SolverControl           solver_control (1000, 1e-12);
  SolverCG<LA::MPI::Vector>              solver (solver_control);

  TrilinosWrappers::PreconditionSSOR preconditioner;
  preconditioner.initialize(system_matrix_level_set, 1.2);
 
  LA::MPI::Vector localized_solution(system_rhs_level_set);
  localized_solution.reinit(solution_level_set,false,true);
  
  solver.solve (system_matrix_level_set, localized_solution, system_rhs_level_set,
                preconditioner
		//PreconditionIdentity()
		);

  constraints_level_set.distribute(localized_solution);
  solution_level_set.reinit(localized_solution, false, true);


}

  
template <int dim>
void
FracturePhaseFieldProblem<dim>::solve_width ()
    {

  SolverControl           solver_control (1000, 1e-12);
  SolverCG<LA::MPI::Vector>              solver (solver_control);

  TrilinosWrappers::PreconditionSSOR preconditioner;
  preconditioner.initialize(system_matrix_width, 1.2);

  LA::MPI::Vector localized_solution(system_rhs_width);
  localized_solution.reinit(solution_width,false,true);

  solver.solve (system_matrix_width, localized_solution, system_rhs_width,
                preconditioner
		//PreconditionIdentity()
		);

  constraints_width.distribute(localized_solution);
  solution_width.reinit(localized_solution, false, true);


    }
  
  



template <int dim>
void
FracturePhaseFieldProblem<dim>::set_newton_bc_pressure ()
    {
      
  pcout << "    :  Set Newton BC PRESSURE " << std::endl;  

  VectorTools::interpolate_boundary_values(dof_handler_pressure,
					   1,
					   ZeroFunction<dim>(1),
					   constraints_pressure);
  
  VectorTools::interpolate_boundary_values(dof_handler_pressure,
					   0,
					   ZeroFunction<dim>(1),
					   constraints_pressure);
      
  if (test_case == TestCase::hole){
    
  
    //VectorTools::interpolate_boundary_values(dof_handler_pressure, 
    //					     0,
    //					     ZeroFunction<dim>(1), 
    //					     constraints_pressure);
      /*
    VectorTools::interpolate_boundary_values(dof_handler_pressure, 
					     1,
					     ZeroFunction<dim>(1), 
    					     constraints_pressure);
    VectorTools::interpolate_boundary_values(dof_handler_pressure, 
    					     2,
    					     ZeroFunction<dim>(1), 
    					     constraints_pressure);
      */
	}
      
  // To be uncommented when prescribing Dirichlet conditions
  // for the pressure
  //VectorTools::interpolate_boundary_values(dof_handler_pressure, 
  //					     0,
  //					     ZeroFunction<dim>(1), 
  //					     constraints_pressure);

}  
        



template <int dim>
void 
FracturePhaseFieldProblem<dim>::read_solutions(){

  //mpi_mgr.write("LOADING SOLUTIONS ");
  
  TrilinosWrappers::MPI::BlockVector    distributed_p_n (system_rhs_pressure);  
    
  /*
  cout<<"Loaded Pressure"<<endl;
  std::vector<TrilinosWrappers::MPI::BlockVector *> Load_Pressure (1);
  Load_Pressure[0] = & (distributed_p_n);

  parallel::distributed::SolutionTransfer<dim, TrilinosWrappers::MPI::BlockVector>
    l_pressure_trans (dof_handler_pressure);
 
  l_pressure_trans.deserialize (Load_Pressure);
  
  solution_pressure = distributed_p_n;
  old_timestep_solution_pressure = solution_pressure;
  */

  //////////////////////////////////////////////////////////////
  /*
  Vector<double> tmp_permeability_per_cell;
  tmp_permeability_per_cell.reinit(triangulation.n_active_cells());



  std::vector< Vector<double> *> Load_permeability_per_cell (1);
  Load_permeability_per_cell[0] = & (tmp_permeability_per_cell);

  parallel::distributed::SolutionTransfer<dim, Vector<double>>
    l_permeability_trans (dof_handler_pressure);
  
  l_permeability_trans.deserialize (Load_permeability_per_cell);
  
  permeability_per_cell = tmp_permeability_per_cell;
  */

  cout<<"2"<<endl;
  std::ifstream infile;
  
  int num = 0;
  infile.open("Perm-000027.txt");

  if(infile.fail())
    {
      cout<<"ERRor Reading Rmesh"<< endl;
      exit(0);
    }
  
// mjm
typename DoFHandler<dim>::active_cell_iterator
      cell = dof_handler.begin_active(),
      endc = dof_handler.end();
  int counter = 0;
  int cell_index = 0;

    for (; cell!=endc; ++cell, ++cell_index){
      if (counter < permeability_per_cell.size()){
          double a,b;
          infile >> a >> b;// >> c >> d >> e >> f >> g;
          permeability_per_cell(cell_index) = b;
          ++counter;}
       }

//  while(num < permeability_per_cell.size())
//    {
//      double a,b;
//      infile >> a >> b;// >> c >> d >> e >> f >> g;
//      //cout<< num << ", " <<  b<<endl;
//      permeability_per_cell(num) = b;
//      ++num;
//      //cout<<permeability_per_cell.size() << "," << num<<endl;
//   }
//  
// mjm


  infile.close();

}


template <int dim>
void 
FracturePhaseFieldProblem<dim>::write_solutions(){
  cout<<"writing on files"<<endl;
  
  std::vector<const TrilinosWrappers::MPI::BlockVector *> Save_Pressure (1);
  Save_Pressure[0] = &solution_pressure; 

  parallel::distributed::SolutionTransfer<dim,TrilinosWrappers::MPI::BlockVector>
    pressure_trans (dof_handler_pressure);

  pressure_trans.prepare_serialization (Save_Pressure);
  
  std::ostringstream filename;
  filename << "sol_files/soln_" ;
  
  triangulation.save (filename.str().c_str());

  ///////////////////////////////////////////////////////////////////////////
  //  std::vector<Vector<double> *> Save_permeability_per_cell (1);
  //  Save_permeability_per_cell[0] = &permeability_per_cell;
  //  parallel::distributed::SolutionTransfer<1, Vector<double> >
  //  permeability_trans (dof_handler_pressure);
  //  permeability_trans.prepare_serialization (Save_permeability_per_cell);

  const std::string filename_0 =
    ( "Perm-"
      + Utilities::int_to_string(timestep_number, 6)
      + ".txt" );
  std::ofstream outputFile_Perm(filename_0.c_str());
  
  unsigned int cell_index = 0;
  
  
  typename DoFHandler<dim>::active_cell_iterator
    cell = dof_handler.begin_active(),
    endc = dof_handler.end();
  for (; cell!=endc; ++cell, ++cell_index)     
    {
      outputFile_Perm  <<  cell <<"  " 
		       << permeability_per_cell(cell_index)
		       << endl;
    }
  

}


      
template <int dim>
void
FracturePhaseFieldProblem<dim>::set_initial_bc_pressure (const double time)
{	     		
  
  pcout << "    :  Set Initial BC Values PRESSURE " << time << std::endl;
  
  std::map<unsigned int,double> boundary_values;  
  
  VectorTools::interpolate_boundary_values(dof_handler_pressure,
					   1,
					   ZeroFunction<dim>(1),
					   boundary_values);
  
  VectorTools::interpolate_boundary_values(dof_handler_pressure,
					   0,
					   ConstantFunction<dim>(1.0),
					   boundary_values);

	  
  if (test_case == TestCase::hole){
    /*
      VectorTools::interpolate_boundary_values(dof_handler_pressure,
      1,
      ZeroFunction<dim>(1),
      boundary_values);

      VectorTools::interpolate_boundary_values(dof_handler_pressure,
      2,
      //                                           ConstantFunction<dim>(2),
      //					     Initial_Pressure_Values,
      ZeroFunction<dim>(1),
      boundary_values);
    */
	  
  }

  // To be uncommented when prescribing Dirichlet conditions
  // for the pressure
  

    std::pair<unsigned int, unsigned int> range;
     
      
    LA::MPI::BlockVector distributed_solution(partition_pressure);
    distributed_solution = solution_pressure;

    range= distributed_solution.block(0).local_range();
    for (typename std::map<unsigned int, double>::const_iterator i =
    boundary_values.begin(); i != boundary_values.end(); ++i)
    if (i->first >= range.first && i->first < range.second)
    distributed_solution(i->first) = i->second;

    distributed_solution.compress(VectorOperation::insert);  
    solution_pressure =  distributed_solution;
  
     
    }






// In this function, we assemble the Jacobian matrix
// for the Newton iteration. 
template <int dim>
void
FracturePhaseFieldProblem<dim>::assemble_system_matrix_pressure ()
{
  
  //pcout << " *********Assemble PRESSURE Matrix." << endl;
  const Point<dim> evaluation_point_rhs_injection   (point_0);
  const Point<dim> evaluation_point_rhs_injection_1 (point_1);
  const Point<dim> evaluation_point_rhs_injection_2 (point_2);
  const Point<dim> evaluation_point_rhs_injection_3 (point_3);
  const Point<dim> evaluation_point_rhs_injection_4 (point_4);

  system_matrix_pressure=0;
  
  
  QGauss<dim>   quadrature_formula(degree+2);


  FEValues<dim> fe_values_level_set (fe_level_set, quadrature_formula,
				    update_values    |
				    update_quadrature_points  |
				    update_JxW_values |
				    update_gradients);
 
  FEValues<dim> fe_values_pressure (fe_pressure, quadrature_formula,
				    update_values    |
				    update_quadrature_points  |
				    update_JxW_values |
				    update_gradients);
  
  FEValues<dim> fe_values(fe, 
			  quadrature_formula,
			  update_values | update_quadrature_points | update_JxW_values
			  | update_gradients);


  FEValues<dim> fe_values_width (fe_width, quadrature_formula,
				 update_values    |
				 update_quadrature_points  |
				 update_JxW_values);


  
  const unsigned int   dofs_per_cell   = fe_pressure.dofs_per_cell;
  
  const unsigned int   n_q_points      = quadrature_formula.size();
  
  FullMatrix<double>   local_matrix (dofs_per_cell, dofs_per_cell);
  std::vector<unsigned int> local_dof_indices (dofs_per_cell); 


  const FEValuesExtractors::Scalar pressure (0);

  
  // We declare Vectors and Tensors for 
  // the solutions at the previous Newton iteration:
  std::vector<Vector<double> > old_solution_values_pressure (n_q_points, 
							     Vector<double>(1));

  std::vector<std::vector<Tensor<1,dim> > > old_solution_grads_pressure (n_q_points, 
									 std::vector<Tensor<1,dim> > (1));
  
  
  std::vector<Vector<double> > old_timestep_solution_values_pressure (n_q_points, 
								      Vector<double>(1));
  
  
  std::vector<std::vector<Tensor<1,dim> > > old_timestep_solution_grads_pressure (n_q_points, 
										  std::vector<Tensor<1,dim> > (1));
  

  //Maybe needed to merge like this ? NEWP
  std::vector<Vector<double> > old_solution_values(n_q_points,
						   Vector<double>(dim+1));
  
  std::vector<std::vector<Tensor<1,dim> > > old_solution_grads (n_q_points,
								std::vector<Tensor<1,dim> > (dim+1));
  
  std::vector<Vector<double> > old_timestep_solution_values(n_q_points,
							    Vector<double>(dim+1));
  
  std::vector<std::vector<Tensor<1,dim> > > old_timestep_solution_grads (n_q_points,
									 std::vector<Tensor<1,dim> > (dim+1));
  
  std::vector<Vector<double> > old_old_timestep_solution_values(n_q_points,
								Vector<double>(dim+1));
  
  
  
  std::vector<Tensor<1,dim> > level_set_solution_grads(n_q_points);
  std::vector<double> width_solution_values(n_q_points);
  
  
  // Declaring test functions:
  std::vector<double>         phi_i_p(dofs_per_cell); 
  std::vector<Tensor<1,dim> > phi_i_grads_p (dofs_per_cell);

  // This is the identity matrix in two dimensions:
  const Tensor<2,dim> Identity = Tensors::get_Identity<dim> ();
 				     				   
  typename DoFHandler<dim>::active_cell_iterator
    cell = dof_handler_pressure.begin_active(),
    endc = dof_handler_pressure.end();

  typename DoFHandler<dim>::active_cell_iterator
    cell_displacement_phase_field = dof_handler.begin_active();

  typename DoFHandler<dim>::active_cell_iterator
    cell_level_set = dof_handler_level_set.begin_active();
 
  typename DoFHandler<dim>::active_cell_iterator
    cell_width = dof_handler_width.begin_active();
  
  
  int cell_index = 0;

  for (; cell!=endc; ++cell, ++cell_displacement_phase_field, ++cell_level_set, ++cell_width, ++cell_index)
    if (cell->is_locally_owned()) {
      fe_values_pressure.reinit (cell);
      fe_values.reinit(cell_displacement_phase_field);
      fe_values_level_set.reinit(cell_level_set);
      fe_values_width.reinit(cell_width);

      local_matrix = 0;

      // Old Newton iteration values pressure 
      fe_values_pressure.get_function_values (solution_pressure, old_solution_values_pressure);
      fe_values_pressure.get_function_gradients (solution_pressure, old_solution_grads_pressure);
      
      // Old_timestep_solution values pressure
      fe_values_pressure.get_function_values (old_timestep_solution_pressure, old_timestep_solution_values_pressure);
      fe_values_pressure.get_function_gradients (old_timestep_solution_pressure, old_timestep_solution_grads_pressure);
      
      fe_values.get_function_values (solution, old_solution_values);
      fe_values.get_function_gradients (solution, old_solution_grads);  

      fe_values.get_function_values (old_solution, old_timestep_solution_values);
      fe_values.get_function_gradients  (old_solution, old_timestep_solution_grads);
      
      fe_values_level_set.get_function_gradients (solution_level_set, level_set_solution_grads);

      fe_values_width.get_function_values (solution_width, width_solution_values);
      
      dirac_force_injection = 0.;
      for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_cell; ++vertex)
	{
	  if (test_case == TestCase::hole){
	    if (cell->vertex(vertex).distance(evaluation_point_rhs_injection)
		< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_1)
		     < cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    
	    
	  }
	  else{
	    if (cell->vertex(vertex).distance(evaluation_point_rhs_injection) < cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_1)< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else  if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_2) < cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }

	  }
	}
      
	      
      for (unsigned int q=0; q<n_q_points; ++q)
	{	      
	  
	      
	  for (unsigned int k=0; k<dofs_per_cell; ++k)
	    {
	      phi_i_p[k]       = fe_values_pressure[pressure].value (k, q);
	      phi_i_grads_p[k] = fe_values_pressure[pressure].gradient (k, q);
	    }
	  
	  const Tensor<1,dim> u = Tensors
	    ::get_u<dim> (q, old_solution_values);
	  
	  const Tensor<2,dim> grad_u = Tensors 
	    ::get_grad_u<dim> (q, old_solution_grads);
	  
	  const Tensor<1,dim> old_timestep_u = Tensors
	    ::get_u<dim> (q, old_timestep_solution_values);

	  const double divergence_u = Tensors 
	    ::get_divergence_u<dim> (grad_u);

	  
	  const Tensor<2,dim> E = 0.5 * (grad_u + transpose(grad_u));
	  const double tr_E = grad_u[0][0] + grad_u[1][1];
	  
	  Tensor<2,dim> stress_term;
	  stress_term.clear();
	  stress_term = lame_coefficient_lambda * tr_E * Identity +  2 * lame_coefficient_mu * E;
	  
	  double pf              = old_solution_values[q](dim);



	  // Initialize fracture and reservoir permeablities
	  double K_R = 0.0; 
	  double K_F = 0.0; 

	  double aperture = width_solution_values[q];
	  
	  // We need w^2 for the fracture permeability K_F
	  aperture *= aperture;

	  // Old point-wise computation
	  // Let's keep for the moment for further testing 
	  // if necessary
	  aperture = 0.0;
//
          for(int b=0; b<dim ; ++b)
	    {	      
	      aperture += u[b]*u[b] ;
//	      //pcout << aperture << "   " << u[b]*u[b] <<  std::endl;
	    }
	 
	  K_F = (K_biot)+(aperture)/12.0; 
	  K_R = K_biot;

	  if(K_F < K_R){
	    cout<<"Aborting! Fracture permeability has to be larger than reservoir permeability!" << endl;
	    exit(0);
	  }
	  

	  //DEBUG - for 3D testing	  
	  double K_F_a = 1e-10;
	  //if(dim == 3)
	    //K_F = K_F_a;



	  // Some parameters for the well model 'Peaceman' Model  
	  double const_Q_R = 0.0;
	  double const_Q_F = 0.0;
	  if (bPeaceman_well_model)
	    {
	      const double pi=numbers::PI;
	      
	      const_Q_R = (2. * pi * density_biot * sqrt(K_R*K_R) *h_3 ) / (viscosity_biot * R_log_value);
	      const_Q_F = (2. * pi * density_biot * sqrt(K_F_a*K_F_a) *h_3 ) / (viscosity_biot * R_log_value);
	  
	    }
	  else 
	    {
	      // Using Dirac injection rather than well-model
	      const_Q_R = 0.0;
	      const_Q_F = 0.0;
	    }
	  
	  double chi_fracture_values = 0.;
	  double chi_reservoir_values = 0.;
	  
	  double x1 = pressure_diff_x_1;
	  double x2 = pressure_diff_x_2;

	  // Interpolation of the transition zone
	  if(pf <= x1 ){
	    chi_fracture_values  = 1.;
	    chi_reservoir_values = 0.;
	  }
	  else if(pf >= x2){
	    chi_fracture_values  = 0.;
	    chi_reservoir_values = 1.;
	  }
	  else{
	    chi_fracture_values  = -( pf - x2 ) / (x2 - x1);
	    chi_reservoir_values = ( pf  -  x1  )/( x2 - x1  );
	  }


	  // K_F, K_R 
	  K_F = permeability_per_cell(cell_index);
	  K_R = permeability_per_cell(cell_index);

	  chi_fracture_values  = 1.;
	  chi_reservoir_values = 0.;


	  

	  double gamma_fixed_stress_local = gamma_fixed_stress * pf * pf;
	  double beta_fixed_stress_local = (alpha_biot * alpha_biot * (1.0+poisson_ratio_nu) * (1.0 - 2.0 * poisson_ratio_nu))/ (2. * E_modulus *  poisson_ratio_nu);
	  //	  if (timestep_number > 1)
	  // beta_fixed_stress_local *= pf * pf;
	  
	  for (unsigned int i=0; i<dofs_per_cell; ++i)
	    {
	      for (unsigned int j=0; j<dofs_per_cell; ++j)
		{
		  // Pressure matrices
		  // Reservoir
		  local_matrix(j,i) += ((c_biot + beta_fixed_stress_local) * phi_i_p[i] * phi_i_p[j] + 
					timestep * theta * (K_R/viscosity_biot)
					* phi_i_grads_p[i] * phi_i_grads_p[j]
					// Well Model
					+ timestep * theta * dirac_force_injection * const_Q_R
					* phi_i_p[i] * phi_i_p[j]
					) * chi_reservoir_values  *   fe_values_pressure.JxW(q);

		  
		  // Fracture
		  local_matrix(j,i) += ( (c_F + gamma_fixed_stress_local)  * phi_i_p[i] * phi_i_p[j] 
					+ timestep * theta * (K_F/viscosity_F)
					* phi_i_grads_p[i] * phi_i_grads_p[j]	
					// Well Model
					 + timestep * theta * dirac_force_injection * const_Q_F  
					* phi_i_p[i] * phi_i_p[j]
					) * chi_fracture_values  *   fe_values_pressure.JxW(q);
		  
		  // end j dofs
		}  
	      // end i dofs		     
	    }   
	  // end n_q_points 
	}    
      
      
      cell->get_dof_indices (local_dof_indices);
      constraints_pressure.distribute_local_to_global (local_matrix, local_dof_indices,
						       system_matrix_pressure);
      
    }
      // end cell
    
  
  system_matrix_pressure.compress(VectorOperation::add);


}



template <int dim>
void
FracturePhaseFieldProblem<dim>::assemble_system_rhs_pressure ()
{


  const Point<dim> evaluation_point_rhs_injection   (point_0);
  const Point<dim> evaluation_point_rhs_injection_1 (point_1);
  const Point<dim> evaluation_point_rhs_injection_2 (point_2);
  const Point<dim> evaluation_point_rhs_injection_3 (point_3);
  const Point<dim> evaluation_point_rhs_injection_4 (point_4);
  system_rhs_pressure=0;

  
  
  QGauss<dim>   quadrature_formula(degree+2);

  FEValues<dim> fe_values_level_set (fe_level_set, quadrature_formula,
				    update_values    |
				    update_quadrature_points  |
				    update_JxW_values |
				    update_gradients);

  FEValues<dim> fe_values_pressure (fe_pressure, quadrature_formula,
				    update_values    |
				    update_quadrature_points  |
				    update_JxW_values |
				    update_gradients);
  
  FEValues<dim> fe_values(fe, quadrature_formula,
			  update_values | update_quadrature_points | update_JxW_values
			  | update_gradients);
  
  
 FEValues<dim> fe_values_width (fe_width, quadrature_formula,
				 update_values    |
				 update_quadrature_points  |
				 update_JxW_values);
  

  const unsigned int   dofs_per_cell   = fe_pressure.dofs_per_cell;
  
  const unsigned int   n_q_points      = quadrature_formula.size();

  Vector<double>       local_rhs (dofs_per_cell);

  std::vector<unsigned int> local_dof_indices (dofs_per_cell); 
		

  // Now, we are going to use the 
  // FEValuesExtractors to determine
  // the four principle variables
  const FEValuesExtractors::Scalar pressure (0); 


  // We declare Vectors and Tensors for 
  // the solutions at the previous Newton iteration:
  std::vector<Vector<double> > old_solution_values_pressure (n_q_points, 
							     Vector<double>(1));
  
  std::vector<std::vector<Tensor<1,dim> > > old_solution_grads_pressure (n_q_points, 
									 std::vector<Tensor<1,dim> > (1));
  
  
  std::vector<Vector<double> > old_timestep_solution_values_pressure (n_q_points, 
								      Vector<double>(1));
  
  
  std::vector<std::vector<Tensor<1,dim> > > old_timestep_solution_grads_pressure (n_q_points, 
										  std::vector<Tensor<1,dim> > (1));
  
  std::vector<Vector<double> > old_fixed_stress_solution_values_pressure (n_q_points, 
									  Vector<double>(1));
  
  
  std::vector<std::vector<Tensor<1,dim> > > old_fixed_stress_solution_grads_pressure (n_q_points, 
										      std::vector<Tensor<1,dim> > (1));


  std::vector<Vector<double> > old_solution_values(n_q_points,
						   Vector<double>(dim+1));
  
  std::vector<std::vector<Tensor<1,dim> > > old_solution_grads (n_q_points,
								std::vector<Tensor<1,dim> > (dim+1));
  
  std::vector<Vector<double> > old_timestep_solution_values(n_q_points,
							    Vector<double>(dim+1));
  
  std::vector<std::vector<Tensor<1,dim> > > old_timestep_solution_grads (n_q_points,
									 std::vector<Tensor<1,dim> > (dim+1));
  
  std::vector<Vector<double> > old_fixed_stress_solution_values(n_q_points,
								Vector<double>(dim+1));

  std::vector<std::vector<Tensor<1,dim> > > old_fixed_stress_solution_grads (n_q_points,
								std::vector<Tensor<1,dim> > (dim+1));



  std::vector<Tensor<1,dim> > level_set_solution_grads(n_q_points);
  std::vector<double> width_solution_values(n_q_points);


  //Function_Pressure<dim> function_pressure;     
  std::vector<double> function_pressure_values(n_q_points);
  
  typename DoFHandler<dim>::active_cell_iterator
    cell = dof_handler_pressure.begin_active(),
    endc = dof_handler_pressure.end();

  typename DoFHandler<dim>::active_cell_iterator
    cell_displacement_phase_field = dof_handler.begin_active();
 
  typename DoFHandler<dim>::active_cell_iterator
    cell_level_set = dof_handler_level_set.begin_active();
 
  typename DoFHandler<dim>::active_cell_iterator
    cell_width = dof_handler_width.begin_active();
 
  
  unsigned int cell_index = 0;
  for (; cell!=endc; ++cell, ++cell_displacement_phase_field, ++cell_index, ++cell_level_set, ++cell_width)
    if (cell->is_locally_owned())
    { 
       
      fe_values_pressure.reinit (cell);
      fe_values.reinit (cell_displacement_phase_field);
      fe_values_level_set.reinit(cell_level_set);
      fe_values_width.reinit(cell_width);

      local_rhs = 0;

      // Old Newton iteration values pressure
      fe_values_pressure.get_function_values (solution_pressure, old_solution_values_pressure);
      fe_values_pressure.get_function_gradients (solution_pressure, old_solution_grads_pressure);
      
      // Old_timestep_solution values pressure
      fe_values_pressure.get_function_values (old_timestep_solution_pressure, old_timestep_solution_values_pressure);
      fe_values_pressure.get_function_gradients (old_timestep_solution_pressure, old_timestep_solution_grads_pressure);
      
      // Old_fixed_stress_solution values pressure
      fe_values_pressure.get_function_values (old_fixed_stress_solution_pressure, old_fixed_stress_solution_values_pressure);
      fe_values_pressure.get_function_gradients  (old_fixed_stress_solution_pressure, old_fixed_stress_solution_grads_pressure); 

      // Old Newton iteration values displacement-phase-field
      fe_values.get_function_values (solution, old_solution_values);
      fe_values.get_function_gradients (solution, old_solution_grads);  
      

      // Old_timestep_solution values displacement-phase-field
      fe_values.get_function_values (old_solution, old_timestep_solution_values);
      fe_values.get_function_gradients  (old_solution, old_timestep_solution_grads);

      // Old_fixed_stress_solution values solid
      fe_values.get_function_values (old_fixed_stress_solution, old_fixed_stress_solution_values);
      fe_values.get_function_gradients  (old_fixed_stress_solution, old_fixed_stress_solution_grads);
    
      fe_values_level_set.get_function_gradients  (solution_level_set, level_set_solution_grads);

      fe_values_width.get_function_values (solution_width, width_solution_values);

    
      //function_pressure.value_list(fe_values_pressure.get_quadrature_points(), function_pressure_values);    
      
      // Well Model, 
      // Before we go to the Quadrature Rules.
      // Search the Vertex(DOF) where we have the injection point.
      
      // DEBUG FIXED STRESS 
      // Two options to prescribe the pressure
      // Option 1 (linear increase)
      double epsilon = 0.03; // Ending Time step number / or / Ending Time 
      //      double range = (timestep_number/epsilon);      
      double range = (time/epsilon);
      double power = 3.;
      double linear_term = std::pow(range,power);
      
      if( time < epsilon )
      	max_pressure_well_bore = 2.* ( linear_term / (1. + linear_term) ) * pressure_wellbore; 
      else 
      	max_pressure_well_bore = pressure_wellbore;


      // Option 2:
      // Not using linearly increasing pressure for the moment.
      if(dim == 3)
      max_pressure_well_bore = pressure_wellbore;
      
      dirac_force_injection = 0.0;
      for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_cell; ++vertex)
	{
	  if (test_case == TestCase::hole){
	    if (cell->vertex(vertex).distance(evaluation_point_rhs_injection)
		< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_1)
		     < cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    
	    
	  }
	  else{
	    
	    if (cell->vertex(vertex).distance(evaluation_point_rhs_injection)
		< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_1)
		< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }
	    else if (cell->vertex(vertex).distance(evaluation_point_rhs_injection_2)
		< cell->diameter()*0.1)
	      {
		dirac_force_injection = 1.;
	      }

	  }
	}


      for (unsigned int q=0; q<n_q_points; ++q)
	{	  
/*	  
	  if(dim==2)
	    dirac_force_injection = 1.0 * std::exp(-10000.0 * (fe_values_pressure.quadrature_point(q)[0] - evaluation_point_rhs_injection[0]) * 
						 (fe_values_pressure.quadrature_point(q)[0] - evaluation_point_rhs_injection[0]) -
						 10000.0 * (fe_values_pressure.quadrature_point(q)[1] - evaluation_point_rhs_injection[1]) * 
						 (fe_values_pressure.quadrature_point(q)[1] - evaluation_point_rhs_injection[1]));
*/
	  if(dim==3)
	    dirac_force_injection = 1.0 * std::exp(-10000.0 * 
						   (fe_values_pressure.quadrature_point(q)[0] - evaluation_point_rhs_injection[0]) 
						   * (fe_values_pressure.quadrature_point(q)[0] - evaluation_point_rhs_injection[0]) 						   
						   -10000.0 * 
						   (fe_values_pressure.quadrature_point(q)[1] - evaluation_point_rhs_injection[1]) 
						   * (fe_values_pressure.quadrature_point(q)[1] - evaluation_point_rhs_injection[1])
						   -10000.0 * 
						   (fe_values_pressure.quadrature_point(q)[2] - evaluation_point_rhs_injection[2]) 
						   * (fe_values_pressure.quadrature_point(q)[2] - evaluation_point_rhs_injection[2]));


	  
	  const Tensor<1,dim> u = Tensors
	    ::get_u<dim> (q, old_solution_values);

	  double pf              = old_solution_values[q](dim);

	  const double p_value            =  old_solution_values_pressure[q](0);
	  const double old_timestep_p     = old_timestep_solution_values_pressure[q](0); 
	  const double old_fixed_stress_p = old_fixed_stress_solution_values_pressure[q](0); 
	  
	  
	  const Tensor<2,dim> grad_u = Tensors 
	    ::get_grad_u<dim> (q, old_solution_grads);
	  
	  
	  // For comments and explanation see the 
	  // the assemble_system_matrix_pressure function
	  double aperture = width_solution_values[q];
	  solution_stress_per_cell(cell_index) += aperture / n_q_points;
	  
	  // We need w^2 for the fracture permeability K_F
	  aperture *= aperture;
 

	  
	  double K_R = 0.0; 
	  double K_F = 0.0; 
  
	  // Old point-wise aperture computation
	  aperture = 0.0;
	  for(int b=0; b<dim ; ++b)
	    {
	      aperture += u[b]*u[b]; 
	      solution_fluid_or_structure_mat(cell_index) += u[b]*u[b] / n_q_points;
	    }

	  //K_F or K_R will be judged by the phase-field variable
	  K_F = (K_biot)+(aperture) / 12.0;
	  K_R = K_biot;
	  

	  // Is used in the well model constant
	  double K_F_a = 1e-10;
	  //DEBUG
	  //if(dim == 3)
	  //K_F = K_F_a;


	  
	  double chi_fracture_values = 0.;
	  double chi_reservoir_values = 0.;

	  double x1 = pressure_diff_x_1;
	  double x2 = pressure_diff_x_2;
	  

	  if(pf <= x1 ){
	    chi_fracture_values  = 1.;
	    chi_reservoir_values = 0.;
	  }
	  else if(pf >= x2){
	    chi_fracture_values  = 0.;
	    chi_reservoir_values = 1.;
	  }
	  else{
	    chi_fracture_values  = -( pf - x2 ) / (x2 - x1);
	    chi_reservoir_values = ( pf  -  x1  )/( x2 - x1  );
	  }

          
	  double gamma_fixed_stress_local = gamma_fixed_stress * pf * pf;
	  double beta_fixed_stress_local = (alpha_biot * alpha_biot * (1.0+poisson_ratio_nu) * (1.0 - 2.0 * poisson_ratio_nu))/ (2. * E_modulus *  poisson_ratio_nu);
	  //	  if (timestep_number > 1)
	  //	    beta_fixed_stress_local *= pf * pf;

	  const Tensor<1,dim> grad_pf = Tensors
	    ::get_grad_pf<dim> (q, old_solution_grads);
	  
	  const Tensor<1,dim> grad_p = Tensors
	    ::get_grad_p<dim> (q, old_solution_grads_pressure);
	  
	


	  const Tensor<2,dim> Identity = Tensors
	    ::get_Identity<dim> ();
	  
	  
	  const Tensor<2,dim> E = 0.5 * (grad_u + transpose(grad_u));
	  const double tr_E = grad_u[0][0] + grad_u[1][1];
	  
	  // Previous time step values
	  const Tensor<1,dim> old_timestep_u = Tensors
	    ::get_u<dim> (q, old_timestep_solution_values);

	  const Tensor<2,dim> old_timestep_grad_u = Tensors 
	    ::get_grad_u<dim> (q, old_timestep_solution_grads);
	  
	  const Tensor<2,dim> old_timestep_E = 0.5 * (old_timestep_grad_u + transpose(old_timestep_grad_u));
	  const double old_timestep_tr_E = old_timestep_grad_u[0][0] + old_timestep_grad_u[1][1];
	  
	  Tensor<2,dim> stress_term;
	  stress_term.clear();
	  stress_term = lame_coefficient_lambda * tr_E * Identity +  2 * lame_coefficient_mu * E;
	  
	  Tensor<2,dim> old_timestep_stress_term;
	  old_timestep_stress_term.clear();
	  old_timestep_stress_term = lame_coefficient_lambda * old_timestep_tr_E * Identity +  2 * lame_coefficient_mu * old_timestep_E;
	  
	  

	  const double divergence_u = Tensors 
	    ::get_divergence_u<dim> (grad_u);
		
	  const double old_timestep_divergence_u = Tensors 
	    ::get_divergence_u<dim> (old_timestep_grad_u);	  

	  const Tensor<2,dim> old_fixed_stress_grad_u = Tensors 
	    ::get_grad_u<dim> (q,old_fixed_stress_solution_grads);

	  const double old_fixed_stress_divergence_u = Tensors 
	    ::get_divergence_u<dim> (old_fixed_stress_grad_u);



	  double volumetric_strain_rate = 0.0;
	 
	  volumetric_strain_rate = (old_fixed_stress_divergence_u - old_timestep_divergence_u);
	  
	  Tensor<1,dim> g_biot;
	  g_biot.clear();
	  g_biot[0] = gravity_x;
	  g_biot[1] = gravity_y;

	  // For WELL-Model 'Peaceman' Model 
	  double const_Q_R = 0.0;
	  double const_Q_F = 0.0;
	  double q_biot_R = 0.0;
	  double q_biot_F = 0.0;
	  if (bPeaceman_well_model)
	    {
	  const double pi=numbers::PI;

	      const_Q_R = (2. * pi * density_biot * sqrt(K_R*K_R) *h_3 ) / (viscosity_biot * R_log_value);
	      const_Q_F = (2. * pi * density_biot * sqrt(K_F_a*K_F_a) *h_3 ) / (viscosity_biot * R_log_value);
	  
	      q_biot_R = volume_source + dirac_force_injection * const_Q_R * max_pressure_well_bore; 
	      q_biot_F = volume_source + dirac_force_injection * const_Q_F * max_pressure_well_bore; 

	    }
	  else 
	    {
	      // Use Dirac injection
	      const_Q_R = 0.0;
	      const_Q_F = 0.0;

	      q_biot_R = volume_source + dirac_force_injection * max_pressure_well_bore; 
	      q_biot_F = volume_source + dirac_force_injection * max_pressure_well_bore; 


	    }

	  //permeability_per_cell(cell_index) = K_F * chi_fracture_values  + K_R * chi_reservoir_values ;

	  ///////////////////////////////////////////////////////////////
	  ///////////////////////////////////////////////////////////////
	  K_F = 	  permeability_per_cell(cell_index);
	  K_R = 	  permeability_per_cell(cell_index);

	  chi_fracture_values  = 1.;
	  chi_reservoir_values = 0.;

	  for (unsigned int i=0; i<dofs_per_cell; ++i)
	    {
	      const double phi_i_p = fe_values_pressure[pressure].value (i, q);
	      const Tensor<1,dim> phi_i_grads_p = fe_values_pressure[pressure].gradient (i, q);
	      
	      // Reservoir Pressure rhs
	      local_rhs(i) -= ((c_biot + beta_fixed_stress_local) * (p_value - old_timestep_p) * phi_i_p 
			       +  timestep * theta * (K_R/viscosity_biot)* grad_p * phi_i_grads_p
			       - timestep * theta * density_biot * (K_R/viscosity_biot) * g_biot * phi_i_grads_p
			       + timestep * theta * dirac_force_injection * const_Q_R
			       * p_value * phi_i_p
			       - timestep *  theta * q_biot_R * phi_i_p
			       // Right hand side values
			       + alpha_biot* volumetric_strain_rate * phi_i_p 
			       - beta_fixed_stress * (old_fixed_stress_p - old_timestep_p) * phi_i_p
			       ) * chi_reservoir_values  *  fe_values_pressure.JxW(q);
	      

	      // Fracture Pressure rhs
	      local_rhs(i) -= ( (c_F + gamma_fixed_stress_local) * (p_value - old_timestep_p) * phi_i_p 
			       + timestep * theta * (K_F/viscosity_F) * grad_p * phi_i_grads_p
			       - timestep * theta * density_biot * (K_F/viscosity_F) * g_biot * phi_i_grads_p 
				+ timestep * theta * dirac_force_injection * const_Q_F  //1
			       * p_value * phi_i_p
			       - timestep *  theta * q_biot_F * phi_i_p
			       - gamma_fixed_stress_local * (old_fixed_stress_p - old_timestep_p) * phi_i_p
			       ) * chi_fracture_values  *  fe_values_pressure.JxW(q);


	      // end i	  
	    } 	
	  // end n_q_points 		   
	} 
      
      cell->get_dof_indices (local_dof_indices);
      constraints_pressure.distribute_local_to_global (local_rhs, local_dof_indices,
						       system_rhs_pressure);
      
      
     }  // end cell
  
  // For MPI
  system_rhs_pressure.compress(VectorOperation::add);

  //timer.exit_section();
}





template <int dim>
void 
FracturePhaseFieldProblem<dim>::solve_pressure () 
{  
  cout<<"Solve Pressure " << endl;

  // TODO: Maybe iterative solver (GMRES)
  LA::MPI::BlockVector sol(system_rhs_pressure);    
  sol = newton_update_pressure;    
  
  SolverControl cn(10000,1e-15*system_rhs_pressure.l2_norm());
  TrilinosWrappers::SolverDirect solver(cn);
  solver.solve(system_matrix_pressure.block(0,0), sol.block(0), system_rhs_pressure.block(0));
    
  constraints_pressure.distribute (sol);
  newton_update_pressure = sol ;
}


// This is the Newton iteration to solve the 
// system of equations. First, we declare some
// standard parameters of the solution method. Addionally,
// we also implement an easy line search algorithm. 
template <int dim>
void 
FracturePhaseFieldProblem<dim>::newton_iteration_pressure (const double time) 
					       
{ 
  pcout << "It.\tResidual\tReduction\tLSrch\t#LinIts" << std::endl;  

  // Decision whether the system matrix should be build
  // at each Newton step
  const double nonlinear_rho = 0.1; 
 
  // Line search parameters
  unsigned int line_search_step;
  double new_newton_residuum;
  
  // Application of the initial boundary conditions to the 
  // variational equations:
  set_initial_bc_pressure (time);
  assemble_system_rhs_pressure();

  double newton_residuum = system_rhs_pressure.linfty_norm(); 
  double old_newton_residuum= newton_residuum;
  unsigned int newton_step = 1;

  
  if (newton_residuum < lower_bound_newton_residual_pressure)
    {
      pcout << '\t' 
	    << std::scientific 
	    << newton_residuum << " -- at begin"  
	    << std::endl;     
    }
  
  cout<<"newton_residuum = " << newton_residuum
      <<"lower_bound_newton_residual_pressure = " <<lower_bound_newton_residual_pressure
      <<endl;

  while (newton_residuum > lower_bound_newton_residual_pressure 
	 &&
	 newton_step < max_no_newton_steps) // TODO: hard-coded
    {
      old_newton_residuum = newton_residuum;
      
      
      assemble_system_rhs_pressure();
      newton_residuum = system_rhs_pressure.linfty_norm();
  
      if (newton_residuum < lower_bound_newton_residual_pressure)
	{
	  pcout << '\t' 
		<< std::scientific 
		<< newton_residuum << " < TOL(Pressure)\n"<<std::endl;
	  break;
	}
      
      // TODO: let's comment for the moment since 
      // this is more robust
      // What this function does? - The Jacobian
      // is not build at every step and saves computational cost
      //if (newton_residuum/old_newton_residuum > nonlinear_rho)
      assemble_system_matrix_pressure ();	

      // Solve Ax = b
      solve_pressure ();	  
        

      LA::MPI::BlockVector saved_solution(partition_relevant_pressure);
      saved_solution  = solution_pressure;

      
      line_search_step = 0;	  
      for ( ; line_search_step < max_no_line_search_steps;  ++line_search_step)
	{	     		

	  LA::MPI::BlockVector distributed_newton_update(partition_pressure);
	  distributed_newton_update = newton_update_pressure;
	  LA::MPI::BlockVector distributed_solution(partition_pressure);
	  distributed_solution = solution_pressure;
	  
	  distributed_solution.add(distributed_newton_update);
	  solution_pressure = distributed_solution; 

	  assemble_system_rhs_pressure ();			
	  new_newton_residuum = system_rhs_pressure.linfty_norm();
	  
	  if (new_newton_residuum < newton_residuum){
	      break;
	  }

	  
	  solution_pressure = saved_solution;
	  distributed_newton_update *= line_search_damping;	  
	  newton_update_pressure = distributed_newton_update;


	}	   

      pcout << std::setprecision(5) <<newton_step << '\t' 
	    << std::scientific << newton_residuum << '\t'
	    << std::scientific << newton_residuum/old_newton_residuum  <<'\t'
	    << std::scientific << line_search_step <<'\t'
	    << "DirectSolve";
//      if (newton_residuum/old_newton_residuum > nonlinear_rho)
//	pcout << "Assemble" << '\t' ;
//      else 
//	pcout << "N.A " << '\t' ;

      pcout<<std::endl;
   
     

      newton_step++;      
    }

}


// End of pressure system
////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////

// We come now to the displacement-phase-field system
// In this method, we set up runtime parameters that
// could also come from a paramter file.
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::set_runtime_parameters ()
  {

    // Get parameters from file
    prm.enter_subsection("Optimal parameters");
    length_1 = prm.get_double("length 1");
    length_2 = prm.get_double("length 2");
    prm.leave_subsection();

    // Get parameters from file
    prm.enter_subsection("Global parameters");
    n_global_pre_refine = prm.get_integer("Global pre-refinement steps");
    n_local_pre_refine = prm.get_integer("Local pre-refinement steps");
    n_refinement_cycles = prm.get_integer("Adaptive refinement cycles");
    max_no_timesteps = prm.get_integer("Max No of timesteps");
    timestep = prm.get_double("Timestep size");
    timestep_size_2 = prm.get_double("Timestep size to switch to");
    switch_timestep = prm.get_integer("Switch timestep after steps");

   if (prm.get("outer solver")=="active set")
      outer_solver = OuterSolverType::active_set;
   else if (prm.get("outer solver")=="simple monolithic")
     outer_solver = OuterSolverType::simple_monolithic;

    if (prm.get("test case")=="sneddon 2d")
      test_case = TestCase::sneddon_2d;
    else if (prm.get("test case")=="sneddon 3d")
      test_case = TestCase::sneddon_3d;

    else if (prm.get("test case")=="miehe tension")
      test_case = TestCase::miehe_tension; // straight crack
    else if (prm.get("test case")=="miehe shear")
      test_case = TestCase::miehe_shear; // curved crack

    else if (prm.get("test case")=="miehe tension 3D")
      test_case = TestCase::miehe_tension_3D; // curved crack


    else if (prm.get("test case")=="multiple homo")
      test_case = TestCase::multiple_homo; // multiple fractures homogeneous material

    else if (prm.get("test case")=="multiple homo 3d")
      test_case = TestCase::multiple_homo_3d; // multiple fractures homogeneous material

    else if (prm.get("test case")=="multiple homo parallel")
      test_case = TestCase::multiple_homo_parallel; // multiple fractures homogeneous material

    else if (prm.get("test case")=="multiple homo parallel 3d")
      test_case = TestCase::multiple_homo_parallel_3d; // multiple fractures homogeneous material



    else if (prm.get("test case")=="multiple het")
      test_case = TestCase::multiple_het; // multiple fractures heterogeneous material
    else if (prm.get("test case")=="multiple het 3d")
      test_case = TestCase::multiple_het_3d; // multiple fractures heterogeneous material

    else if (prm.get("test case")=="gupta 3d")
      test_case = TestCase::gupta_3d; // multiple fractures heterogeneous material

    else if (prm.get("test case")=="hole")
      test_case = TestCase::hole; // multiple fractures heterogeneous material

    else
      AssertThrow(false, ExcNotImplemented());

    if (prm.get("ref strategy")=="phase field")
      refinement_strategy = RefinementStrategy::phase_field_ref;
    else if (prm.get("ref strategy")=="fixed preref sneddon")
      refinement_strategy = RefinementStrategy::fixed_preref_sneddon;

    else if (prm.get("ref strategy")=="fixed preref sneddon 3D")
      refinement_strategy = RefinementStrategy::fixed_preref_sneddon_3D;

    else if (prm.get("ref strategy")=="fixed preref miehe tension")
      refinement_strategy = RefinementStrategy::fixed_preref_miehe_tension;
    else if (prm.get("ref strategy")=="fixed preref miehe shear")
      refinement_strategy = RefinementStrategy::fixed_preref_miehe_shear;
   else if (prm.get("ref strategy")=="fixed preref multiple homo")
      refinement_strategy = RefinementStrategy::fixed_preref_multiple_homo;
   else if (prm.get("ref strategy")=="fixed preref multiple het")
      refinement_strategy = RefinementStrategy::fixed_preref_multiple_het;
   else if (prm.get("ref strategy")=="global")
      refinement_strategy = RefinementStrategy::global;
   else if (prm.get("ref strategy")=="mix")
      refinement_strategy = RefinementStrategy::mix;
   else
     AssertThrow(false, ExcNotImplemented());
    
    // Refinemet Criteria
    value_phase_field_for_refinement
      = prm.get_double("value phase field for refinement");

    filename_basis  = prm.get ("Output filename");

    prm.leave_subsection();

    prm.enter_subsection("Problem dependent parameters");

    
    // Material and problem-rhs parameters
    func_pressure.initialize ("time", prm.get("Pressure"),
			      FunctionParser<1>::ConstMap());
    
    // Debug: G_c is given by a function
    G_c = prm.get_double("Fracture toughness G_c");
    G_c_2 =  prm.get_double("Fracture toughness G_c 2");
    density_structure = prm.get_double("Density solid");
    

    if (test_case == TestCase::sneddon_2d ||
	test_case == TestCase::sneddon_3d || 
	test_case == TestCase::multiple_homo || 
	test_case == TestCase::multiple_homo_3d || 
	test_case == TestCase::multiple_het ||
	test_case == TestCase::multiple_het_3d|| 
	test_case == TestCase::gupta_3d ||
	test_case == TestCase::hole )
      {
	// Setting 
	// possion_ratio_nu : from param.file
	// E_modulus        : from param.file
	// from those
	// we calculate 
	// lame_coefficient_mu 
	// lame_coeffiecint_lambda
	poisson_ratio_nu = prm.get_double("Poisson ratio nu");
	E_modulus = prm.get_double("E modulus");
	
	lame_coefficient_mu = E_modulus / (2.0 * (1 + poisson_ratio_nu));
	
	lame_coefficient_lambda = (2 * poisson_ratio_nu * lame_coefficient_mu)
	  / (1.0 - 2 * poisson_ratio_nu);
		
      }
    else if( test_case == TestCase::multiple_homo_parallel || 
	     test_case == TestCase::multiple_homo_parallel_3d || 
	     test_case == TestCase::miehe_tension_3D ||
	     test_case == TestCase::miehe_tension ||
	     test_case == TestCase::miehe_shear){


      lame_coefficient_mu = prm.get_double("Lame mu");
      lame_coefficient_lambda = prm.get_double("Lame lambda");

      
      double lambda= lame_coefficient_lambda; 
      double G = lame_coefficient_mu;
      E_modulus = G * ( 3. * lambda + 2.*G ) / ( lambda + G) ;
      poisson_ratio_nu = lambda / ( 2. * ( lambda + G));
      
    } 
    else{
      cout<<" ERROR in Setting Physical Parameters ! " << endl;
      exit(0);
    }


    // bDarcy determines whether alpha_biot is 0 or 1
    // Replace by solve_only_elasticity_phase_field
    //    if(bDarcy){
      pressure_diff_x_1 = prm.get_double("pressure diff x1");
      pressure_diff_x_2 = prm.get_double("pressure diff x2");

      pressure_wellbore = prm.get_double("pressure wellbore");

      TOL_Fixed_Stress = prm.get_double("tol fixed stress");

      TOL_Fixed_Stress_Two = prm.get_double("tol fixed stress two");
      //   }

    E_prime = E_modulus / (1.0 - poisson_ratio_nu * poisson_ratio_nu);



    // Pressure equation parameters
    double M_biot = prm.get_double("M Biot");
    alpha_biot = prm.get_double("alpha Biot coefficient");
    c_biot = 1.0/M_biot;

    c_F = prm.get_double("Compressibility Fracture");
 
    viscosity_biot = prm.get_double("Viscosity Reservoir");
    viscosity_F    = prm.get_double("Viscosity Fracture");

    K_biot = prm.get_double("Permeability Reservoir");
    density_biot = prm.get_double("Density Reservoir");

    bPeaceman_well_model = prm.get_bool("use peaceman well model");

    gravity_x = 0.0;
    gravity_y = 0.0;
    volume_source = 0.0;
    dirac_force_injection = 0.0;


    prm.leave_subsection();

    // Vector for output of level_set width
    //solution_stress_per_cell.reinit(triangulation.n_active_cells());

    // A variable to count the number of time steps
    timestep_number = 0;

    // Counts total time
    time = 0;

    bool three_parallel = true;


    
    if (test_case != TestCase::hole){
      // Debug: improve the switch which
      // grid for which test case is used
      std::string grid_name;
      if (test_case == TestCase::sneddon_2d ||
	  test_case == TestCase::multiple_homo ||
	  test_case == TestCase::multiple_homo_parallel ||
	  test_case == TestCase::multiple_het)
	grid_name = "input_msh/unit_square_4.inp";
      else if (test_case == TestCase::sneddon_3d)
	grid_name = "input_msh/unit_cube_10.inp";
      else if( test_case == TestCase::multiple_homo_parallel_3d ){ 
	if(three_parallel == false)
	  grid_name = "input_msh/unit_cube_4.inp";
	else if(three_parallel == true) 
	  grid_name = "input_msh/unit_cube_4.inp";
      }
      else if(test_case == TestCase::gupta_3d)	
	grid_name = "input_msh/unit_cube_4.inp"; 
      else if (test_case == TestCase::multiple_homo_3d || 
	       test_case == TestCase::multiple_het_3d)
	grid_name = "input_msh/unit_cube_4.inp";
      else if (test_case == TestCase:: miehe_tension_3D)
	grid_name  = "input_msh/unit_slit_3D.inp";
      else
	grid_name  = "input_msh/unit_slit.inp";
      
      GridIn<dim> grid_in;
      grid_in.attach_triangulation(triangulation);
      std::ifstream input_file(grid_name.c_str());
      
      grid_in.read_ucd(input_file);
    }
    else if(test_case == TestCase::hole){
      double inner_radius = 0.1;
      double outer_radius = 2.;
      GridGenerator::hyper_cube_with_cylindrical_hole(triangulation,inner_radius,outer_radius,false);
      Point<dim> center(0.,0.);
      static const HyperBallBoundary<dim> inner_cylinder( center, inner_radius );
      triangulation.set_boundary (1, inner_cylinder);  
    }
    

    if(b_input_file){
      //input_file
      cout<<"Read-input_File"<< endl;
      std::ostringstream filename_1;
      filename_1 << "sol_files/soln_";
      triangulation.load (filename_1.str().c_str());
      cout<<"Got-input_File"<< endl;
      cout<<"Number of cells : " << triangulation.n_active_cells()<< endl;
    }


    if(!b_input_file)
      triangulation.refine_global(n_global_pre_refine);
    

    
    if (test_case == TestCase::multiple_het){
      func_emodulus = new BitmapFunction<dim>("input_msh/test.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
    }
    else if (test_case == TestCase::multiple_het_3d){
      func_emodulus = new BitmapFunction<dim>("input_msh/test.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
      func_emodulus_0 = new BitmapFunction<dim>("input_msh/test_90.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
      func_emodulus_1 = new BitmapFunction<dim>("input_msh/test_180.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
      func_emodulus_2 = new BitmapFunction<dim>("input_msh/test_m90.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
      func_emodulus_3 = new BitmapFunction<dim>("input_msh/test_horiz.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
      func_emodulus_4 = new BitmapFunction<dim>("input_msh/test_vert.pgm",0,4,0,4,E_modulus,10.0*E_modulus);
    }


    prm.enter_subsection("Solver parameters");
    direct_solver = prm.get_bool("Use Direct Inner Solver");
    
    // Newton tolerances and maximum steps
    lower_bound_newton_residuum = prm.get_double("Newton lower bound");
    max_no_newton_steps = prm.get_integer("Newton maximum steps");

    // Line search control
    max_no_line_search_steps = prm.get_integer("Line search maximum steps");
    line_search_damping = prm.get_double("Line search damping");

    // Newton tolerance pressure equation
    lower_bound_newton_residual_pressure = prm.get_double("Newton lower bound pressure");

    // Decompose stress in plus (tensile) and minus (compression)
    // 0.0: no decomposition, 1.0: with decomposition
    // Motivation see Miehe et al. (2010)
    decompose_stress_rhs = prm.get_double("Decompose stress in rhs");
    decompose_stress_matrix = prm.get_double("Decompose stress in matrix");

    // For pf_extra
    use_old_timestep_pf = false;



    // For Backward Euler scheme
    // Could be later extended to 2nd order 
    // timestepping schemes using theta = 0.5
    theta = 1.0; 


    // Fixed-stress stabilization
    double K_dr = 0.;
    // Redefined locally in order to account for 
    // heterogeneous media.
    beta_fixed_stress = (alpha_biot * alpha_biot * (1.0+poisson_ratio_nu) * (1.0 - 2.0 * poisson_ratio_nu))/ (2. * E_modulus *  poisson_ratio_nu);
    gamma_fixed_stress = 0.0; // Option 2: beta_fixed_stress; //Option 3: M_biot * alpha_biot * alpha_biot * c_F  * beta_fixed_stress;
    
    if( gamma_fixed_stress !=0. && 4. * E_modulus / (2.0 * (1 + poisson_ratio_nu)) < 1./gamma_fixed_stress) 
      exit(0);


    pcout<<"********************************************"<<endl;
    pcout<<" c_Biot + Beta-fixed-stress = " << c_biot + beta_fixed_stress << endl;
    pcout<<" c_F + gamma_fixed_stress = " << c_F + gamma_fixed_stress << endl;
    pcout<<" c_biot = " << c_biot << ", c_F = " << c_F <<  endl;
    pcout<<" Gamma-fixed-stress = " << gamma_fixed_stress << endl;
    pcout<<"********************************************"<<endl;


    double center = 2.;


    if (test_case == TestCase::multiple_het){

      point_0[0] = 100002.5; //center; //- length_1;
      point_0[1] = 100001.25; //center;
      
      point_1[0] = 100001.25;// center-length_1;
      point_1[1] = 100002.5;//center;
      
      point_2[0] = 10000.;
      point_2[1] = 10000.;
      
      point_3[0] = 9.;
      point_3[1] = 1.;
      
      point_4[0] = 9.;
      point_4[1] = 9.;
      
    }
    else  if( test_case == TestCase::hole){
      point_0[0] = 0.; //center; //- length_1;
      point_0[1] = 0.248438; //center;
      
      point_1[0] = 0.248438;// center-length_1;
      point_1[1] =0.;//center;
      
    }
    else{
      
      point_0[0] = center; //- length_1;
      point_0[1] = center;
      if(dim == 3)
	point_0[2] = center;
      
    point_1[0] = center-length_1;
    point_1[1] = center;
    
    point_2[0] = center+ length_2;
    point_2[1] = center;
    
    point_3[0] = 9.;
    point_3[1] = 1.;
    
    point_4[0] = 9.;
    point_4[1] = 9.;
    }
    
    prm.leave_subsection();
  }


// This function is similar to many deal.II tuturial steps.
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::setup_system ()
  {

    system_pde_matrix.clear();

    dof_handler.distribute_dofs(fe);

    std::vector<unsigned int> sub_blocks (dim+1,0);
    sub_blocks[dim] = 1;
    DoFRenumbering::component_wise (dof_handler, sub_blocks);

    // FOR AMG Preconditioner.
    FEValuesExtractors::Vector extract_displacement(0);
    constant_modes.clear();
    DoFTools::extract_constant_modes(dof_handler,
				     fe.component_mask(extract_displacement), constant_modes);

    std::vector<types::global_dof_index> dofs_per_block (2);
    DoFTools::count_dofs_per_block (dof_handler, dofs_per_block, sub_blocks);
    const unsigned int n_solid = dofs_per_block[0];
    const unsigned int n_phase = dofs_per_block[1];
    pcout << "====== # of Dofs ==========================" << std::endl;
    pcout << "DoFs: " << n_solid << " solid + " << n_phase << " phase = "
	  << n_solid + n_phase << std::endl;
    
    partition.clear();
    if (direct_solver)
      {
	partition.push_back(dof_handler.locally_owned_dofs());
      }
    else
      {	
	partition.push_back(dof_handler.locally_owned_dofs().get_view(0,n_solid));
	partition.push_back(dof_handler.locally_owned_dofs().get_view(n_solid,n_solid+n_phase));
      }
    

    DoFTools::extract_locally_relevant_dofs(dof_handler, relevant_set);
    partition_relevant.clear();
    if (direct_solver)
      {
	partition_relevant.push_back(relevant_set);
      }
    else
      {
	partition_relevant.push_back(relevant_set.get_view(0,n_solid));
	partition_relevant.push_back(relevant_set.get_view(n_solid,n_solid+n_phase));
      }

    
    {
      constraints_hanging_nodes.clear();
      constraints_hanging_nodes.reinit(relevant_set);
      DoFTools::make_hanging_node_constraints(dof_handler,
					      constraints_hanging_nodes);
      constraints_hanging_nodes.close();
    }
    {
      constraints_update.clear();
      constraints_update.reinit(relevant_set);
      
      set_newton_bc();

      // DEBUG : right_object_wins 
      /*
	Using the default value of the second arguments, 
	the constraints in each of the two objects 
	(the old one represented by this object and the argument) 
	may not refer to the same degree of freedom, 
	i.e. a degree of freedom that is constrained in one object 
	may not be constrained in the second. 
	If this is nevertheless the case, an exception is thrown. 
	However, this behavior can be changed by providing 
	a different value for the second argument.
      */
      constraints_update.merge(constraints_hanging_nodes,
			       ConstraintMatrix::right_object_wins);

      //pcout << "  - Setup System :: constraints :: merge end" << endl;
      constraints_update.close();
    }
    
    {
      TrilinosWrappers::BlockSparsityPattern csp(partition, mpi_com);
      
      DoFTools::make_sparsity_pattern(dof_handler, csp,
				      constraints_update,
				      false,
				      Utilities::MPI::this_mpi_process(mpi_com)); 
      
      csp.compress();
      system_pde_matrix.reinit(csp);
    }

    // Actual solution at time step n
    //solution.reinit(partition);
    solution.reinit(partition_relevant, mpi_com);
    
    // Old timestep solution at time step n-1
    old_solution.reinit(partition_relevant,mpi_com);
    
    // Old timestep solution at time step n-2
    old_old_solution.reinit(partition_relevant,mpi_com);


    // Old fixed stress solution at time step n-1
    old_fixed_stress_solution.reinit (partition_relevant, mpi_com);
    
    // A tmp solution vector
    tmp_solution.reinit (partition_relevant, mpi_com);

    
    // Updates for Newton's method
    //newton_update.reinit(partition);
    newton_update.reinit(partition_relevant,mpi_com);
    // Residual for  Newton's method

    system_pde_residual.reinit(partition,mpi_com); 
    system_total_residual.reinit(partition, mpi_com);
    
    diag_mass.reinit(partition);   
    diag_mass_relevant.reinit(partition_relevant);
    assemble_diag_mass_matrix();
    
    active_set.clear();
    active_set.set_size(dof_handler.n_dofs());
    
  }


// Now, there follow several functions to perform
// the spectral decomposition of the stress tensor
// into tension and compression parts
// assumes the matrix is symmetric!
// The explicit calculation does only work 
// in 2d. For 3d, we should use other libraries or approximative
// tools to compute eigenvectors and -functions.
// Borden et al. (2012, 2013) suggested some papers to look into.
// TODO: This explicit computations is not possible any more in 3d!!!!
// See Borden et al. (2013) how to implement this in 3d
// Or use the law proposed by Amor et al. (2009)
template <int dim>
void eigen_vectors_and_values(
    double & E_eigenvalue_1, double & E_eigenvalue_2,
    Tensor<2,dim> & ev_matrix,
    const Tensor<2,dim> & matrix)
{

  // Debug: there is a little mistake
  double sq = std::sqrt((matrix[0][0] - matrix[1][1]) * (matrix[0][0] - matrix[1][1]) + 4.0*matrix[0][1]*matrix[1][0]);
  E_eigenvalue_1 = 0.5 * ((matrix[0][0] + matrix[1][1]) + sq);
  E_eigenvalue_2 = 0.5 * ((matrix[0][0] + matrix[1][1]) - sq);

  // Compute eigenvectors
  Tensor<1,dim> E_eigenvector_1;
  Tensor<1,dim> E_eigenvector_2;
  if (std::abs(matrix[0][1]) < 1e-10*std::abs(matrix[0][0]))
    { // E is close to diagonal
      E_eigenvector_1[0]=0;
      E_eigenvector_1[1]=1;
      E_eigenvector_2[0]=1;
      E_eigenvector_2[1]=0;
    }
  else
    {
      E_eigenvector_1[0] = 1.0/(std::sqrt(1 + (E_eigenvalue_1 - matrix[0][0])/matrix[0][1] * (E_eigenvalue_1 - matrix[0][0])/matrix[0][1]));
      E_eigenvector_1[1] = (E_eigenvalue_1 - matrix[0][0])/(matrix[0][1] * (std::sqrt(1 + (E_eigenvalue_1 - matrix[0][0])/matrix[0][1] * (E_eigenvalue_1 - matrix[0][0])/matrix[0][1])));
      E_eigenvector_2[0] = 1.0/(std::sqrt(1 + (E_eigenvalue_2 - matrix[0][0])/matrix[0][1] * (E_eigenvalue_2 - matrix[0][0])/matrix[0][1]));
      E_eigenvector_2[1] = (E_eigenvalue_2 - matrix[0][0])/(matrix[0][1] * (std::sqrt(1 + (E_eigenvalue_2 - matrix[0][0])/matrix[0][1] * (E_eigenvalue_2 - matrix[0][0])/matrix[0][1])));
    }

  ev_matrix[0][0] = E_eigenvector_1[0];
  ev_matrix[0][1] = E_eigenvector_2[0];
  ev_matrix[1][0] = E_eigenvector_1[1];
  ev_matrix[1][1] = E_eigenvector_2[1];

  // Sanity check if orthogonal
  double scalar_prod = 1.0e+10;
  scalar_prod = E_eigenvector_1[0] * E_eigenvector_2[0] + E_eigenvector_1[1] * E_eigenvector_2[1]; 

  if (scalar_prod > 1.0e-6)
    {
      std::cout << "Seems not to be orthogonal" << std::endl;
      abort();
    }
}


template <int dim>
void decompose_stress(
			   Tensor<2,dim> &stress_term_plus,
			   Tensor<2,dim> &stress_term_minus,
			   const Tensor<2, dim> &E,
			   const double tr_E,
			   const Tensor<2, dim> &E_LinU,
			   const double tr_E_LinU,
			   const double lame_coefficient_lambda,
			   const double lame_coefficient_mu,
			   const bool derivative)
{
  static const Tensor<2, dim> Identity =
    Tensors::get_Identity<dim>();

  Tensor<2, dim> zero_matrix;
  zero_matrix.clear();


  // Compute first the eigenvalues for u (as in the previous function)
  // and then for \delta u

  // Compute eigenvalues/vectors
  double E_eigenvalue_1, E_eigenvalue_2;
  Tensor<2,dim> P_matrix;
  eigen_vectors_and_values(E_eigenvalue_1, E_eigenvalue_2,P_matrix,E);

  double E_eigenvalue_1_plus = std::max(0.0, E_eigenvalue_1);
  double E_eigenvalue_2_plus = std::max(0.0, E_eigenvalue_2);

  Tensor<2,dim> Lambda_plus;
  Lambda_plus[0][0] = E_eigenvalue_1_plus;
  Lambda_plus[0][1] = 0.0;
  Lambda_plus[1][0] = 0.0;
  Lambda_plus[1][1] = E_eigenvalue_2_plus;

  if (!derivative)
    {
      Tensor<2,dim> E_plus = P_matrix * Lambda_plus * transpose(P_matrix);

      double tr_E_positive = std::max(0.0, tr_E);

      stress_term_plus = lame_coefficient_lambda * tr_E_positive * Identity
          + 2 * lame_coefficient_mu * E_plus;

      stress_term_minus = lame_coefficient_lambda * (tr_E - tr_E_positive) * Identity
          + 2 * lame_coefficient_mu * (E - E_plus);
    }
  else
    {  // Derviatives (\delta u)

      // Compute eigenvalues/vectors
      double E_eigenvalue_1_LinU, E_eigenvalue_2_LinU;
      Tensor<1,dim> E_eigenvector_1_LinU;
      Tensor<1,dim> E_eigenvector_2_LinU;
      Tensor<2,dim> P_matrix_LinU;

      // Compute linearized Eigenvalues
      double diskriminante = std::sqrt(E[0][1] * E[1][0] + (E[0][0] - E[1][1]) * (E[0][0] - E[1][1])/4.0);

      E_eigenvalue_1_LinU = 0.5 * tr_E_LinU + 1.0/(2.0 * diskriminante) * 
	(E_LinU[0][1] * E[1][0] + E[0][1] * E_LinU[1][0] + (E[0][0] - E[1][1])*(E_LinU[0][0] - E_LinU[1][1])/2.0);

      E_eigenvalue_2_LinU = 0.5 * tr_E_LinU - 1.0/(2.0 * diskriminante) * 
	(E_LinU[0][1] * E[1][0] + E[0][1] * E_LinU[1][0] + (E[0][0] - E[1][1])*(E_LinU[0][0] - E_LinU[1][1])/2.0);


      // Compute normalized Eigenvectors and P
      double normalization_1 = 1.0/(std::sqrt(1 + (E_eigenvalue_1 - E[0][0])/E[0][1] * (E_eigenvalue_1 - E[0][0])/E[0][1]));
      double normalization_2 = 1.0/(std::sqrt(1 + (E_eigenvalue_2 - E[0][0])/E[0][1] * (E_eigenvalue_2 - E[0][0])/E[0][1]));

      double normalization_1_LinU = 0.0;
      double normalization_2_LinU = 0.0;

      normalization_1_LinU = -1.0 * (1.0/(1.0 + (E_eigenvalue_1 - E[0][0])/E[0][1] * (E_eigenvalue_1 - E[0][0])/E[0][1])
				     * 1.0/(2.0 * std::sqrt(1.0 + (E_eigenvalue_1 - E[0][0])/E[0][1] * (E_eigenvalue_1 - E[0][0])/E[0][1]))
				     * (2.0 * (E_eigenvalue_1 - E[0][0])/E[0][1]) 
				     * ((E_eigenvalue_1_LinU - E_LinU[0][0]) * E[0][1] - (E_eigenvalue_1 - E[0][0]) * E_LinU[0][1])/(E[0][1] * E[0][1]));

      normalization_2_LinU = -1.0 * (1.0/(1.0 + (E_eigenvalue_2 - E[0][0])/E[0][1] * (E_eigenvalue_2 - E[0][0])/E[0][1])
				     * 1.0/(2.0 * std::sqrt(1.0 + (E_eigenvalue_2 - E[0][0])/E[0][1] * (E_eigenvalue_2 - E[0][0])/E[0][1]))
				     * (2.0 * (E_eigenvalue_2 - E[0][0])/E[0][1]) 
				     * ((E_eigenvalue_2_LinU - E_LinU[0][0]) * E[0][1] - (E_eigenvalue_2 - E[0][0]) * E_LinU[0][1])/(E[0][1] * E[0][1]));


      E_eigenvector_1_LinU[0] = normalization_1 * 1.0;
      E_eigenvector_1_LinU[1] = normalization_1 * (E_eigenvalue_1 - E[0][0])/E[0][1];

      E_eigenvector_2_LinU[0] = normalization_2 * 1.0;
      E_eigenvector_2_LinU[1] = normalization_2 * (E_eigenvalue_2 - E[0][0])/E[0][1];

      
      // Apply product rule to normalization and vector entries
      double EV_1_part_1_comp_1 = 0.0;  // LinU in vector entries, normalization U
      double EV_1_part_1_comp_2 = 0.0;  // LinU in vector entries, normalization U
      double EV_1_part_2_comp_1 = 0.0;  // vector entries U, normalization LinU
      double EV_1_part_2_comp_2 = 0.0;  // vector entries U, normalization LinU

      double EV_2_part_1_comp_1 = 0.0;  // LinU in vector entries, normalization U
      double EV_2_part_1_comp_2 = 0.0;  // LinU in vector entries, normalization U
      double EV_2_part_2_comp_1 = 0.0;  // vector entries U, normalization LinU
      double EV_2_part_2_comp_2 = 0.0;  // vector entries U, normalization LinU

      // Effizienter spaeter, aber erst einmal uebersichtlich und verstehen!
      EV_1_part_1_comp_1 = normalization_1 * 0.0;
      EV_1_part_1_comp_2 = normalization_1 * 
	((E_eigenvalue_1_LinU - E_LinU[0][0]) * E[0][1] - (E_eigenvalue_1 - E[0][0]) * E_LinU[0][1])/(E[0][1] * E[0][1]);
 
      EV_1_part_2_comp_1 = normalization_1_LinU * 1.0;
      EV_1_part_2_comp_2 = normalization_1_LinU * (E_eigenvalue_1 - E[0][0])/E[0][1];


      EV_2_part_1_comp_1 = normalization_2 * 0.0;
      EV_2_part_1_comp_2 = normalization_2 * 
	((E_eigenvalue_2_LinU - E_LinU[0][0]) * E[0][1] - (E_eigenvalue_2 - E[0][0]) * E_LinU[0][1])/(E[0][1] * E[0][1]);
      
      EV_2_part_2_comp_1 = normalization_2_LinU * 1.0;
      EV_2_part_2_comp_2 = normalization_2_LinU * (E_eigenvalue_2 - E[0][0])/E[0][1];

      
      
      // Build eigenvectors
      E_eigenvector_1_LinU[0] = EV_1_part_1_comp_1 + EV_1_part_2_comp_1;
      E_eigenvector_1_LinU[1] = EV_1_part_1_comp_2 + EV_1_part_2_comp_2;

      E_eigenvector_2_LinU[0] = EV_2_part_1_comp_1 + EV_2_part_2_comp_1;
      E_eigenvector_2_LinU[1] = EV_2_part_1_comp_2 + EV_2_part_2_comp_2;
      


      // P-Matrix
      P_matrix_LinU[0][0] = E_eigenvector_1_LinU[0];
      P_matrix_LinU[0][1] = E_eigenvector_2_LinU[0];
      P_matrix_LinU[1][0] = E_eigenvector_1_LinU[1];
      P_matrix_LinU[1][1] = E_eigenvector_2_LinU[1];


      double E_eigenvalue_1_plus_LinU = 0.0; 
      double E_eigenvalue_2_plus_LinU = 0.0;


  // Very important: Set E_eigenvalue_1_plus_LinU to zero when 
  // the corresponding rhs-value is set to zero and NOT when
  // the value itself is negative!!!
  if (E_eigenvalue_1 < 0.0)
    {
      E_eigenvalue_1_plus_LinU = 0.0;
    }
  else
    E_eigenvalue_1_plus_LinU = E_eigenvalue_1_LinU;


  if (E_eigenvalue_2 < 0.0)
    {
      E_eigenvalue_2_plus_LinU = 0.0;
    }
  else
    E_eigenvalue_2_plus_LinU = E_eigenvalue_2_LinU;


  
  Tensor<2,dim> Lambda_plus_LinU;
  Lambda_plus_LinU[0][0] = E_eigenvalue_1_plus_LinU;
  Lambda_plus_LinU[0][1] = 0.0;
  Lambda_plus_LinU[1][0] = 0.0;
  Lambda_plus_LinU[1][1] = E_eigenvalue_2_plus_LinU;
  
  Tensor<2,dim> E_plus_LinU = P_matrix_LinU * Lambda_plus * transpose(P_matrix) +  P_matrix * Lambda_plus_LinU * transpose(P_matrix) + P_matrix * Lambda_plus * transpose(P_matrix_LinU);


  double tr_E_positive_LinU = 0.0; 
  if (tr_E < 0.0)
    {
      tr_E_positive_LinU = 0.0;

    }
  else 
    tr_E_positive_LinU = tr_E_LinU;



  stress_term_plus = lame_coefficient_lambda * tr_E_positive_LinU * Identity
    + 2 * lame_coefficient_mu * E_plus_LinU;
  
  stress_term_minus = lame_coefficient_lambda * (tr_E_LinU - tr_E_positive_LinU) * Identity
    + 2 * lame_coefficient_mu * (E_LinU - E_plus_LinU);
    

    }


}






// In this function, we assemble the Jacobian matrix
// for the Newton iteration.
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::assemble_system (bool residual_only)
  {
    if (residual_only)
        system_total_residual = 0;
    else
        system_pde_matrix = 0;
    system_pde_residual = 0;

    
    // It seems that Lobatto works better with active set
    //QGauss<dim> quadrature_formula(degree + 2);
    QGaussLobatto<dim> quadrature_formula(degree + 1);
   

    FEValues<dim> fe_values_pressure (fe_pressure, quadrature_formula,
				      update_values    |
				      update_quadrature_points  |
				      update_JxW_values |
				      update_gradients);
    
    
    FEValues<dim> fe_values(fe, quadrature_formula,
			    update_values | update_quadrature_points | update_JxW_values
			    | update_gradients);
    
    const unsigned int dofs_per_cell = fe.dofs_per_cell;

    const unsigned int n_q_points = quadrature_formula.size();

    FullMatrix<double> local_matrix(dofs_per_cell, dofs_per_cell);
    Vector<double> local_rhs(dofs_per_cell);

    std::vector<types::global_dof_index> local_dof_indices(fe.dofs_per_cell);

    const FEValuesExtractors::Vector displacements(0);
    const FEValuesExtractors::Scalar phase_field (dim); // 2


    std::vector<Vector<double> > old_solution_values_pressure (n_q_points,
                                                             Vector<double>(1));

    std::vector<std::vector<Tensor<1,dim> > > old_solution_grads_pressure (n_q_points,
                                                                         std::vector<Tensor<1,dim> > (1));

    std::vector<Vector<double> > old_solution_values(n_q_points,
						     Vector<double>(dim+1));
    
    std::vector<std::vector<Tensor<1,dim> > > old_solution_grads (n_q_points,
								  std::vector<Tensor<1,dim> > (dim+1));
    
    std::vector<Vector<double> > old_timestep_solution_values(n_q_points,
							      Vector<double>(dim+1));
    
    std::vector<std::vector<Tensor<1,dim> > > old_timestep_solution_grads (n_q_points,
									   std::vector<Tensor<1,dim> > (dim+1));
    
    std::vector<Vector<double> > old_old_timestep_solution_values(n_q_points,
								  Vector<double>(dim+1));
    
    // Declaring test functions:
    std::vector<Tensor<1, dim> > phi_i_u(dofs_per_cell);
    std::vector<Tensor<2, dim> > phi_i_grads_u(dofs_per_cell);
    std::vector<double>          phi_i_pf(dofs_per_cell);
    std::vector<Tensor<1,dim> >  phi_i_grads_pf (dofs_per_cell);
    
    // This is the identity matrix in two dimensions:
    const Tensor<2, dim> Identity = Tensors::get_Identity<dim>();

    Tensor<2,dim> zero_matrix;
    zero_matrix.clear();

    Function_G_c<dim> function_G_c(G_c,G_c_2);
    std::vector<double> G_c_values(n_q_points);;

    int cell_index=0;
    
    InitialValues_Pressure<dim> initial_pressure(min_cell_diameter);
    std::vector<double> initial_pressure_values(n_q_points);

    typename DoFHandler<dim>::active_cell_iterator cell =
      dof_handler.begin_active(), endc = dof_handler.end();

    typename DoFHandler<dim>::active_cell_iterator
      cell_pressure = dof_handler_pressure.begin_active();
    
    
    double stress_split_switch = 1.;
    if(test_case == TestCase::sneddon_3d || 
       test_case == TestCase::sneddon_2d)
      stress_split_switch = 0.;
    

    
    for (; cell != endc; ++cell, ++cell_pressure, ++cell_index)
      if (cell->is_locally_owned()) 
        {
	  cell->get_dof_indices(local_dof_indices);
          fe_values.reinit(cell);
          fe_values_pressure.reinit(cell_pressure);
	  
	  
	  
	  // DEBUG
	  // The stress splitting does not work correctly when the fracture 
	  // touches the boundary. For this reason, we switch it off.
	  // Switch On/Off for splitting the Stress to Tensile and Compression
	  for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face) {
	    if (cell->face(face)->at_boundary()){
	      // if(cell->face(face)->boundary_indicator() == 1 || cell->face(face)->boundary_indicator() == 2 ){
	      if(dim == 3){
		if(timestep_number > 20)
		  stress_split_switch = 0.;
	      }
	      else if(dim == 2)
		stress_split_switch = 0.;
	    // }
	    }
	    // else
	    //  stress_split_switch = 1.;
	  }

//	stress_split_switch = 1.;


          // update lame coefficients based on current cell
	  // when working with heterogeneous materials
          if (test_case == TestCase::multiple_het || test_case == TestCase::multiple_het_3d )
            {
	      double x = cell->center()[0];
	      double y = cell->center()[1];
	      //double z = cell->center()[2];

	      //Point< dim, double >::Point(x,y,z); 	
	      /*
	      if( test_case == TestCase::multiple_het_3d ){
		  if( z >= 0 && z <= 0.25  || 
		      z >= 2 && z <= 2.25  ||
		      z >= 3 && z <= 3.25  ){
		    E_modulus = 1.0 + func_emodulus_0->value(Point<dim>(x,y,z), 0) ;		  
		    
		  }
		  else if( z > 0.25 && z <= 0.5 
			   || z > 2.25 && z <= 2.5
			   || z > 3.25 && z <= 3.5  ){
		    E_modulus = 1.0 + func_emodulus_1->value(Point<dim>(x,y,z), 0) ;
		  }
		  else if( z > 0.5 && z <= 0.75 || 
			   z > 1.5 && z <= 1.75 ||
			   z > 3.5 && z <= 3.75 ){
		    E_modulus = 1.0 + func_emodulus_2->value(Point<dim>(x,y,z), 0) ;
		  }
		  else if( z > 0.75 && z <= 1. ||
			   z > 1.75 && z <= 2. ||
			   z > 3.75 && z <= 4.){
		    E_modulus = 1.0 + func_emodulus->value(Point<dim>(x,y,z), 0) ;
		  }
		  else if( z >= 1 && z <= 1.25  ||
			   z > 2.5 && z <= 2.75  ){
		    E_modulus = 1.0 + func_emodulus_3->value(Point<dim>(x,y,z), 0) ;
		  }
		  else if( z > 1.25 && z <= 1.5 ||
			   z > 2.75 && z <= 3. ){
		    E_modulus = 1.0 + func_emodulus_4->value(Point<dim>(x,y,z), 0) ;
		  }
		  
		}
	      */
	      //else
{
		  E_modulus = 1.0 + func_emodulus->value(cell->center(), 0);
                 }



              lame_coefficient_mu = E_modulus / (2.0 * (1 + poisson_ratio_nu));

              lame_coefficient_lambda = (2 * poisson_ratio_nu * lame_coefficient_mu)
                     / (1.0 - 2 * poisson_ratio_nu);
            }

	  

	    function_G_c.value_list(fe_values.get_quadrature_points(), G_c_values);
	  

          local_matrix = 0;
          local_rhs = 0;

	  // Old Newton iteration values
	  fe_values.get_function_values (solution, old_solution_values);
	  fe_values.get_function_gradients (solution, old_solution_grads);

	  fe_values_pressure.get_function_values (solution_pressure, old_solution_values_pressure);
	  fe_values_pressure.get_function_gradients (solution_pressure, old_solution_grads_pressure);

	  // Old_timestep_solution values
	  fe_values.get_function_values (old_solution, old_timestep_solution_values);
	  fe_values.get_function_values (old_old_solution, old_old_timestep_solution_values);
	  
	  initial_pressure.value_list(fe_values_pressure.get_quadrature_points(),initial_pressure_values);
	  
	  {
	    for (unsigned int q = 0; q < n_q_points; ++q)
	      {
		for (unsigned int k = 0; k < dofs_per_cell; ++k)
		  {
		    phi_i_u[k]       = fe_values[displacements].value(k, q);
		    phi_i_grads_u[k] = fe_values[displacements].gradient(k, q);
		    phi_i_pf[k]       = fe_values[phase_field].value (k, q);
		    phi_i_grads_pf[k] = fe_values[phase_field].gradient (k, q);
		      
		  }
		
		double current_pressure = 0.;
		
		// TODO: check
		if(bDarcy)
		  current_pressure = old_solution_values_pressure[q](0); 
		else
		  current_pressure  =  func_pressure.value(Point<1>(time), 0);
		
	
		// First, we prepare things coming from the previous Newton
		// iteration...
		double pf = old_solution_values[q](dim);
		double old_timestep_pf = old_timestep_solution_values[q](dim);
		double old_old_timestep_pf = old_old_timestep_solution_values[q](dim);

                double pf_minus_old_timestep_pf_plus =
                    std::max(0.0, pf - old_timestep_pf);

		double pf_extra = pf;
		// Linearization by extrapolation to cope with non-convexity of the underlying 
		// energy functional. 
		// This idea might be refined in a future work (be also careful because 
		// theoretically, we do not have time regularity; therefore extrapolation in time
		// might be questionable. But for the time being, this is numerically robust.
		pf_extra = old_old_timestep_pf + (time - (time-old_timestep-old_old_timestep))/
		  (time-old_timestep - (time-old_timestep-old_old_timestep)) * (old_timestep_pf - old_old_timestep_pf);
		if (pf_extra <= 0.0)
		  pf_extra = 0.0;
		if (pf_extra >= 1.0)
		  pf_extra = 1.0;
		

		if (use_old_timestep_pf)
		  pf_extra = old_timestep_pf;

		const Tensor<1,dim> grad_p = Tensors
		  ::get_grad_p<dim> (q, old_solution_grads_pressure);


		const Tensor<1,dim> u = Tensors
		  ::get_u<dim> (q, old_solution_values);

		const Tensor<1,dim> old_timestep_u = Tensors
		  ::get_u<dim> (q, old_timestep_solution_values);
		
		const Tensor<2,dim> grad_u = Tensors 
		  ::get_grad_u<dim> (q, old_solution_grads);
		
		const Tensor<1,dim> grad_pf = Tensors
		  ::get_grad_pf<dim> (q, old_solution_grads);
		
		const Tensor<2,dim> old_timestep_grad_u = Tensors 
		  ::get_grad_u<dim> (q, old_timestep_solution_grads);
		
		const double divergence_u = Tensors 
		  ::get_divergence_u<dim> (grad_u);
		
		const Tensor<2,dim> Identity = Tensors
		  ::get_Identity<dim> ();
		
		const Tensor<2,dim> E = 0.5 * (grad_u + transpose(grad_u));
		const double tr_E = trace(E);
		
		const Tensor<2,dim> stress_term = lame_coefficient_lambda * tr_E * Identity
		  + 2 * lame_coefficient_mu * E;
		
		Tensor<2,dim> stress_term_plus;
		Tensor<2,dim> stress_term_minus;

		// Stress splitting proposed by Amor et al. 2009
		// Works in 2D and 3D
		if(timestep_number > 1 && stress_split_switch == 1 ){ 
		  // DEBUG. Divide Stress Term Plus simple version ...
		  stress_term_plus = 0.;
		  stress_term_plus = ((2./dim)* lame_coefficient_mu +  lame_coefficient_lambda )* std::max(0.,tr_E) * Identity;
		  stress_term_plus += 2. *  lame_coefficient_mu * ( E - (1./dim) * tr_E * Identity);
		  
		  stress_term_minus = 0.;
		  stress_term_minus = ((2./dim)* lame_coefficient_mu +  lame_coefficient_lambda ) 
		    * ( tr_E - std::max(0.,tr_E)) * Identity;

		}
		else{
		  
		  stress_term_plus = lame_coefficient_lambda * tr_E * Identity
		    + 2 * lame_coefficient_mu * E;
		  stress_term_minus = 0; 
		  
		}
		
	      
	      // residual_only ==>  only the RHS. 
	      if (!residual_only)
		for (unsigned int i = 0; i < dofs_per_cell; ++i)
		  {
		    double pf_minus_old_timestep_pf_plus = 0.0;
		    
		    // If only for monolithic.
		    if ((pf - old_timestep_pf) < 0.0)
		      pf_minus_old_timestep_pf_plus = 0.0;
		    else 
		      pf_minus_old_timestep_pf_plus = phi_i_pf[i]; 
		    
		    
		    const Tensor<2, dim> E_LinU = 0.5
		      * (phi_i_grads_u[i] + transpose(phi_i_grads_u[i]));
		    const double tr_E_LinU = trace(E_LinU);

		    const double divergence_u_LinU = Tensors 
		      ::get_divergence_u<dim> (phi_i_grads_u[i]);

		    Tensor<2,dim> stress_term_LinU;
		    stress_term_LinU = lame_coefficient_lambda * tr_E_LinU * Identity
		      + 2 * lame_coefficient_mu * E_LinU;
		    
		    Tensor<2,dim> stress_term_plus_LinU;
		    Tensor<2,dim> stress_term_minus_LinU;
		    
		    const unsigned int comp_i = fe.system_to_component_index(i).first;
	


		    if(timestep_number > 1 && stress_split_switch == 1){
		      double tr_E_positive_LinU = 0.0;
		      if (tr_E < 0.0)
			{
			  tr_E_positive_LinU = 0.0;
			}
		      else
			tr_E_positive_LinU = tr_E_LinU;
		      
		      stress_term_plus_LinU =0.;
		      stress_term_plus_LinU = ((2./dim)* lame_coefficient_mu +  lame_coefficient_lambda )* tr_E_positive_LinU * Identity;
		      stress_term_plus_LinU += 2. *  lame_coefficient_mu * ( E_LinU - (1./dim) * tr_E_LinU * Identity);
		      
		      stress_term_minus_LinU = 0.;
		      stress_term_minus_LinU =  ((2./dim)* lame_coefficient_mu +  lame_coefficient_lambda )
			*( tr_E_LinU - tr_E_positive_LinU ) * Identity;
		      
		    }
		    else
		      {
			stress_term_plus_LinU = lame_coefficient_lambda * tr_E_LinU * Identity
			  + 2 * lame_coefficient_mu * E_LinU;
			stress_term_minus_LinU = 0; 
		      }

		    
		    if (comp_i == dim)
		      {
			stress_term_plus_LinU = 0;
			stress_term_minus_LinU = 0;
		      }

		    for (unsigned int j = 0; j < dofs_per_cell; ++j)
		      {
			const unsigned int comp_j = fe.system_to_component_index(j).first;   
			if (comp_j < dim)
			  {
			    // Solid   M_uu
			    local_matrix(j,i) += 1.0 * 
			      (scalar_product(((1-constant_k) * pf_extra * pf_extra + constant_k) * 	      
					      stress_term_plus_LinU, phi_i_grads_u[j])
			       // stress term minus
			       + scalar_product(stress_term_minus_LinU, phi_i_grads_u[j])				
			       ) * fe_values.JxW(q);
			    
			  }
			else if (comp_j == dim)
			  {
			    // Phase-field
			    local_matrix(j,i) += 
  			       ((1-constant_k) * (scalar_product(stress_term_plus_LinU, E) + scalar_product(stress_term_plus, E_LinU)) * pf * phi_i_pf[j]
			       +(1-constant_k) *  scalar_product(stress_term_plus, E) * phi_i_pf[i] * phi_i_pf[j]
			       + G_c_values[q]/alpha_eps * phi_i_pf[i] * phi_i_pf[j]  
			       + G_c_values[q] * alpha_eps * phi_i_grads_pf[i] * phi_i_grads_pf[j]
			       // Pressure terms
			       - 2.0 * (alpha_biot - 1.0) * current_pressure *
			       (pf * divergence_u_LinU + phi_i_pf[i] * divergence_u) * phi_i_pf[j]			      
			       ) * fe_values.JxW(q);       

			    // Additional pressure term due to Biot System
			      local_matrix(j,i) += 2. * pf * grad_p *  phi_i_u[j] * phi_i_pf[i] * fe_values.JxW(q); 
			      local_matrix(j,i) += 2. * grad_p * u *   phi_i_pf[i] * phi_i_pf[j]  * fe_values.JxW(q); 


			  }
			
			// end j dofs
		      }
		    // end i dofs
		  }
	      
	      
	      // RHS:
	      for (unsigned int i = 0; i < dofs_per_cell; ++i)
		{
		  const unsigned int comp_i = fe.system_to_component_index(i).first;
		  if (comp_i < dim)
		    {
                      const Tensor<1, dim> phi_i_u =
			fe_values[displacements].value(i, q);
                      const Tensor<2, dim> phi_i_grads_u =
			fe_values[displacements].gradient(i, q);		      
		      const double divergence_u_LinU = Tensors 
			::get_divergence_u<dim> (phi_i_grads_u);
		      
                      // Solid
                      local_rhs(i) -= 
                        (scalar_product(((1.0-constant_k) * pf_extra * pf_extra + constant_k) *
					stress_term_plus, phi_i_grads_u)
                         +  scalar_product(stress_term_minus, phi_i_grads_u)
                         // Pressure terms
                         //- (alpha_biot - 1.0) * (current_pressure - initial_pressure_values[q]) * pf_extra * pf_extra * divergence_u_LinU		    
			 - (alpha_biot - 1.0) * (current_pressure) * pf_extra * pf_extra * divergence_u_LinU		    
                         ) * fe_values.JxW(q);

		      // Additional pressure term due to Biot System
			local_rhs(i) -= pf_extra * pf_extra * grad_p * phi_i_u * fe_values.JxW(q);


		      
		    }
		  else if (comp_i == dim)
                    {
                      const double phi_i_pf = fe_values[phase_field].value (i, q);
                      const Tensor<1,dim> phi_i_grads_pf = fe_values[phase_field].gradient (i, q);
		      
                      // Phase field
                      local_rhs(i) -= 
                        ((1.0 - constant_k) * scalar_product(stress_term_plus, E) * pf * phi_i_pf
                         - G_c_values[q]/alpha_eps * (1.0 - pf) * phi_i_pf
                         + G_c_values[q] * alpha_eps * grad_pf * phi_i_grads_pf
                         // Pressure terms
                         //- 2.0 * (alpha_biot - 1.0) * (current_pressure - initial_pressure_values[q]) * pf * divergence_u * phi_i_pf
			 - 2.0 * (alpha_biot - 1.0) * (current_pressure) * pf * divergence_u * phi_i_pf
                         ) * fe_values.JxW(q);

		      // Additional pressure term due to Biot System
			local_rhs(i) -= 2. * pf * grad_p * u  * phi_i_pf * fe_values.JxW(q);

		      

                    }
		  
		} // end i
	      
	      
	      
                  // end n_q_points
	      }
	    
	    
	    cell->get_dof_indices(local_dof_indices);
	    if (residual_only)
	      {
		constraints_update.distribute_local_to_global(local_rhs,
							      local_dof_indices,
							      system_pde_residual);
		
		constraints_update.distribute_local_to_global(local_rhs,
							      local_dof_indices, 
							      system_total_residual);
		
	      } // if residual only
	    else
	      {

		all_constraints.distribute_local_to_global(local_matrix,
							   local_rhs,
							   local_dof_indices,
							   system_pde_matrix,
							   system_pde_residual);
		
	      }
	  } // just a bracket ?
          // end cell

        } // cell locally owned

    if (residual_only)
      system_total_residual.compress(VectorOperation::add);
    else
      system_pde_matrix.compress(VectorOperation::add);

    system_pde_residual.compress(VectorOperation::add);

    if (!direct_solver && !residual_only)
      {	
	{
	  LA::MPI::PreconditionAMG::AdditionalData data;
	  data.constant_modes = constant_modes;
	  data.elliptic = true;
	  data.higher_order_elements = true;
	  data.smoother_sweeps = 2;
	  data.aggregation_threshold = 0.02;
	  preconditioner_solid.initialize(system_pde_matrix.block(0, 0), data);
	}
	{

	  LA::MPI::PreconditionAMG::AdditionalData data;
	  //data.constant_modes = constant_modes;
	  data.elliptic = true;
	  data.higher_order_elements = true;
	  data.smoother_sweeps = 2;
	  data.aggregation_threshold = 0.02;
	  preconditioner_phase_field.initialize(system_pde_matrix.block(1, 1), data);
	}
      }
  }




// In this function we assemble the semi-linear
// of the right hand side of Newton's method (its residual).
// The framework is in principal the same as for the
// system matrix.
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::assemble_nl_residual ()
  {
    assemble_system(true);
  }

template <int dim>
  void
  FracturePhaseFieldProblem<dim>::assemble_diag_mass_matrix ()
{
  diag_mass = 0;

  QGaussLobatto<dim> quadrature_formula(degree + 1);
  //QGauss<dim> quadrature_formula(degree + 2);
  
  FEValues<dim> fe_values(fe, quadrature_formula,
			  update_values | update_quadrature_points | update_JxW_values
			  | update_gradients);
  
  const unsigned int dofs_per_cell = fe.dofs_per_cell;
  
  const unsigned int n_q_points = quadrature_formula.size();

  Vector<double> local_rhs(dofs_per_cell);

  std::vector<unsigned int> local_dof_indices(dofs_per_cell);
  
  typename DoFHandler<dim>::active_cell_iterator cell =
    dof_handler.begin_active(), endc = dof_handler.end();
  
  for (; cell != endc; ++cell)
    if (cell->is_locally_owned())
      {
	fe_values.reinit(cell);
	local_rhs = 0;
	
	for (unsigned int q_point = 0; q_point < n_q_points; ++q_point)
	  for (unsigned int i = 0; i < dofs_per_cell; ++i)
	    {
	      const unsigned int comp_i = fe.system_to_component_index(i).first;
	      if (comp_i != dim)
		continue; // only look at phase field
	      
	      local_rhs (i) += fe_values.shape_value(i, q_point) *
		fe_values.shape_value(i, q_point) * fe_values.JxW(q_point);
	    }
	
	cell->get_dof_indices(local_dof_indices);
	for (unsigned int i=0; i<dofs_per_cell; i++)
	  diag_mass(local_dof_indices[i]) += local_rhs(i);
	
	
      }

  diag_mass.compress(VectorOperation::add);
  diag_mass_relevant = diag_mass;
}



// Here, we impose boundary conditions
// for the system and the first Newton step
template <int dim>
void
FracturePhaseFieldProblem<dim>::set_initial_bc (const double time)
{
  std::map<unsigned int, double> boundary_values;
  std::vector<bool> component_mask(dim+1, false);

  if (test_case == TestCase::hole){
    component_mask[0] = true;
    component_mask[1] = true;
    if(dim == 3)
      component_mask[2] = true;
    
    VectorTools::interpolate_boundary_values(dof_handler, 0,
					     ZeroFunction<dim>(dim+1), boundary_values, component_mask);

    VectorTools::interpolate_boundary_values(dof_handler, 1,
					     ZeroFunction<dim>(dim+1), boundary_values, component_mask);

    component_mask[0] = false;  //u_1
    component_mask[1] = false;  //u_2
    component_mask[2] = true;   //phi
    VectorTools::interpolate_boundary_values(dof_handler, 2,
					     ConstantFunction<dim>(1.,dim+1), boundary_values, component_mask);
        
    component_mask[0] = true;   //u_1
    component_mask[1] = true;   //u_2
    component_mask[2] = false;  //phi
    VectorTools::interpolate_boundary_values(dof_handler, 2,
					     ZeroFunction<dim>(dim+1), boundary_values, component_mask);

  }
  else if (test_case == TestCase::sneddon_2d ||
      test_case == TestCase::sneddon_3d || 
      test_case == TestCase::multiple_homo ||
      test_case == TestCase::multiple_homo_3d ||
      test_case == TestCase::multiple_homo_parallel ||
      test_case == TestCase::multiple_homo_parallel_3d ||
      test_case == TestCase::multiple_het ||
      test_case == TestCase::multiple_het_3d || 
      test_case == TestCase::gupta_3d) 
      {
	component_mask[0] = true;
	component_mask[1] = true;
        if(dim == 3)
          component_mask[2] = true;

	VectorTools::interpolate_boundary_values(dof_handler, 0,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);

// SHLEE	
//	component_mask[0] = true;
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 1,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	
//	component_mask[0] = true;
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	
//	component_mask[0] = true;
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	
	if(test_case == TestCase::sneddon_3d || 
	   test_case == TestCase::multiple_homo_3d || 
	   test_case == TestCase::multiple_homo_parallel_3d ||
	   test_case == TestCase::multiple_het_3d ||
	   test_case == TestCase::gupta_3d){
	  VectorTools::interpolate_boundary_values(dof_handler, 4,
						   ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	  
	  VectorTools::interpolate_boundary_values(dof_handler, 5,
						   ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	}
  }
  else if (test_case == TestCase::miehe_tension)
    {
      // For Miehe 2010 tension test 
      
      component_mask[0] = false;
      component_mask[1] = true;
      component_mask[2] = false;

      VectorTools::interpolate_boundary_values(dof_handler, 2,
					       ZeroFunction<dim>(dim+1), boundary_values, component_mask);
      
      component_mask[0] = true;
      component_mask[1] = true;
      component_mask[2] = false;
      VectorTools::interpolate_boundary_values(dof_handler, 3,
					       BoundaryParabelTension<dim>(time), boundary_values, component_mask);
    }
  else if (test_case == TestCase::miehe_tension_3D)
    {
      // For Miehe 2010 tension test 
      
      component_mask[0] = false;
      component_mask[1] = true;
      component_mask[2] = false;
      component_mask[3] = false;

      // BOTTOM.
      VectorTools::interpolate_boundary_values(dof_handler, 2,
					       ZeroFunction<dim>(dim+1), boundary_values, component_mask);
      
      component_mask[0] = true;
      component_mask[1] = true;
      component_mask[2] = true;
      VectorTools::interpolate_boundary_values(dof_handler, 3,
					       BoundaryParabelTension<dim>(time), boundary_values, component_mask);
    }
  


    else if (test_case == TestCase::miehe_shear)
      {  
	// For Miehe 2010 shear test 
	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 0,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);

	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 1,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);



	component_mask[0] = true;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);
	
	component_mask[0] = true;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 BoundaryParabelShear<dim>(time), boundary_values, component_mask);


	//      bottom part of crack
	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 4,
						 ZeroFunction<dim>(dim+1), boundary_values, component_mask);

      }
 

    std::pair<unsigned int, unsigned int> range;
  
  LA::MPI::BlockVector distributed_solution(partition);
  distributed_solution  = solution;
    
    if (direct_solver)
      {
	// this is not elegant, but works
	std::vector<unsigned int> sub_blocks (dim+1,0);
	sub_blocks[dim] = 1;
	std::vector<types::global_dof_index> dofs_per_block (2);
	DoFTools::count_dofs_per_block (dof_handler, dofs_per_block, sub_blocks);
	const unsigned int n_solid = dofs_per_block[0];
	IndexSet is = distributed_solution.block(0).locally_owned_elements().get_view(0, n_solid);
	Assert(is.is_contiguous(), ExcInternalError());
	range.first = is.nth_index_in_set(0);
	range.second = is.nth_index_in_set(is.n_elements()-1)+1;
      }
    else
      {	
	range = distributed_solution.block(0).local_range();
      }

    
    for (typename std::map<unsigned int, double>::const_iterator i =
	   boundary_values.begin(); i != boundary_values.end(); ++i)
      if (i->first >= range.first && i->first < range.second)
	distributed_solution(i->first) = i->second;
    
    distributed_solution.compress(VectorOperation::insert);
    constraints_hanging_nodes.distribute(distributed_solution); 
    
    
    solution = distributed_solution;

  }




// This function applies boundary conditions
// to the Newton iteration steps. For all variables that
// have Dirichlet conditions on some (or all) parts
// of the outer boundary, we apply zero-Dirichlet
// conditions, now.
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::set_newton_bc ()
  {
    
    std::vector<bool> component_mask(dim+1, false);
    
    if (test_case == TestCase::hole){
      component_mask[0] = true;
      component_mask[1] = true;
      if( dim == 3) 
	component_mask[2] = true; // SHLEE 	
      
      VectorTools::interpolate_boundary_values(dof_handler, 0,
					       ZeroFunction<dim>(dim+1), 
					       constraints_update, component_mask);

      VectorTools::interpolate_boundary_values(dof_handler, 1,
					       ZeroFunction<dim>(dim+1), 
					       constraints_update, component_mask);

      VectorTools::interpolate_boundary_values(dof_handler, 2,
					       ZeroFunction<dim>(dim+1), 
					       constraints_update, component_mask);
    }
      
    
    else if (test_case == TestCase::sneddon_2d ||
	test_case == TestCase::sneddon_3d || 
	test_case == TestCase::multiple_homo ||
	test_case == TestCase::multiple_homo_3d ||
	test_case == TestCase::multiple_homo_parallel ||
	test_case == TestCase::multiple_homo_parallel_3d ||
	test_case == TestCase::multiple_het ||
	test_case == TestCase::multiple_het_3d || 
	test_case == TestCase::gupta_3d)
      {
	component_mask[0] = true;
	component_mask[1] = true;
        if( dim == 3) 
	  component_mask[2] = true; 
	
        VectorTools::interpolate_boundary_values(dof_handler, 0,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);

// DEBUG
// SHLEE	
//	component_mask[0] = true;
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 1,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
	
	
//	component_mask[0] = true; // false
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
	
//	component_mask[0] = true; // false
//	component_mask[1] = true;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
   
	if(test_case == TestCase::sneddon_3d ||
	   test_case == TestCase::multiple_homo_3d ||
	   test_case == TestCase::multiple_homo_parallel_3d ||
	   test_case == TestCase::multiple_het_3d || 
	   test_case == TestCase::gupta_3d){
	  VectorTools::interpolate_boundary_values(dof_handler, 4,
						   ZeroFunction<dim>(dim+1), 
						   constraints_update, component_mask);
	  
	  VectorTools::interpolate_boundary_values(dof_handler, 5,
						   ZeroFunction<dim>(dim+1), 
						   constraints_update, component_mask);
	}

   }
    else if (test_case == TestCase::miehe_tension)
      {
	// Miehe 2010 tension 
	component_mask[0] = true; // false
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
	
	component_mask[0] = true; // false
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
      }
    
    
    else if (test_case == TestCase::miehe_tension_3D)
      {
	// Miehe 2010 tension 
	component_mask[0] = true; // false
	component_mask[1] = true;
        component_mask[2] = true;
        component_mask[3] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
	
	component_mask[0] = true; // false
	component_mask[1] = true;
	component_mask[2] = true;
	component_mask[3] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
      }


    else if (test_case == TestCase::miehe_shear)
      {
	// Miehe 2010 shear
	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 0,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);

	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 1,
						 ZeroFunction<dim>(dim+1),
						 constraints_update, component_mask);


	component_mask[0] = true; 
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 2,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);
	
	component_mask[0] = true; 
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 3,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);

	// bottom part of crack
	component_mask[0] = false;
	component_mask[1] = true;
	component_mask[2] = false;
	VectorTools::interpolate_boundary_values(dof_handler, 4,
						 ZeroFunction<dim>(dim+1), 
						 constraints_update, component_mask);

	
      }
 

  }


template <class PreconditionerA, class PreconditionerC>
class BlockDiagonalPreconditioner
{
public:
  BlockDiagonalPreconditioner(const LA::MPI::BlockSparseMatrix  &M,
      const PreconditionerA &pre_A, const PreconditionerC &pre_C)
: matrix(M),
  prec_A (pre_A),
  prec_C (pre_C)
  {
  }

  void vmult (LA::MPI::BlockVector       &dst,
      const LA::MPI::BlockVector &src) const
  {
    prec_A.vmult(dst.block(0), src.block(0));
    prec_C.vmult(dst.block(1), src.block(1));
  }


  const LA::MPI::BlockSparseMatrix & matrix;
  const PreconditionerA & prec_A;
  const PreconditionerC  & prec_C;
};

// In this function, we solve the linear systems
// inside the nonlinear Newton iteration.
template <int dim>
  unsigned int
  FracturePhaseFieldProblem<dim>::solve ()
  {


    LA::MPI::BlockVector distributed_newton_update(partition);
    distributed_newton_update = newton_update;

    constraints_hanging_nodes.set_zero(distributed_newton_update);
    constraints_hanging_nodes.set_zero(system_pde_residual);

    if (direct_solver)
      {
	SolverControl cn;
	TrilinosWrappers::SolverDirect solver(cn);
	solver.solve(system_pde_matrix.block(0,0), distributed_newton_update.block(0), system_pde_residual.block(0));
	
	all_constraints.distribute(distributed_newton_update);
	newton_update = distributed_newton_update;
	
	return 1;
      }
    else 
      {
	SolverControl solver_control(1000, system_pde_residual.l2_norm() * 1e-8);
	
	SolverGMRES<LA::MPI::BlockVector> solver(solver_control);
	
	BlockDiagonalPreconditioner<LA::MPI::PreconditionAMG,LA::MPI::PreconditionAMG>
	  preconditioner(system_pde_matrix,
			 preconditioner_solid, preconditioner_phase_field);
	
	solver.solve(system_pde_matrix, distributed_newton_update,
		     system_pde_residual, preconditioner);
	
	
	all_constraints.distribute(distributed_newton_update);
	newton_update = distributed_newton_update;
	
	return solver_control.last_step();
      }
  }


template <int dim>
double FracturePhaseFieldProblem<dim>::newton_active_set()
{
  
  pcout << "It.\t#A.Set\tResidual\tReduction\tLSrch\t#LinIts" << std::endl;  

  set_initial_bc(time);  

  assemble_nl_residual();     

  
  LA::MPI::BlockVector residual_relevant(partition_relevant, mpi_com);
  residual_relevant = system_total_residual;  

  double newton_residual = system_pde_residual.l2_norm();
  
  double old_newton_residual = newton_residual;
  
  
  pcout << "0\t\t" << std::scientific << newton_residual << std::endl; 
  std::cout.unsetf(std::ios_base::floatfield);
  
  unsigned int newton_it=0;
  double new_newton_residual = 0.0;
  
  int number_of_active_set = 0;
  int number_of_old_active_set = 0;

  while (true){

    ++newton_it;
    pcout << newton_it << std::flush;  
    
    IndexSet active_set_old = active_set;
    active_set.clear();
    active_set.set_size(dof_handler.n_dofs());

    all_constraints.reinit(relevant_set);
    
    // This only contains the active set. 
    // will use to create \tilde{F} and \tilde{G}
    only_activeset_constraints.reinit(relevant_set);
    
    std::vector<bool> dof_touched(dof_handler.n_dofs(), false);
   
    LA::MPI::BlockVector distributed_solution(partition);
    distributed_solution = solution;
    

    unsigned int owned_active_set_dofs = 0;
    
    std::vector<types::global_dof_index> local_dof_indices(fe.dofs_per_cell);
    
    int cell_index =0 ;
    
    typename DoFHandler<dim>::active_cell_iterator cell =
      dof_handler.begin_active(), endc = dof_handler.end();
    
    for (; cell != endc; ++cell, ++cell_index)
      {
	if (! cell->is_artificial() ) // Following step-42. 
	  //	if(cell->is_locally_owned())  //-> wee see some errors !
	  for (unsigned int i=0;i<fe.dofs_per_cell;++i)
	    {
	      // IMPORTANT ! WE need this. 
	      cell->get_dof_indices(local_dof_indices); 

	      const unsigned int comp_i = fe.system_to_component_index(i).first;
	      const unsigned int idx = local_dof_indices[i];
	      
	      if (comp_i != dim)
		continue; // only look at phase field
	      
	      double old_value = old_solution(idx);
	      double new_value = solution(idx);
	      
	      if (dof_touched[idx]==true  || constraints_hanging_nodes.is_constrained(idx))
		continue;
	      
	      dof_touched[idx] = true;
	      
	      
	      double c= 1e+1 * E_modulus;
	      double massm = diag_mass_relevant(idx);
	      
	      double gap = new_value - old_value;
	      
	      // Now we are in the Inactive Set.
	      // TODO/Debug
	      // The active set tolerance should be 0 but 
	      // weakening this condition seems to avoid 
	      // active set cycles
	      double active_set_tolerance = 1e-10;
	      if ( residual_relevant(idx)/massm + c * (gap) <= active_set_tolerance)
		continue;
	      
	      // now idx is in the active set
	      // NOTE ** 
	      // ** Important 
	      // Only 'active set' constraint is here applied to 
	      // the solution, NOT, anything else..
	      all_constraints.add_line(idx);
	      all_constraints.set_inhomogeneity(idx, 0.0);
	      
	      //DEBUG
	      only_activeset_constraints.add_line(idx);
	      only_activeset_constraints.set_inhomogeneity(idx, 0.0);
	      // Active Set, we just take the old_values.
	      distributed_solution(idx) = old_value;
	      
	      active_set.add_index(idx);
	      
	      if (dof_handler.locally_owned_dofs().is_element(idx))
		++owned_active_set_dofs;


	    } // for i
      } // for cell
    

    distributed_solution.compress(VectorOperation::insert);
    solution=distributed_solution;
    
    all_constraints.close();
    only_activeset_constraints.close();
 

    all_constraints.merge(constraints_update); 
    
    
    pcout << "\t"
	  << Utilities::MPI::sum(owned_active_set_dofs, mpi_com)
	  << std::flush;

    number_of_old_active_set = number_of_active_set;
    number_of_active_set  =  Utilities::MPI::sum(owned_active_set_dofs, mpi_com) ;          

    int is_my_set_changed = (active_set == active_set_old) ? 0 : 1;
    
    int num_changed = Utilities::MPI::sum(is_my_set_changed,
					  MPI_COMM_WORLD);
    
    // Only assemble the residual
    assemble_system(false);
  

    unsigned int no_linear_iterations = solve();
    LA::MPI::BlockVector saved_solution(partition_relevant);
    saved_solution  = solution;

    unsigned int line_search_step = 0;
    
    for (; line_search_step < max_no_line_search_steps; ++line_search_step)
      {
	
	LA::MPI::BlockVector distributed_newton_update(partition);
	distributed_newton_update = newton_update;
	LA::MPI::BlockVector distributed_solution(partition);
	distributed_solution = solution;

	distributed_solution.add(distributed_newton_update);
        solution = distributed_solution; 	


	assemble_nl_residual();
	residual_relevant = system_total_residual;
	
	LA::MPI::BlockVector residual_norm(partition);
	residual_norm = system_pde_residual;
	

	if(direct_solver){
	  cout<<"ERROR : need to think about being efficient with direct solver here "<< endl;
	  exit(0);
	}
	
	unsigned int start_disp = (residual_norm.block(0).local_range().first);
	unsigned int end_disp = (residual_norm.block(0).local_range().second);
	
	end_disp = Utilities::MPI::max(end_disp, mpi_com); 

	unsigned int start_res = (residual_norm.block(1).local_range().first);
	unsigned int end_res = (residual_norm.block(1).local_range().second);
	
	start_res += end_disp;
	end_res += end_disp;

	unsigned int n_number_count= 0;
	for (unsigned int n = start_res; n < end_res; ++n){
	  if (only_activeset_constraints.is_constrained(n)){
	    residual_norm(n) = 0;
	    ++n_number_count;
	  }
	}

	// Sanity check
 	if(n_number_count != owned_active_set_dofs){
	  cout<<" HERE we have WRONG numbers !! " << endl;
	  cout<< n_number_count << endl;
	  cout<< owned_active_set_dofs << endl;	  
	  exit(0);
	}
	

	residual_norm.compress(VectorOperation::insert);
	new_newton_residual = residual_norm.l2_norm();


	if (new_newton_residual < newton_residual)
	  break;
	
	solution = saved_solution;
	distributed_newton_update *= line_search_damping;
	
	newton_update = distributed_newton_update;
	
      }//for line search


      pcout << std::scientific
	    << "\t" << new_newton_residual
	    << "\t" << new_newton_residual/newton_residual;
      std::cout.unsetf(std::ios_base::floatfield);
      pcout << "\t" << line_search_step
	    << "\t" << no_linear_iterations
    	  << std::endl;
      
    old_newton_residual = newton_residual;
    newton_residual = new_newton_residual;
    
    if (newton_residual < lower_bound_newton_residuum  && num_changed == 0 )
      {
	pcout << '\t' 
	      << std::scientific 
	      << Utilities::MPI::sum(owned_active_set_dofs, mpi_com)
	      << '\t' 
	      << newton_residual << " < TOL(Displ.-PFF)"<<std::endl;
            break;
        }
      
    // We stop Newton when the tolerance is very low and 
    // active set just changes a bit
    double tmp_tol = 1.0e-15;
    if(dim == 3) 
      tmp_tol = 1.0e-8;
    if (newton_residual < tmp_tol)
        {
	pcout << '\t' 
	      << std::scientific 
	      << Utilities::MPI::sum(owned_active_set_dofs, mpi_com)
	      << '\t' 
	      << newton_residual << " < TOL(Displ.-PFF)"<<std::endl;
          break;
        }
      
    if (newton_it>=max_no_newton_steps)
        {
	pcout << "Newton iteration did not converge in " << newton_it
		<< " steps." << std::endl;
          throw SolverControl::NoConvergence(0,0);
        }
      
      
  } // end while

  return new_newton_residual/old_newton_residual;

	

	  }




    
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::project_back_phase_field ()
  {


  LA::MPI::BlockVector distributed_solution(partition, mpi_com);
  distributed_solution = solution;

    typename DoFHandler<dim>::active_cell_iterator cell =
                       dof_handler.begin_active(), endc = dof_handler.end();

    std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
    for (; cell != endc; ++cell)
      if (cell->is_locally_owned())
        {
          cell->get_dof_indices(local_dof_indices);
          for (unsigned int i=0;i<fe.dofs_per_cell;++i)
            {
              const unsigned int comp_i = fe.system_to_component_index(i).first;
              if (comp_i != dim)
                continue; // only look at phase field

              const unsigned int idx = local_dof_indices[i];
              if (!dof_handler.locally_owned_dofs().is_element(idx))
                continue;

              distributed_solution(idx) = std::max(0.0,
                        std::min(static_cast<double>(solution(idx)), 1.0));
            }
        }


    constraints_hanging_nodes.distribute(distributed_solution);
    distributed_solution.compress(VectorOperation::insert);
    solution = distributed_solution; 
  }



template <int dim>
void  FracturePhaseFieldProblem<dim>::compute_stress_per_cell ()
{
  QGauss<dim> quad_gauss(degree+2);
  FEValues<dim> fe_values (fe, quad_gauss, update_gradients | update_quadrature_points);			   
  const unsigned int  n_q_points  = quad_gauss.size();

  std::vector<std::vector<Tensor<1, dim> > > solution_grads(n_q_points, std::vector<Tensor<1, dim> >(dim+1));

  typename DoFHandler<dim>::active_cell_iterator
    cell = dof_handler.begin_active(),
    endc = dof_handler.end();
  
  unsigned int cell_counter;
  
  solution_stress_per_cell.reinit(triangulation.n_active_cells());

  // This is the identity matrix in two dimensions:
  const Tensor<2,dim> Identity = Tensors::get_Identity<dim> ();
  
  
  for (unsigned int cell_counter = 0; cell!=endc; ++cell, ++cell_counter)
    if (cell->is_locally_owned())
      {
	fe_values.reinit (cell);
	fe_values.get_function_gradients (solution, solution_grads);
	
	double norm_stress_term = 0.0;
	double norm = 0.0;
	for (unsigned int q=0; q<n_q_points; ++q)
	  {
	    const Tensor<2,dim> grad_u = Tensors
	      ::get_grad_u<dim> (q, solution_grads);
	    
	    const Tensor<2,dim> E = 0.5 * (grad_u + transpose(grad_u));
	    const double tr_E = grad_u[0][0] + grad_u[1][1];
	    
	    Tensor<2,dim> stress_term;
	    stress_term.clear();
	    stress_term = lame_coefficient_lambda * tr_E * Identity +  2 * lame_coefficient_mu * E;
	    norm_stress_term += Tensors::get_deviator_norm<dim> (stress_term);
	    
	  }
	
	solution_stress_per_cell(cell_counter) = (norm_stress_term / n_q_points);
	
	
      }
  
}



//////////////////
template <int dim>void FracturePhaseFieldProblem<dim>::output_results () const
{ 
  /*
  {
    //cout << "Pressure Values along the line = " << endl;
    double num = 100;
    double initial_radius = 0.2;
    double step = (initial_radius/num);

    double center_x = 2.;
    double center_y = 2.;

  std::ostringstream filename2;
    filename2 <<  "pressure-" << Utilities::int_to_string(timestep_number, 5) << "b.txt";
  std::ofstream f(filename2.str().c_str());


    for(int n=0; n<num+1; n++){
    double tmp = 0.;
      tmp = center_x + step * n;
      f <<  tmp  << " " << center_y << " "  <<  VectorTools::point_value
      (dof_handler_pressure,
       solution_pressure.block(0),
	 Point<2>(tmp, center_y)) <<endl;
     }
}
  */
  static int refinement_cycle=-1;
  ++refinement_cycle;

//////////////////

 const FESystem<dim> joint_fe (fe, 1,
			       fe_pressure, 1,
			       fe_level_set, 1,
			       fe_width, 1);
  DoFHandler<dim> joint_dof_handler (triangulation);
  joint_dof_handler.distribute_dofs (joint_fe);

  Assert (joint_dof_handler.n_dofs() ==
      dof_handler.n_dofs() 
	  + dof_handler_pressure.n_dofs() 
	  + dof_handler_level_set.n_dofs() 
	  + dof_handler_width.n_dofs(),
      ExcInternalError());

  LA::MPI::Vector joint_solution;
  joint_solution.reinit(joint_dof_handler.locally_owned_dofs(),mpi_com);



  {
    std::vector<unsigned int> local_joint_dof_indices (joint_fe.dofs_per_cell);
    std::vector<unsigned int> local_dof_indices_solid_pff (fe.dofs_per_cell);
    std::vector<unsigned int> local_dof_indices_pressure (fe_pressure.dofs_per_cell);
    std::vector<unsigned int> local_dof_indices_level_set (fe_level_set.dofs_per_cell);
    std::vector<unsigned int> local_dof_indices_width (fe_width.dofs_per_cell);

    typename DoFHandler<dim>::active_cell_iterator
      joint_cell       = joint_dof_handler.begin_active(),
      joint_endc       = joint_dof_handler.end(),
      cell_solid_pff       = dof_handler.begin_active(),
      cell_pressure     = dof_handler_pressure.begin_active(),
      cell_level_set     = dof_handler_level_set.begin_active(),
      cell_width    = dof_handler_width.begin_active();

    for (; joint_cell!=joint_endc; ++joint_cell, ++cell_solid_pff, ++cell_pressure, ++cell_level_set, ++cell_width)
        if (!joint_cell->is_artificial() && !joint_cell->is_ghost())
{
          joint_cell->get_dof_indices (local_joint_dof_indices);
          cell_solid_pff->get_dof_indices (local_dof_indices_solid_pff);
          cell_pressure->get_dof_indices (local_dof_indices_pressure);
	  cell_level_set->get_dof_indices (local_dof_indices_level_set);
	  cell_width->get_dof_indices (local_dof_indices_width);
  
          for (unsigned int i=0; i<joint_fe.dofs_per_cell; ++i)
            if (joint_fe.system_to_base_index(i).first.first == 0)
              {
                Assert (joint_fe.system_to_base_index(i).second
                        <
                        local_dof_indices_solid_pff.size(),
                        ExcInternalError());
                joint_solution(local_joint_dof_indices[i])
                  = solution(local_dof_indices_solid_pff[joint_fe.system_to_base_index(i).second]);
              }
            else if (joint_fe.system_to_base_index(i).first.first == 1)
              {
                Assert (joint_fe.system_to_base_index(i).first.first == 1,
                        ExcInternalError());
                Assert (joint_fe.system_to_base_index(i).second
                        <
                        local_dof_indices_pressure.size(),
                        ExcInternalError());
                joint_solution(local_joint_dof_indices[i])
                  = solution_pressure(local_dof_indices_pressure[joint_fe.system_to_base_index(i).second]);
              }
	    else if (joint_fe.system_to_base_index(i).first.first == 2)
	      {
		Assert (joint_fe.system_to_base_index(i).first.first == 2,
                        ExcInternalError());
                Assert (joint_fe.system_to_base_index(i).second
                        <
                        local_dof_indices_level_set.size(),
                        ExcInternalError());
                joint_solution(local_joint_dof_indices[i])
                  = solution_level_set(local_dof_indices_level_set[joint_fe.system_to_base_index(i).second]);

	      }
	    else
	      {
		Assert (joint_fe.system_to_base_index(i).first.first == 3,
                        ExcInternalError());
                Assert (joint_fe.system_to_base_index(i).second
                        <
                        local_dof_indices_level_set.size(),
                        ExcInternalError());
                joint_solution(local_joint_dof_indices[i])
                  = solution_width(local_dof_indices_width[joint_fe.system_to_base_index(i).second]);

	      }
        }
    }

   IndexSet locally_relevant_joint_dofs(joint_dof_handler.n_dofs());
   DoFTools::extract_locally_relevant_dofs (joint_dof_handler, locally_relevant_joint_dofs);
    
   TrilinosWrappers::MPI::Vector locally_relevant_joint_solution;
   locally_relevant_joint_solution.reinit (locally_relevant_joint_dofs, mpi_com);
   locally_relevant_joint_solution = joint_solution;
    
  
    std::vector<std::string> solution_names;
    solution_names.push_back("dis_x");
    solution_names.push_back("dis_y");

    if(dim == 3)
      solution_names.push_back("dis_z");

     solution_names.push_back("phase_field");
     solution_names.push_back("pressure");
     solution_names.push_back("level_set");
     solution_names.push_back("width_fe");

    std::vector<DataComponentInterpretation::DataComponentInterpretation> 
      data_component_interpretation(dim+4, DataComponentInterpretation::component_is_scalar);
    
  DataOut<dim> data_out;
  data_out.attach_dof_handler(joint_dof_handler);
  data_out.add_data_vector(locally_relevant_joint_solution,
                 solution_names,
                            DataOut<dim>::type_dof_data,
                            data_component_interpretation);
  
  
  Vector<float> e_mod(triangulation.n_active_cells());
  if (test_case == TestCase::multiple_het || test_case == TestCase::multiple_het_3d)
    {
      typename DoFHandler<dim>::active_cell_iterator cell =
                               dof_handler.begin_active(), endc = dof_handler.end();
      
            std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
            unsigned int cellindex = 0;
            for (; cell != endc; ++cell, ++cellindex)
              if (cell->is_locally_owned())
                {

		  double x = cell->center()[0];
		  double y = cell->center()[1];
		  //		  double z = cell->center()[2];
		  
		  //Point< dim, double >::Point(x,y,z); 	
		  /*
		  if( test_case == TestCase::multiple_het_3d ){
		    if( z >= 0 && z <= 0.25  || 
			z >= 2 && z <= 2.25  ||
			z >= 3 && z <= 3.25  ){
		      e_mod(cellindex) = 1.0 + func_emodulus_0->value(Point<dim>(x,y,z), 0) ;		  
		    }
		    else if( z > 0.25 && z <= 0.5 
			     || z > 2.25 && z <= 2.5
			     || z > 3.25 && z <= 3.5  ){
		      e_mod(cellindex) = 1.0 + func_emodulus_1->value(Point<dim>(x,y,z), 0) ;
		    }
		    else if( z > 0.5 && z <= 0.75 || 
			     z > 1.5 && z <= 1.75 ||
			     z > 3.5 && z <= 3.75 ){
		      e_mod(cellindex) = 1.0 + func_emodulus_2->value(Point<dim>(x,y,z), 0) ;
		    }
		    else if( z > 0.75 && z <= 1. ||
			     z > 1.75 && z <= 2. ||
			     z > 3.75 && z <= 4.){
		      e_mod(cellindex) = 1.0 + func_emodulus->value(Point<dim>(x,y,z), 0) ;
		    }
		    else if( z >= 1 && z <= 1.25  ||
			     z > 2.5 && z <= 2.75  ){
		      e_mod(cellindex) = 1.0 + func_emodulus_3->value(Point<dim>(x,y,z), 0) ;
		    }
		    else if( z > 1.25 && z <= 1.5 ||
			     z > 2.75 && z <= 3. ){
		      e_mod(cellindex) = 1.0 + func_emodulus_4->value(Point<dim>(x,y,z), 0) ;
		    }
		    
		  }
		  */
		  //else{
		    e_mod(cellindex) = 1.0 + func_emodulus->value(cell->center(), 0);
		    //}

                  //e_mod(cellindex) = 1.0 + func_emodulus->value(cell->center(), 0);
                }
	    data_out.add_data_vector(e_mod, "emodulus");
    }
  
  
  data_out.add_data_vector(solution_stress_per_cell, "level_set_width");
  data_out.add_data_vector(solution_fluid_or_structure_mat, "point_wise_width");
  data_out.add_data_vector(solution_fracture_zone, "fracture_zone");
  data_out.add_data_vector(permeability_per_cell, "permeability");
 
  Vector<float> subdomain(triangulation.n_active_cells());
  for (unsigned int i = 0; i < subdomain.size(); ++i)
    subdomain(i) = triangulation.locally_owned_subdomain();
  data_out.add_data_vector(subdomain, "subdomain");
  
  if (outer_solver == OuterSolverType::active_set)
    data_out.add_data_vector(dof_handler, active_set,
			     "active_set");
  
  data_out.build_patches();
  
  // Filename basis comes from parameter file
  std::ostringstream filename;
      
  pcout << "Write solution " << refinement_cycle << std::endl;
 
  filename << "output/"
	   << filename_basis
       << Utilities::int_to_string(refinement_cycle, 5)
	   << "."
       << Utilities::int_to_string(triangulation.locally_owned_subdomain(), 4)
	   << ".vtu";
  
  std::ofstream output(filename.str().c_str());
  data_out.write_vtu(output);
  
 
    if (Utilities::MPI::this_mpi_process(mpi_com) == 0)
      {
        std::vector<std::string> filenames;
        for (unsigned int i = 0; i < Utilities::MPI::n_mpi_processes(mpi_com);
	     ++i)
          filenames.push_back(
                  filename_basis + Utilities::int_to_string(refinement_cycle, 5)
                  + "." + Utilities::int_to_string(i, 4) + ".vtu");

        std::ofstream master_output(
                    ("output/" + filename_basis + Utilities::int_to_string(refinement_cycle, 5)
				     + ".pvtu").c_str());
        data_out.write_pvtu_record(master_output, filenames);
	
        std::string visit_master_filename = ("output/" + filename_basis
                         + Utilities::int_to_string(refinement_cycle, 5) + ".visit");
        std::ofstream visit_master(visit_master_filename.c_str());
        data_out.write_visit_record(visit_master, filenames);
	
        static std::vector<std::vector<std::string> > output_file_names_by_timestep;
        output_file_names_by_timestep.push_back(filenames);
        std::ofstream global_visit_master("output/solution.visit");
        data_out.write_visit_record(global_visit_master,
				    output_file_names_by_timestep);
      }


 cout<<"Finished-Output"<< endl;

}



// With help of this function, we extract 
// point values for a certain component from our
// discrete solution. We use it to gain the 
// displacements of the solid in the x- and y-directions.
template <int dim>
  double
  FracturePhaseFieldProblem<dim>::compute_point_value (
      const DoFHandler<dim> & dofh, const LA::MPI::BlockVector & vector,
      const Point<dim> &p, const unsigned int component) const
  {
    double value = 0.0;
    try
      {
        Vector<double> tmp_vector(dim+1);
        VectorTools::point_value(dofh, vector, p, tmp_vector);
        value = tmp_vector(component);
      }
    catch (typename VectorTools::ExcPointNotAvailableHere e)
      {
	value = 0.;
      }

    return  value; //Utilities::MPI::sum(value, mpi_com);
  }





template <int dim>
  double
  FracturePhaseFieldProblem<dim>::compute_cod (
      const double eval_line)
  {

    const QGauss<dim - 1> face_quadrature_formula(3);
    FEFaceValues<dim> fe_face_values(fe, face_quadrature_formula,
        update_values | update_quadrature_points | update_gradients
            | update_normal_vectors | update_JxW_values);


    const unsigned int dofs_per_cell = fe.dofs_per_cell;
    const unsigned int n_face_q_points = face_quadrature_formula.size();

    std::vector<unsigned int> local_dof_indices(dofs_per_cell);

    std::vector<Vector<double> > face_solution_values(n_face_q_points,
        Vector<double>(dim+1));
    std::vector<std::vector<Tensor<1, dim> > > face_solution_grads(
        n_face_q_points, std::vector<Tensor<1, dim> >(dim+1));

    double cod_value = 0.0;
    double cod_value_synt_phi = 0.0;
    double eps = 1.0e-6;

    typename DoFHandler<dim>::active_cell_iterator cell =
        dof_handler.begin_active(), endc = dof_handler.end();

     for (; cell != endc; ++cell)
      if (cell->is_locally_owned())
        {
          for (unsigned int face = 0; face < GeometryInfo<dim>::faces_per_cell;
              ++face)
            {
              fe_face_values.reinit(cell, face);

              fe_face_values.get_function_values(solution,
                  face_solution_values);
              fe_face_values.get_function_gradients(solution,
                  face_solution_grads);

              for (unsigned int q_point = 0; q_point < n_face_q_points;
                  ++q_point)
                {
                  if ((fe_face_values.quadrature_point(q_point)[0]
                      < (eval_line + eps))
                      && (fe_face_values.quadrature_point(q_point)[0]
                          > (eval_line - eps)))
                    {
                      const Tensor<1, dim> u = Tensors::get_u<dim>(
                          q_point, face_solution_values);

                      const Tensor<1, dim> grad_pf =
                          Tensors::get_grad_pf<dim>(q_point,
                              face_solution_grads);

                      // Motivated by Bourdin et al. (2012); SPE Paper
                      cod_value += 0.5 * u * grad_pf
                          * fe_face_values.JxW(q_point);

                    }

                }
            }
        }

    cod_value = Utilities::MPI::sum(cod_value, mpi_com) / 2.0;

    pcout << eval_line << "  " << cod_value << std::endl;

    return cod_value;

  }


// TODO: should already hold for 3d
template <int dim>
double
FracturePhaseFieldProblem<dim>::compute_energy()
{
  // What are we computing? In Latex-style it is:
  // bulk energy = [(1+k)phi^2 + k] psi(e)
  // crack energy = \frac{G_c}{2}\int_{\Omega}\Bigl( \frac{(\varphi - 1)^2}{\eps}
  //+ \eps |\nabla \varphi|^2 \Bigr) \, dx
  double local_bulk_energy = 0.0;
  double local_crack_energy = 0.0;

  QGaussLobatto<dim> quadrature_formula(degree + 1);
  const unsigned int n_q_points = quadrature_formula.size();

  FEValues<dim> fe_values(fe, quadrature_formula,
      update_values | update_quadrature_points | update_JxW_values
          | update_gradients);

  typename DoFHandler<dim>::active_cell_iterator cell =
      dof_handler.begin_active(), endc = dof_handler.end();

  const unsigned int dofs_per_cell = fe.dofs_per_cell;

  std::vector<double>           x_values(n_q_points);
  std::vector<double>           y_values(n_q_points);

  std::vector<Vector<double> > solution_values(n_q_points,
					       Vector<double>(dim+1));
  
  std::vector<std::vector<Tensor<1, dim> > > solution_grads(
        n_q_points, std::vector<Tensor<1, dim> >(dim+1));

  

  // to find the radius of the crack GUPTA 
  double min_radius_x = 10000000;
  double max_radius_x = -1;
  
  Function_X<dim> function_x;
  Function_Y<dim> function_y;
  
  
  for (; cell != endc; ++cell)
    if (cell->is_locally_owned())
      {
        fe_values.reinit(cell);

	if(test_case == TestCase::gupta_3d){
	  function_x.value_list(fe_values.get_quadrature_points(), x_values);
	  function_y.value_list(fe_values.get_quadrature_points(), y_values);
	}
	// update lame coefficients based on current cell
        if (test_case == TestCase::multiple_het || test_case == TestCase::multiple_het_3d)
          {
            E_modulus = func_emodulus->value(cell->center(), 0);

            lame_coefficient_mu = E_modulus / (2.0 * (1 + poisson_ratio_nu));

            lame_coefficient_lambda = (2 * poisson_ratio_nu * lame_coefficient_mu)
                   / (1.0 - 2 * poisson_ratio_nu);
          }

        fe_values.get_function_values(solution, solution_values);
        fe_values.get_function_gradients(solution, solution_grads);

        for (unsigned int q = 0; q < n_q_points; ++q)
          {
            const Tensor<1, dim> u = Tensors::get_u<dim>(
                q, solution_values);

            const Tensor<2,dim> grad_u = Tensors
                ::get_grad_u<dim> (q, solution_grads);

            const Tensor<2,dim> E = 0.5 * (grad_u + transpose(grad_u));
            const double tr_E = trace(E);

            const double pf = solution_values[q](dim);

	    // FIND THE RADIUS OF THE CRACK  ( PENNY ) 
	    if(test_case == TestCase::gupta_3d){

	      if( pf < 0.1 ){  
		if(max_radius_x < x_values[q] ){  
		  max_radius_x = x_values[q];
		}
		if(min_radius_x > x_values[q] ){
		  min_radius_x = x_values[q];
		}

              }
	      
	    }
	    
            const double tr_e_2 = trace(E*E);
	    
            const double psi_e = 0.5 * lame_coefficient_lambda * tr_E*tr_E + lame_coefficient_mu * tr_e_2;
	    
            local_bulk_energy += ((1+constant_k)*pf*pf+constant_k) * psi_e * fe_values.JxW(q);
	    
	    local_crack_energy += G_c/2.0 * ((pf-1) * (pf-1)/alpha_eps + alpha_eps * scalar_product(grad_u, grad_u))
	      * fe_values.JxW(q);
	    
	    
          }// for q
	
	
      }
  
  double bulk_energy = Utilities::MPI::sum(local_bulk_energy, mpi_com);
  double crack_energy = Utilities::MPI::sum(local_crack_energy, mpi_com);

  double total_max = 0.;
  double total_min = 0.;
  total_max =  Utilities::MPI::max(max_radius_x, mpi_com); 

  MPI_Allreduce(&min_radius_x ,&total_min ,1,MPI_DOUBLE,MPI_MIN, mpi_com );
  
  
  pcout << "No " << timestep_number << " time " << time
	<< " bulk energy: " << bulk_energy 
	<< " crack energy: " << crack_energy 
    //       << " pressure : " <<  func_pressure.value(Point<1>(time), 0) 
	<< endl;


  double U_Z;
  double tmp1,tmp2,tmp3;
  LA::MPI::BlockVector vector(partition_relevant);
  vector = solution;
  

  //if(test_case == TestCase::gupta_3d){    
    {
    //double center = 0.0+min_cell_diameter;
    
    //U_Z =  compute_point_value(dof_handler,vector,Point<dim>(center, center + min_cell_diameter, center), 1);
    //tmp1 = compute_point_value(dof_handler,vector,Point<dim>(center, center + alpha_eps, center), 1);
    
    
    //pcout << center + min_cell_diameter 
//	  <<" "
//	  << endl;
    
    d_radius = 0.5 *( total_max - total_min);    
    
    pcout << "X RADIUS = " << d_radius  << endl;
    /*
    if(d_radius > 1.){

      timer.print_summary ();
      cout<<"Exceed the desired radius"<< endl;
      exit(0);
     } 
    */
    /*
    bool print_flag = false;
    
    if(U_Z != 0 || tmp1 != 0)
      print_flag = true;
    
    if(print_flag == true ){
      cout<< time 
	  << "  U_z= "<< U_Z 
	  << " " << tmp1 << endl;
      
      outputFile_2 << time 
		   << " " << U_Z 
		   << " " << tmp1  << endl;
      
    }
*/
  }
  
      

  Vector<float> difference_per_cell_p (triangulation.n_locally_owned_active_cells());
  VectorTools::integrate_difference (dof_handler_pressure,
				     solution_pressure.block(0),
				     ZeroFunction<dim>(1),					     
				     difference_per_cell_p,
				     QGauss<dim>(3),	
	                             VectorTools::Linfty_norm);
  double Linfty_p = difference_per_cell_p.linfty_norm();
  double high_pressure = 0.;
  high_pressure = Utilities::MPI::max(Linfty_p, mpi_com);


  pcout << "P_Linfty: " << time << "   " << high_pressure << endl;

  /*
  outputFile << time << " " << bulk_energy <<" " << crack_energy << " " 
	     << high_pressure
	     << endl;
  */


  

  return 0;

}

// Here, we compute the four quantities of interest:
// the x and y-displacements of the structure, the drag, and the lift.
template <int dim>
void
FracturePhaseFieldProblem<dim>::compute_functional_values ()
{
  if (dim == 2)
    {
      double lines[] =
	{ 1.0, 1.5, 1.75, 1.78125, 1.8125, 1.84375, 1.875, 1.9375, 2.0, 2.0625, 2.125, 2.15625,
	  2.1875, 2.21875, 2.25, 2.5, 3.0 };
      const unsigned int n_lines = sizeof(lines) / sizeof(*lines);
     /* 
      static unsigned int no = 0;
      ++no;
      std::ostringstream filename;
      filename <<  "cod-" << Utilities::int_to_string(no, 5) << "b.txt";
      pcout << "writing " << filename.str() << std::endl;
      
      std::ofstream f(filename.str().c_str());
      for (unsigned int i = 0; i < n_lines; ++i)
	{
	  double value = compute_cod(lines[i]);
	  f << lines[i] << " " << value << std::endl;
	}
      */
    }
  else if (dim == 3)
    {
      // TODO: Rough and dirty approximation of the 
      // crack opening displacements in 3d
      // We just measure the point values u_y a bit (min_cell_diameter) 
      // above the fracture and use this as approximation

      double y_and_eps = 5.0 + min_cell_diameter;
      
      LA::MPI::BlockVector vector(partition_relevant);
      vector = solution;
      
      double points[] =
	{3., 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75, 5.0, 5.25, 5.5, 5.75, 6.0, 6.25, 6.5, 6.75, 7.};
      const unsigned int n_points = sizeof(points) / sizeof(*points);
      std::vector<unsigned int> cod_values (n_points);

      for (unsigned int i = 0; i < n_points; ++i)
	{
	  double value = compute_point_value(dof_handler,vector,Point<dim>(points[i],  y_and_eps, 5.0), 1);
	  cout << points[i] << " " << value << std::endl;
	  outputFile_2 << points[i] << " " << value << std::endl;
	}
    
     
      
    }
  
  
}


// TODO: still 2d!!!
template <int dim>
void
FracturePhaseFieldProblem<dim>::compute_load ()
  {
    const QGauss<dim-1> face_quadrature_formula (3);
    FEFaceValues<dim> fe_face_values (fe, face_quadrature_formula,
				      update_values | update_gradients | update_normal_vectors |
				      update_JxW_values);

    const unsigned int dofs_per_cell = fe.dofs_per_cell;
    const unsigned int n_face_q_points = face_quadrature_formula.size();

    std::vector<unsigned int> local_dof_indices (dofs_per_cell);

    std::vector<std::vector<Tensor<1,dim> > >
      face_solution_grads (n_face_q_points, std::vector<Tensor<1,dim> > (dim+1));

    Tensor<1,dim> load_value;

    const Tensor<2, dim> Identity =
      Tensors::get_Identity<dim>();

    typename DoFHandler<dim>::active_cell_iterator
      cell = dof_handler.begin_active(),
      endc = dof_handler.end();

    for (; cell!=endc; ++cell)
      if (cell->is_locally_owned())
    {
       for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face)
	 if (cell->face(face)->at_boundary() &&
	     cell->face(face)->boundary_indicator()==3)
	   {
	     fe_face_values.reinit (cell, face);
	     fe_face_values.get_function_gradients (solution, face_solution_grads);

	     for (unsigned int q_point=0; q_point<n_face_q_points; ++q_point)
	       {
                  const Tensor<2, dim> grad_u
		    = Tensors::get_grad_u<dim>(q_point, face_solution_grads);

		 const Tensor<2, dim> E = 0.5 * (grad_u + transpose(grad_u));
		 const double tr_E = trace(E);

		 Tensor<2, dim> stress_term;
		 stress_term = lame_coefficient_lambda * tr_E * Identity
		   + 2 * lame_coefficient_mu * E;

		 load_value +=  stress_term *
		   fe_face_values.normal_vector(q_point)* fe_face_values.JxW(q_point);

	       }
	   } // end boundary 3 for structure


     }

    
    load_value[0] *= -1.0;

    if (test_case == TestCase::miehe_tension || test_case == TestCase::miehe_tension_3D)
      {
	pcout << "  Load y: " << Utilities::MPI::sum(load_value[1], mpi_com) << std::endl;
	outputFile <<" " <<  time << " " << Utilities::MPI::sum(load_value[1], mpi_com) << std::endl;
      }
    else if (test_case == TestCase::miehe_shear)
      {
	pcout << "  Load x: " << Utilities::MPI::sum(load_value[0], mpi_com) << std::endl;
	outputFile <<" " <<  time << " " << Utilities::MPI::sum(load_value[0], mpi_com) << std::endl;
      }
  }


// Determine the phase-field regularization parameters
// eps and kappa
template <int dim>
  void
  FracturePhaseFieldProblem<dim>::determine_mesh_dependent_parameters()
{
  min_cell_diameter = 1.0e+300;
 
  // Option 1 (without adaptivity)
  /*
   {
   typename DoFHandler<dim>::active_cell_iterator cell =
       dof_handler.begin_active(), endc = dof_handler.end();

   for (; cell != endc; ++cell)
     if (cell->is_locally_owned())
       {
         min_cell_diameter = std::min(cell->diameter(), min_cell_diameter);
       }

   min_cell_diameter = -Utilities::MPI::max(-min_cell_diameter, mpi_com); 
   }
  */

  // Option 2:
   // for this test we want to use the h that will be used at the end
   if (test_case == TestCase::miehe_tension 
       || test_case == TestCase::miehe_tension_3D
       || test_case == TestCase::miehe_shear
       || test_case == TestCase::multiple_homo
       || test_case == TestCase::multiple_homo_3d
       || test_case == TestCase::multiple_homo_parallel
       || test_case == TestCase::multiple_homo_parallel_3d
       || test_case == TestCase::multiple_het
       || test_case == TestCase::multiple_het_3d 
       || test_case == TestCase::gupta_3d 
       || test_case == TestCase::hole)
     {
       min_cell_diameter = dof_handler.begin(0)->diameter();
       min_cell_diameter *= std::pow(2.0,-1.0*(n_global_pre_refine+n_refinement_cycles+n_local_pre_refine));
     }

   // Set additional runtime parameters, the
   // regularization parameters, which
   // are chosen dependent on the present mesh size
   // old relations (see below why now others are used!)

       FunctionParser<1> func;
       prm.enter_subsection("Problem dependent parameters");
       
       //SHLEE DEBUG
       /*
	 set K reg				= 1e-8*h
	 # 2.0*h
	 # 0.25 * pow(h,0.5)
	 set Eps reg 				= 0.125*pow(h,0.25)
       */
       func.initialize("h", prm.get("K reg"), std::map<std::string, double>());
       constant_k = func.value(Point<1>(min_cell_diameter), 0);
       func.initialize("h", prm.get("Eps reg"), std::map<std::string, double>());
       alpha_eps = func.value(Point<1>(min_cell_diameter), 0);
       

       prm.leave_subsection();
       

       if(test_case == TestCase::sneddon_3d) {
	 constant_k =  5.41266e-09;
	 alpha_eps =  2.* min_cell_diameter       ;// 0.5 * std::sqrt(constant_k);
       }
       
       pcout <<"-------------------------------------------- --"<<endl;
       pcout <<"runin IN determine_mesh_dependent_parameters SET :: " << " -- Set constant_k and alpha_eps " << std::endl;
       pcout <<" alpha_ eps = " << alpha_eps << " constant _k = " << constant_k << " min cell diameter " << min_cell_diameter << std::endl;
       pcout <<"-------------------------------------------- --"<<endl;
     


   //Well Model Parameters
   if(bDarcy){
     const double pi=numbers::PI;
     //double R_e = 0.;   // Equivalent Radius for the well
     R_e = pow(2.,0.25) * exp(-3.*pi/4.) * min_cell_diameter;
     // R_e > R_w is required.
     R_w =  1e-4 * min_cell_diameter; // 2.* min_cell_diameter;     // Radius for the wellbore
     h_3 = 2.;   //Thickness of the well
     R_log_value = log(R_e/R_w);

 
     AssertThrow(R_log_value > 0., ExcMessage("You need to pick R_log_value > 0"));     
   }

   AssertThrow(alpha_eps >= min_cell_diameter, ExcMessage("You need to pick eps >= h"));
   AssertThrow(constant_k < 1.0, ExcMessage("You need to pick K < 1"));
   

}


template <int dim>
  bool
  FracturePhaseFieldProblem<dim>::refine_mesh ()
{
  
  if (refinement_strategy == RefinementStrategy::fixed_preref_sneddon)
    {
      typename DoFHandler<dim>::active_cell_iterator cell =
	dof_handler.begin_active(), endc = dof_handler.end();
      
      for (; cell != endc; ++cell)
	if (cell->is_locally_owned())
	  {
	    
	    for (unsigned int vertex = 0;
		 vertex < GeometryInfo<dim>::vertices_per_cell; ++vertex)
	      {
		Tensor<1, dim> cell_vertex = (cell->vertex(vertex));
		if (cell_vertex[0] <= 2.5 && cell_vertex[0] >= 1.5
		    && cell_vertex[1] <= 2.25 && cell_vertex[1] >= 1.75)
		  {
		    cell->set_refine_flag();
		    break;
		  }
	      }
	    
	  }
    }    // end Sneddon  2D
  else if (refinement_strategy == RefinementStrategy::fixed_preref_sneddon_3D)
    {
      typename DoFHandler<dim>::active_cell_iterator cell =
	dof_handler.begin_active(), endc = dof_handler.end();

      for (; cell != endc; ++cell)
	if (cell->is_locally_owned())
	  {
	    
	    for (unsigned int vertex = 0;
		 vertex < GeometryInfo<dim>::vertices_per_cell; ++vertex)
	      {
		Tensor<1, dim> cell_vertex = (cell->vertex(vertex));
		if (cell_vertex[0] <= 6.0 && cell_vertex[0] >= 4.0 && 
		    cell_vertex[2] <= 6.0 && cell_vertex[2] >= 4.0 && 
		    cell_vertex[1] <= 5.15 && cell_vertex[1] >= 4.85) 
		  {
		    cell->set_refine_flag();
		    break;
		  }
	      }
	      
	  }
    }    // end Sneddon 3D
  
  
  
    else if (refinement_strategy == RefinementStrategy::fixed_preref_miehe_tension)
      {
	typename DoFHandler<dim>::active_cell_iterator cell =
	  dof_handler.begin_active(), endc = dof_handler.end();
	
	for (; cell != endc; ++cell)
	  if (cell->is_locally_owned())
	    {
	      
	      for (unsigned int vertex = 0;
		   vertex < GeometryInfo<dim>::vertices_per_cell; ++vertex)
		{
		  Assert(dim==2, ExcNotImplemented());
		  Tensor<1, dim> cell_vertex = (cell->vertex(vertex));
		  if (cell_vertex[0] <= 0.6 && cell_vertex[0] >= 0.0
		      && cell_vertex[1] <= 0.55 && cell_vertex[1] >= 0.45)
		    {
		      cell->set_refine_flag();
		      break;
		    }
		}
	      
	    }
      }    // end Miehe tension
    else if (refinement_strategy == RefinementStrategy::fixed_preref_miehe_shear)
      {
	typename DoFHandler<dim>::active_cell_iterator cell =
	  dof_handler.begin_active(), endc = dof_handler.end();
	
	for (; cell != endc; ++cell)
	  if (cell->is_locally_owned())
	    {
	      
	      for (unsigned int vertex = 0;
		   vertex < GeometryInfo<dim>::vertices_per_cell; ++vertex)
		{
		  Assert(dim==2, ExcNotImplemented());

		  Tensor<1, dim> cell_vertex = (cell->vertex(vertex));
		  if (cell_vertex[0] <= 0.6 && cell_vertex[0] >= 0.0
		      && cell_vertex[1] <= 0.55 && cell_vertex[1] >= 0.0)
		    {
		      cell->set_refine_flag();
                  break;
		    }
		}
	      
	    }
      }    // end Miehe shear
    else if (refinement_strategy == RefinementStrategy::phase_field_ref)    
      { 
	// refine if phase field < constant
	typename DoFHandler<dim>::active_cell_iterator cell =
	  dof_handler.begin_active(), endc = dof_handler.end();
	std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
	
	for (; cell != endc; ++cell)
	  if (cell->is_locally_owned())
	    {
	      cell->get_dof_indices(local_dof_indices);
	      for (unsigned int i=0;i<fe.dofs_per_cell;++i)
		{
	          const unsigned int comp_i = fe.system_to_component_index(i).first;
	          if (comp_i != dim)
	            continue; // only look at phase field
		  //DEBUG5
		  if (solution(local_dof_indices[i])
		      < value_phase_field_for_refinement )
		    {
		      cell->set_refine_flag();
		      break;
		    }
		}
	    }
      }
    else if (refinement_strategy == RefinementStrategy::global)
      {
	typename DoFHandler<dim>::active_cell_iterator cell =
          dof_handler.begin_active(), endc = dof_handler.end();
	for (; cell != endc; ++cell)
	  if (cell->is_locally_owned())
	    cell->set_refine_flag();
      }
    else if (refinement_strategy == RefinementStrategy::mix)
      {
	
       {
	 typename DoFHandler<dim>::active_cell_iterator cell =
	   dof_handler.begin_active(), endc = dof_handler.end();
	 std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
	 
	 for (; cell != endc; ++cell)
	   if (cell->is_locally_owned())
	     {
	       cell->get_dof_indices(local_dof_indices);
	       for (unsigned int i=0;i<fe.dofs_per_cell;++i)
		 {
		   const unsigned int comp_i = fe.system_to_component_index(i).first;
		   if (comp_i != dim)
		     continue; // only look at phase field
		   //DEBUG5                  
		   if (solution(local_dof_indices[i])
		       < value_phase_field_for_refinement )
		     {
		       cell->set_refine_flag();
		       break;
		     }
                }
	     }
       }
       


       
       //DEBUG needed here for MPI
       // maybe locally_active_cells? 
       
       Vector<float> estimated_error_per_cell (triangulation.n_active_cells());
       std::vector<bool> component_mask(1, true);
       //component_mask[dim] = false;
       
       /*
       // estimate displacement:
       KellyErrorEstimator<dim>::estimate (dof_handler_pressure,
					   QGauss<dim-1>(degree+2),
					   typename FunctionMap<dim>::type(),
					   solution_pressure,  //DEBUG5
					   estimated_error_per_cell,
					   component_mask,
					   0,
					   0,
					   triangulation.locally_owned_subdomain());
              
       // but ignore cells in the crack:
       {
	 typename DoFHandler<dim>::active_cell_iterator cell =
	   dof_handler.begin_active(), endc = dof_handler.end();
	 std::vector<unsigned int> local_dof_indices(fe.dofs_per_cell);
	 
	 unsigned int idx = 0;
	 for (; cell != endc; ++cell, ++idx)
	   if (cell->refine_flag_set())
	     estimated_error_per_cell[idx] = 0.0;
       }
       
       // DO WE NEED THIS?? - SHLEE? DEBUG
       parallel::distributed::GridRefinement::
	 refine_and_coarsen_fixed_number (triangulation,
                                          estimated_error_per_cell,
                                          0.3, 0.03);
       
       
       */
       
       
      } // end refinement strategy mix
      
    

  
    
  // limit level
  if (  (test_case != TestCase::sneddon_2d) && (test_case != TestCase::sneddon_3d) )  
      {
	typename DoFHandler<dim>::active_cell_iterator cell =
          dof_handler.begin_active(), endc = dof_handler.end();
	for (; cell != endc; ++cell)
	  if (cell->is_locally_owned()
	      && cell->level() == static_cast<int>(n_global_pre_refine+n_refinement_cycles+n_local_pre_refine))
	    cell->clear_refine_flag();
      }
    
    // check if we are doing anything
    {
      pcout << "    +  Refine Mesh ::  Check if we are doing anything. " << std::endl;
      bool refine_or_coarsen = false;
      triangulation.prepare_coarsening_and_refinement();
      
      typename DoFHandler<dim>::active_cell_iterator cell =
	dof_handler.begin_active(), endc = dof_handler.end();
      for (; cell != endc; ++cell)
        if (cell->is_locally_owned() &&
            (cell->refine_flag_set() || cell->coarsen_flag_set()))
	  {
	    refine_or_coarsen = true;
	    break;
	  }
      
      if (Utilities::MPI::sum(refine_or_coarsen?1:0, mpi_com) == 0){
	pcout << "    +  Refine Mesh ::  We're doing No refining/coarsening " << std::endl;
        return false;
      }
    }

    
    
    std::vector<const LA::MPI::BlockVector *> x(3);
    x[0] = &solution;  
    x[1] = &old_solution;
    x[2] = &old_old_solution;
    

    std::vector<const LA::MPI::BlockVector *> pressure_x(2); 
    pressure_x[0] =&solution_pressure;
    pressure_x[1] =&old_timestep_solution_pressure;

    std::vector<const LA::MPI::Vector *> level_set_x(2); 
    level_set_x[0] =&solution_level_set;
    level_set_x[1] =&old_timestep_solution_level_set;

    std::vector<const LA::MPI::Vector *> width_x(1); 
    width_x[0] =&solution_width;


    parallel::distributed::SolutionTransfer<dim, LA::MPI::BlockVector> 
      solution_transfer(dof_handler);
    
    parallel::distributed::SolutionTransfer<dim, LA::MPI::BlockVector> 
      solution_transfer_pressure(dof_handler_pressure);
    
    parallel::distributed::SolutionTransfer<dim, LA::MPI::Vector> 
      solution_transfer_level_set(dof_handler_level_set);

    parallel::distributed::SolutionTransfer<dim, LA::MPI::Vector> 
      solution_transfer_width(dof_handler_width);
    
    
    pcout << "    +  Refine Mesh ::  refining.. execute triangulation. " << std::endl;
    solution_transfer.prepare_for_coarsening_and_refinement(x);
    solution_transfer_pressure.prepare_for_coarsening_and_refinement(pressure_x);
    solution_transfer_level_set.prepare_for_coarsening_and_refinement(level_set_x);
    solution_transfer_width.prepare_for_coarsening_and_refinement(width_x);

    triangulation.execute_coarsening_and_refinement();
    
    // SetUp System.
    pcout << "    +  Refine Mesh ::  Setup System " << std::endl;
    setup_system();
    
    LA::MPI::BlockVector distributed_solution(partition);
    LA::MPI::BlockVector distributed_old_solution(partition);
    LA::MPI::BlockVector distributed_old_old_solution(partition);
    
    std::vector<LA::MPI::BlockVector *> tmp(3);
    tmp[0] = &(distributed_solution);
    tmp[1] = &(distributed_old_solution);
    tmp[2] = &(distributed_old_old_solution);

    solution_transfer.interpolate(tmp);

    solution = distributed_solution;
    old_solution = distributed_old_solution;
    old_old_solution = distributed_old_old_solution;
    

    pcout << "    +  Refine Mesh ::  Setup System--PRESSURE " << std::endl;
    setup_system_pressure();

    LA::MPI::BlockVector distributed_solution_pressure(partition_pressure);
    LA::MPI::BlockVector distributed_old_solution_pressure(partition_pressure);
    
    std::vector<LA::MPI::BlockVector *> tmp_pressure(2);
    tmp_pressure[0] = &(distributed_solution_pressure);
    tmp_pressure[1] = &(distributed_old_solution_pressure);

    solution_transfer_pressure.interpolate(tmp_pressure);

    solution_pressure = distributed_solution_pressure;
    old_timestep_solution_pressure = distributed_old_solution_pressure;


    pcout << "    +  Refine Mesh ::  Setup System--LevelSet " << std::endl;
    setup_system_level_set();

    LA::MPI::Vector distributed_solution_level_set(locally_owned_level_set);
    LA::MPI::Vector distributed_old_timestep_solution_level_set(locally_owned_level_set);

    
    std::vector<LA::MPI::Vector *> tmp_level_set(2);
    tmp_level_set[0] = &(distributed_solution_level_set);
    tmp_level_set[1] = &(distributed_old_timestep_solution_level_set);


    solution_transfer_level_set.interpolate(tmp_level_set);


    solution_level_set = distributed_solution_level_set;
    old_timestep_solution_level_set = distributed_old_timestep_solution_level_set;



    pcout << "    +  Refine Mesh ::  Setup System--Width " << std::endl;
    setup_system_width();

    LA::MPI::Vector distributed_solution_width(locally_owned_width);
    LA::MPI::Vector distributed_old_timestep_solution_width(locally_owned_width);

    
    std::vector<LA::MPI::Vector *> tmp_width(1);
    tmp_width[0] = &(distributed_solution_width);

    solution_transfer_width.interpolate(tmp_width);


    solution_width = distributed_solution_width;




    determine_mesh_dependent_parameters();
 

    pcout << "    +  Refine Mesh ::  completed " << std::endl;

    

    return true;


 
  }

template <int dim>
void
FracturePhaseFieldProblem<dim>::set_initial_values ()
{
  pcout << "    :  Set Initial Values " << std::endl;
  
  
  LA::MPI::BlockVector distributed_solution(partition);
  distributed_solution  = solution;
  
  //input_file
  /*
  if(!b_input_file){
    //if(bError && bParabolic){
    Initial_Pressure<dim> initial_pressure;
    //  initial_pressure.set_time(0.);
    
    LA::MPI::BlockVector saved_solution(partition_pressure);
    saved_solution = old_timestep_solution_pressure;	 
  
    VectorTools::interpolate(dof_handler_pressure,
			     initial_pressure,
			     saved_solution);
    
    old_timestep_solution_pressure=saved_solution;
    
    solution_pressure = old_timestep_solution_pressure;  
  }
  */
  //}
  

  if (test_case == TestCase::sneddon_2d || 
      test_case == TestCase::sneddon_3d)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesSneddon<dim>(min_cell_diameter), 
			       distributed_solution);
      
    }
  
  else if (test_case == TestCase::multiple_homo || 
	   test_case == TestCase::multiple_homo_3d)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesMultipleHomo<dim>(min_cell_diameter, length_1, length_2),
			       distributed_solution);
      
    }
  else if (test_case == TestCase::multiple_homo_parallel || 
	   test_case == TestCase::multiple_homo_parallel_3d)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesMultipleHomo_Parallel<dim>(min_cell_diameter), 
			       distributed_solution);
      
    }
  
  else if (test_case == TestCase::multiple_het || 
	   test_case == TestCase::multiple_het_3d)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesMultipleHet<dim>(min_cell_diameter), 
			       distributed_solution);
      
    }
  else if (test_case == TestCase::gupta_3d)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesGupta<dim>(min_cell_diameter), 
			       distributed_solution);
    }
  else if (test_case == TestCase::hole)
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesHole<dim>(min_cell_diameter), 
			       distributed_solution);
    }
  else
    {
      VectorTools::interpolate(dof_handler,
			       InitialValuesMiehe<dim>(min_cell_diameter), 
			       distributed_solution);
    }
  
  solution = distributed_solution;
  
  //pcout<<"set initial pressure values"<< endl;
  /*
  //Initial Pressure Solution
  LA::MPI::BlockVector distributed_solution_pressure(partition_relevant_pressure);
  distributed_solution_pressure  = solution_pressure;

  VectorTools::interpolate(dof_handler_pressure,
			   InitialValues_Pressure<dim>(min_cell_diameter), 
			   distributed_solution_pressure.block(0));    
  solution_pressure  = distributed_solution_pressure;
  
  old_timestep_solution_pressure = solution_pressure;
  */
}



// As usual, we have to call the run method. 
template <int dim>
void
FracturePhaseFieldProblem<dim>::run ()
{

// mjm
//  int nerr = 0;
//  int ndim = 2 ;
//  int Nlev = triangulation.n_levels();
//  cout <<"the triangulation _ n levels is" <<triangulation.n_levels()<<endl;
 // int TotSteps = 10;
 // double Tend = 10.0;
//  double xLow[3]={0.0, 0.0, 0.0};
//  double xHigh[3]={1.0, 4.0, 4.0} ;
//  int Ncoarse[3]={1,32,32} ;
//  const int NE =  triangulation.n_active_cells();
//  int Nele = NE;
//  double *perm, *pres;
 // perm = (double*) malloc(Nele*sizeof(double));
//  perm = (double*) malloc(Nele*sizeof(double));
 // fill(perm, perm+Nele, 9.869233e-18);

//  pres = (double*) malloc(Nele*sizeof(double));
//  fill(pres, pres+Nele, 0.0e0);
//  double perm[3322]={0};
//  double pres[3322]={0};

 //         perm = & permeability_per_cell(cell_index);
//        xLow = {0.0, 0.0, 0.0};
//        xHigh = {1.0, 4.0, 4.0} ;
//        ndim = 2;
//        Ncoarse = {1,32,32};
//        Nlev  = triangulation.n_levels()-5; 
//        Nele = triangulation.n_active_cells();
// // int grid[NE][4] = {
 //   {1,1,1,1},
 //   {1,2,1,1},
 //   {1,1,2,1},
 //   {1,3,3,2},
 //   {1,3,4,2},
 //   {1,4,3,2},
 //   {1,4,4,2}};
 //     nerr = MPI_Init(NULL,NULL);
 //     if (nerr !=0){
  //                  cout << "Error " << nerr <<"from MPI_Init"<<endl;
  //                  exit(1);
   //                }
        
     
 // fill(perm, perm+Nele, 9.869233e-15);

 // pres = (double*) malloc(Nele*sizeof(double));
 // fill(pres, pres+Nele, 0.0e0);
 
//  ipars_start_(&nerr);
 


//// double deltim = Tend/TotSteps;
 // double tim = 0.0;
 // //for (int nstep=0; nstep<TotSteps; nstep++) {
  //  ipars_solve_(&nstep,&tim,&deltim,&nerr);
  //  ipars_visualize_(&nerr);
  //  tim += deltim;
  //  ipars_recvpres_(&nerr);
 // }
 // ipars_finish_(&nerr);

 // free(perm);
 // free(pres);

// mjm
  
  pcout << "Running on " 
	<< Utilities::MPI::n_mpi_processes(mpi_com)
	<< " cores" 
	<< std::endl;
  
  pcout << "++RUN : Set Runtime Parameters " << std::endl;
  set_runtime_parameters();
  
  pcout << "++RUN : Setup system / Pressure " << std::endl;
  setup_system();
  setup_system_pressure();
  setup_system_level_set();
  setup_system_width();
  
  determine_mesh_dependent_parameters();
 cout<<"leaving mesh dependent parameters"<<endl;
 // mjm
 //
        
          
 
  /*
  {
	outputFile_C << "dimension = 2" << endl;
	outputFile_C << "Total Coarsest Active Cell Number = " 
		   << triangulation.n_active_cells()
		   << endl;
	outputFile_C << "Total Coarsest Active DOFs = " 
		   << dof_handler.n_dofs()
		   << endl;
	outputFile_C << "N = 1, " 
		   << sqrt(triangulation.n_active_cells())<<", "
		   << sqrt(triangulation.n_active_cells())
		   << endl;
	outputFile_C << "(X1, Y1, Z1)  = "
		   << " 0, 0, 4 "
		   <<endl;
	outputFile_C << "(X2, Y2, Z2)  = "
		   << " 0, 4, 0 "
		   <<endl;
	outputFile_C << "Total levels = " 
		   << triangulation.n_levels() + n_local_pre_refine -1 
		   << endl;

  }
  */
	   

//        cout <<"now leaving cell index loop "<<cell_index<<endl;

 //       ipars_sendgrid_(xLow,xHigh,&ndim,Ncoarse,&Nlev,&Nele,&grid[0][0],&nerr);

 //       ipars_init_(&nerr);
 //   //    cout << " Permeability is :"<<permeability_per_cell<<endl;
 //   //    perm = &permeability_per_cell;
 //  //     pres = solution_pressure;
 //       ipars_set_array_pointers_(perm,pres,&nerr);
 //       ipars_sendperm_(&nerr);
 //       ipars_sendpres_(&nerr);

 //       cout << " Before starting the time loop"<<endl;

 // mjm 

  for (unsigned int i = 0; i < n_local_pre_refine; ++i)
    {
      pcout << "RUN : refine mesh cycle :  n_local_pre_refine = " << i << " " <<   std::endl;
      
      set_initial_values();
      //refine_mesh();

    }
 
  if (n_local_pre_refine==0){
    set_initial_values();
  }

  //input_file
  if(b_input_file)
    read_solutions();
    

  
  pcout << "\n=============================="
	<< "=====================================" << std::endl;
  pcout << "Parameters\n" << "==========\n" 
	<< "h (min):                " << min_cell_diameter << "\n" 
	<< "k:                      " << constant_k << "\n" 
	<< "eps:                    " << alpha_eps << "\n"
	<< "Biot coefficient:       " << alpha_biot << "\n"
        << "Compress. Reservoir:    " << c_biot << "\n"
	<< "Compress. Fracture:     " << c_F << "\n"
	<< "Viscosity Reservoir:    " << viscosity_biot << "\n"
	<< "Viscosity Fracture:     " << viscosity_F << "\n"
    	<< "Permeability Reservoir: " << K_biot << "\n"
	<< "Density Reservoir:      " << density_biot << "\n"
	<< "Use Peaceman's well:    " << bPeaceman_well_model << "\n"
	<< "G_c:               " << G_c <<", " << G_c_2 <<  "\n" 
	<< "Poisson nu:        " << poisson_ratio_nu << "\n"
	<< "E modulus:         " << E_modulus << "\n" 
	<< "Lame mu:           " << lame_coefficient_mu << "\n" 
	<< "Lame lambda:       " << lame_coefficient_lambda << "\n" 
	<< std::endl;
  
  set_initial_values();

  // Normalize phase-field function between 0 and 1
  project_back_phase_field();
  
  output_results();

  if( test_case == TestCase::hole ){
    QGauss<dim-1>   face_quadrature_formula(degree+1);
    FEFaceValues<dim> fe_face_values (fe, face_quadrature_formula,
                                      update_values |  update_quadrature_points );
    const unsigned int  n_q_points  = face_quadrature_formula.size();
    std::vector<Vector<double> > solution_values(n_q_points,
                                                 Vector<double>(dim+1));
    
    typename DoFHandler<dim>::active_cell_iterator
      cell = dof_handler.begin_active(),
      endc = dof_handler.end();
    
    for (; cell!=endc; ++cell)
      if (cell->subdomain_id() == triangulation.locally_owned_subdomain())
        for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face) {
          if (cell->face(face)->at_boundary())
            if(cell->face(face)->boundary_indicator() == 1){
	      
              fe_face_values.reinit (cell, face);
              fe_face_values.get_function_values (solution, solution_values);
	      
              for (unsigned int q=0; q<n_q_points; ++q)
                {
                  double pf_value = solution_values[q](dim);
		  
                  if(pf_value == 0)
                    cell->face(face)->set_boundary_indicator (2);
		  
                }
            }
	  
        }
  }
  
  
  
  {
  int cell_index = 0; 
  Vector<double> bd_no ( triangulation.n_active_cells() );
  typename DoFHandler<dim>::active_cell_iterator cell  = dof_handler_pressure.begin_active(),
    cell_end        = dof_handler_pressure.end();
  
  for(  ; cell != cell_end; ++cell,  ++cell_index )
    {
      if (cell->subdomain_id() == triangulation.locally_owned_subdomain()){
        for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face) {
	  int a = cell->face(face)->boundary_indicator();
	  if(cell->face(face)->at_boundary() )
	    bd_no(cell_index) = a;   
        }
        
      }
    }
  
  output_bd(bd_no);  
  }
  
  
  const unsigned int output_skip = 1;
  unsigned int refinement_cycle = 0;
  double finishing_timestep_loop = 0;

  // Initialize old and old_old_solutions
  // old_old is needed for extrapolation for pf_extra to avoid pf^2 in block(0,0)
  old_old_solution = solution;
  old_solution = solution;    
  old_timestep_solution_pressure = solution_pressure;

  
  // Initialize old and old_old timestep sizes
  old_timestep = timestep;
  old_old_timestep = timestep;


// ********************* Added part to interface with IPARS *********************** \\
// mjm
//
  int adaptivity = 0;  // 0=off, 1=on
  int nerr = 0;
  int ndim = 2 ;
  int Nlev = triangulation.n_levels()-n_global_pre_refine; 
 // int TotSteps = 10;
 // double Tend = 10.0;
  double xLow[3]={0.0, 0.0, 0.0};
  double xHigh[3]={1.0, 4.0, 4.0} ;
  int Ncoarse[3]={1,pow(2,n_global_pre_refine),pow(2,n_global_pre_refine)} ;
  const int NE =  triangulation.n_active_cells();
  int Nele = NE;
  double *perm, *pres;
  perm = (double*) malloc(Nele*sizeof(double));
  fill(perm, perm+Nele, 9.869233e-15);
  pres = (double*) malloc(Nele*sizeof(double));
  // fill(pres, pres+Nele, 0.0e0);
  int grid[NE][4] ;
  unsigned int cell_index = 0;
  int x,y,z ;
  double dy, dz ;
  const unsigned int   dofs_per_cell   = fe_pressure.dofs_per_cell;
  typename DoFHandler<dim>::active_cell_iterator
      cell = dof_handler.begin_active(),
      endc = dof_handler.end();
  int counter=0;
    for (; cell!=endc; ++cell, ++cell_index){
   
             dy = (cell->vertex(1)[0]) - (cell->vertex(0)[0]) ;
             dz = (cell->vertex(2)[1]) - (cell->vertex(0)[1]) ;
             y  = (cell->vertex(0)[0])+dy;
             z  = (cell->vertex(0)[1])+dz;
             grid[counter][0] = 1;
             grid[counter][1] = (cell->vertex(3)[0])/dy;
             grid[counter][2] = (cell->vertex(3)[1])/dz;
             grid[counter][3] = cell->level()-n_global_pre_refine+1;
             perm[counter] =  permeability_per_cell(cell_index);
             pres[counter]=VectorTools::point_value(dof_handler_pressure,solution_pressure.block(0),Point<2>(y, z));
          //   cout << " The cell index is "<<cell<<endl;
             ++counter;
         }
  ipars_start_(&nerr);
  ipars_sendgrid_(xLow,xHigh,&ndim,Ncoarse,&Nlev,&Nele,&grid[0][0],&adaptivity,&nerr);
  ipars_init_(&nerr);
  ipars_set_array_pointers_(perm,pres,&nerr);
  ipars_sendperm_(&nerr);
  ipars_sendpres_(&nerr);
  ipars_visualize_(&nerr);


// ********************* End of the added part  *********************** \\


  // Timestep loop
  do
    {     
      double newton_reduction = 1.0;
      
      if (timestep_number > switch_timestep && switch_timestep>0)
	timestep = timestep_size_2;
      
      old_old_timestep = old_timestep;
      old_timestep = timestep;
      
      // Compute next time step
      old_old_solution = old_solution;
      old_solution = solution;
      old_timestep_solution_pressure = solution_pressure;
      
      
      unsigned int total_no_fixed_stress_iterations = 0;
      
      //cout<< "The point pressure is  "; pcout  <<  VectorTools::point_value(dof_handler_pressure,solution_pressure.block(0),Point<2>(0.125, 0.125)) <<endl;
      // redo-step is for predictor-corrector mesh refinement
    redo_step:
      pcout << std::endl;
      pcout << "\n=============================="
	    << "=========================================" << std::endl;
      pcout << "Timestep " << timestep_number << ": " << time + timestep << " (" << timestep << ")"
	    << "   " << "Cells: " << triangulation.n_global_active_cells()
	    << "   " << "DoFs: " << dof_handler.n_dofs();
      pcout << "\n--------------------------------"
	    << "---------------------------------------" << std::endl;
      
      pcout << std::endl;


      time += timestep;
// mjm
  //
   //   newton_iteration_pressure (time);
      cout<<"the time step number is:"<<timestep_number<<endl;  
      cout<<"the time  is:"<<time<<endl;  
      cout<<"the time step is:"<<timestep<<endl;  
      ipars_solve_(&timestep_number,&time,&timestep,&nerr);
      ipars_visualize_(&nerr);
      ipars_recvpres_(&nerr);
      Vector<double> pres_cell_temp;
      Vector<double> pres_dof_temp;
      pres_cell_temp.reinit(triangulation.n_active_cells());  
      pres_dof_temp.reinit(dof_handler_pressure.n_dofs());
  typename DoFHandler<dim>::active_cell_iterator
      cell = dof_handler.begin_active(),
      endc = dof_handler.end();
  int counter = 0;
  int cell_index = 0;
    
    for (; cell!=endc; ++cell, ++cell_index){
      pres_cell_temp[cell_index]=pres[counter];  
      ++counter;
       }

  DoFTools::distribute_cell_to_dof_vector(dof_handler_pressure,pres_cell_temp,pres_dof_temp);	
  cout<<"the size of pres_dof_temp is "<<dof_handler_pressure.n_dofs()<<endl;

      cell = dof_handler.begin_active(),
      endc = dof_handler.end();
      counter = 0;
      cell_index = 0;
    for (; cell!=endc; ++cell, ++cell_index){
      solution_pressure[cell_index]=pres_dof_temp[counter];  
      ++counter;
       }


// mjm

 // for (unsigned int i=0;dof_handler_pressure.n_dofs(); ++i) {
 //       cout<< pres_dof_temp(i)<<endl;
 //     }
 
  /*
      if (outer_solver == OuterSolverType::active_set)
	{
	  time += timestep;
	  
	  if(bDarcy){
	    old_fixed_stress_solution_pressure = solution_pressure;
	    old_fixed_stress_solution = solution;
	  }
	  

	  bool fixed_stress_toleration = true;
	  unsigned int fixed_stress_counter = 0;
	  while(fixed_stress_toleration)
	    {
	      ++fixed_stress_counter;
	      
	      if(fixed_stress_counter > 1)
		fixed_stress_toleration = false;
	      
	      
	      if(fixed_stress_counter > 100){
		pcout<< "===== Over Fixed Stress Iterations ======" << std::endl;
		exit(0);
	      }
		  
		 


	      // Solve level set
	      set_material_ids();
	      
	  //    for (unsigned int k=0;k<2; k++)
	//	{
	//	  assemble_system_level_set ();
	//	  solve_level_set ();
	//	  old_timestep_solution_level_set = solution_level_set;
	//	}
	      

	      assemble_system_level_set_by_phasefield ();
	      
	      // Solve for the width
	      assemble_system_width ();
	      solve_width ();

	      
	      // Solve pressure
              if(bDarcy){
		pcout<< "====== Fixed Stress Interation Number - " << fixed_stress_counter << " - ============" << std::endl;
		newton_iteration_pressure (time);
	      }
	      
	      // Solve displacements-phase-field
	      //newton_active_set();		    
	      pcout<< "================================================================" << std::endl;




	      if(bDarcy){
		// Fixed stress stopping criterium
		old_fixed_stress_solution_pressure -= solution_pressure;
		old_fixed_stress_solution    -= solution;


		// TODO 3: I am not 100% sure if we just can take these
		// measurements as stopping criterium for the fixed-stress
		// with phase-field (maybe we have to cut out the crack region)
		Vector<float> difference_per_cell_p (triangulation.n_locally_owned_active_cells());
		VectorTools::integrate_difference (dof_handler_pressure,
						   old_fixed_stress_solution_pressure.block(0),
						   ZeroFunction<dim>(1),					     
						   difference_per_cell_p,
						   QGauss<dim>(3),
						   VectorTools::L2_norm);
		//double L2_error_p = difference_per_cell_p.l2_norm();
		const double local_error_p = difference_per_cell_p.l2_norm();
		const double L2_error_p =  std::sqrt( Utilities::MPI::sum(local_error_p * local_error_p, mpi_com));
		
		


		Vector<float> difference_per_cell_p1 (triangulation.n_locally_owned_active_cells());
		VectorTools::integrate_difference (dof_handler_pressure,
						   solution_pressure.block(0),
						   ZeroFunction<dim>(1),					     
						   difference_per_cell_p1,
						   QGauss<dim>(3),	
						   VectorTools::Linfty_norm);
		double Linfty_p = difference_per_cell_p1.linfty_norm();
		double high_pressure = 0.;
		high_pressure = Utilities::MPI::max(Linfty_p, mpi_com);

		
		ComponentSelectFunction<dim> value_select_phi (dim,dim+1);
		Vector<float> difference_per_cell_phi (triangulation.n_locally_owned_active_cells());
		VectorTools::integrate_difference (dof_handler,
						   old_fixed_stress_solution,
						   ZeroFunction<dim>(dim+1),                                       
						   difference_per_cell_phi,
						   QGauss<dim>(3),
						   VectorTools::L2_norm,
						   &value_select_phi);
		double local_error_phi = difference_per_cell_phi.l2_norm();
		const double L2_error_phi =  std::sqrt( Utilities::MPI::sum(local_error_phi * local_error_phi, mpi_com));
		
		
		const ComponentSelectFunction<dim> value_select_u (std::make_pair(0,dim), dim+1);
		Vector<float> difference_per_cell_u (triangulation.n_locally_owned_active_cells());
		VectorTools::integrate_difference (dof_handler,
						 old_fixed_stress_solution,
						   ZeroFunction<dim>(dim+1),					     
						   difference_per_cell_u,
						   QGauss<dim>(3),
						   VectorTools::L2_norm,
						   &value_select_u);
		
		
		double local_error_u = difference_per_cell_u.l2_norm();
		const double L2_error_u =  std::sqrt( Utilities::MPI::sum(local_error_u * local_error_u, mpi_com));
		
		pcout << "-----------------------------------" << endl;        
		pcout << " Highest Pressure: " << time << "   " << high_pressure << endl;
		pcout << " FS Stopping :  ||Phi||_L2: " << L2_error_phi 
		      << " ||P||_L2 : " << L2_error_p 
		      << " ||P||_scaled : " << L2_error_p/lame_coefficient_mu 
		      << " ||U||_L2 : "  << L2_error_u
		      <<endl;
//		pcout <<" Current time step = " 
//		      <<timestep_number << " Current Fixed Stress Counter = "  << fixed_stress_counter 
		//      << endl;
		pcout << "-----------------------------------" << endl;        
		pcout << " " << endl;
		
		old_fixed_stress_solution_pressure = solution_pressure;
		old_fixed_stress_solution = solution;
		
		//double l2_norm_pressure, l2_norm_displacement, l2_norm_phasefield = 0.;
		double TOL_fs = TOL_Fixed_Stress;
		double TOL_fs_p = TOL_Fixed_Stress;
		if (timestep_number > 3)
		  {
		    TOL_fs   = TOL_Fixed_Stress_Two;
		    TOL_fs_p = TOL_Fixed_Stress_Two;
		  }
		
		//DEBUG
		if(test_case == TestCase::hole)
		  TOL_fs_p = 1e-2;
		
		// TODO: implement fixed-stress stopping 
		// from Bin Wang et al. 

		// Bin Wang:
		double L2_error_p_scaled = L2_error_p/lame_coefficient_mu;
		

		
		if( L2_error_phi < TOL_fs &&  
		    L2_error_p_scaled   < TOL_fs_p && 
		    L2_error_u   < TOL_fs)
		  {
		    pcout << " NoFSIterPerMesh: " << time << "  " << fixed_stress_counter << endl;
		    pcout << "-----------------------------------" << endl; 
		    pcout << endl;

		    total_no_fixed_stress_iterations += fixed_stress_counter;
		  fixed_stress_toleration = false;

	      }
	      
	      
	      } // end bDarcy

	      //DEBUG
	      if(bDarcy == false)
		fixed_stress_toleration = false;

	    } // end of fixed-stress iteration
	  
	  
		   
	    
	}// if == Active set
      */

      // Normalize phase-field function between 0 and 1
      //project_back_phase_field();

      /*
      if ( (test_case != TestCase::sneddon_2d) &&  (test_case != TestCase::sneddon_3d))   
	{
	  
	  bool changed = refine_mesh();
	  if (changed)
	    { // redo the current time step
	      pcout << "MESH CHANGED!" << std::endl;
	      time -= timestep;
	      solution = old_solution;
	      pcout <<"****************** GOTO - REDO STEP *********************"<<endl;
	      goto redo_step;
	      continue;
	    }
	}
      */

      pcout << " TotNoFSIter: " << time << "  " << total_no_fixed_stress_iterations << endl;
      pcout << "-----------------------------------" << endl; 
      pcout << endl;

      
      // Compute functional values
      //pcout << std::endl;
      //compute_energy();
      //compute_functional_values();

      
      if (test_case == TestCase::sneddon_2d ||
	  test_case == TestCase::sneddon_3d ||  
	  test_case == TestCase::multiple_homo ||	    
	  test_case == TestCase::multiple_homo_3d ||	    
	  test_case == TestCase::multiple_homo_parallel ||	    
	  test_case == TestCase::multiple_homo_parallel_3d ||	    
	  test_case == TestCase::multiple_het ||
	  test_case == TestCase::multiple_het_3d || 
	  test_case == TestCase::gupta_3d  ||
          test_case == TestCase::hole)
	{
	  pcout << std::endl;
	}
      else
	compute_load();
      	
      
      if (test_case == TestCase::sneddon_2d  || 
	  test_case == TestCase::sneddon_3d )
	{
	  LA::MPI::BlockVector distributed_solution(partition);
	  distributed_solution = solution;       
	  
	  LA::MPI::BlockVector distributed_old_solution(partition);
	  distributed_old_solution = old_solution;       
	  
	  
	  LA::MPI::BlockVector residual(partition);
	  
	  residual = distributed_old_solution;
	  residual.add(-1.0, distributed_solution);
	  
	  solution= distributed_solution;
	  old_solution= distributed_old_solution;
	  
	    
	  const double local_error =   residual.linfty_norm();
	  const double Linfty_error =  Utilities::MPI::max(local_error, mpi_com);
	  
	  
	  // Stopping criterion time step algorithm (Sneddon)
	  finishing_timestep_loop = Linfty_error;// residual.linfty_norm();
	  pcout << "Timestep difference linfty: " << finishing_timestep_loop << std::endl;
	  
	  
	  if(finishing_timestep_loop < 1.0e-4){
	//    compute_functional_values();
	    
	      

	    if (n_refinement_cycles==0)
	      break;
	    
	    --n_refinement_cycles;
	    //timestep_number = 0;
	    pcout << std::endl;
	    pcout  << "\n================== " << std::endl;
	    pcout << "Refinement cycle " << refinement_cycle
		  << "\n------------------ " << std::endl;
	    
	    //refine_mesh();
	    
	    ++refinement_cycle;
	    set_initial_values();
	    
	    
	  } // if (finishing_timestep_loop < 1.0e-5)
	  
	}//	if (test_case == TestCase::sneddon_2d  ||   test_case == TestCase::sneddon_3d )
      
      
      /*
      const std::string filename_0 =
	( "Rmesh-"
	  + Utilities::int_to_string(timestep_number, 6)
	  + ".txt" );
      std::ofstream outputFile_R(filename_0.c_str());
      
      {
	
	//outputFile_R<<"Cell #" <<" " 
	//	    <<"Dof Index " << " " 
	//	    <<  "Nodes (I=1 , J, K, level)" << " "
	//	    << "Data " 
	//	    <<endl;
	
	unsigned int cell_index = 0;

	const unsigned int   dofs_per_cell   = fe_pressure.dofs_per_cell;
	typename DoFHandler<dim>::active_cell_iterator
	  cell = dof_handler.begin_active(),
	  endc = dof_handler.end();
	for (; cell!=endc; ++cell, ++cell_index)     
	  {
	    for (unsigned int i=0; i<dofs_per_cell; ++i){
	      
	      outputFile_R  <<  cell <<"  " 
			   << i << " " 
			   <<"1 "
			   <<  cell->vertex(i) <<"  " 
			   << cell->level() << "  "
			    << permeability_per_cell(cell_index)
			   << endl;
	    }
	  
	  }
	
      }
      */
      //write_solutions();
      
      ++timestep_number;



      if ((timestep_number % output_skip == 0)){
	output_results();
      

      }
      


      
    } while (timestep_number <= max_no_timesteps);
  
  pcout << std::endl;
  pcout << "Finishing time step loop: " << finishing_timestep_loop
	<< std::endl;
  
  pcout << std::resetiosflags(std::ios::floatfield) << std::fixed;
  std::cout.precision(2);
  
  Utilities::System::MemoryStats stats;
  Utilities::System::get_memory_stats(stats);
  pcout << "VMPEAK, Resident in kB: " << stats.VmSize << " " << stats.VmRSS
        << std::endl;

// mjm  
      if (timestep_number == max_no_timesteps){

      ipars_finish_(&nerr);
      free(perm);
      free(pres);
//      MPI_Finalize();}
// mjm
}
}



template <int dim>
void FracturePhaseFieldProblem<dim>::output_bd(Vector<double> bd_indicator_v){

  DataOut<dim> density_per_cell_data;
  density_per_cell_data.attach_dof_handler (dof_handler_pressure);
  
  density_per_cell_data.add_data_vector (bd_indicator_v, "bd_indicator"
					 /*DataOut<dim>::type_cell_data*/);
  
  density_per_cell_data.build_patches ();
  
  
  
  const std::string filename_0 =
    ( "bd_indicator-"
      + Utilities::int_to_string(timestep_number, 6)
      + "."
      + Utilities::int_to_string(triangulation.locally_owned_subdomain(), 2)
      + ".vtu" );
  std::ofstream output_0 (filename_0.c_str());
  density_per_cell_data.write_vtu (output_0);
  
if(Utilities::MPI::this_mpi_process(mpi_com) == 0)
    {
      std::vector<std::string> filenames;
      for (unsigned int i=0; i< Utilities::MPI::n_mpi_processes(mpi_com); ++i)
	filenames.push_back ("bd_indicator-" +
			     Utilities::int_to_string(timestep_number, 6)  +
			     "." +
			     Utilities::int_to_string(i, 2) +
			     ".vtu");
      
      // FOR MANY PROCESSORS..
      const std::string
	pvtu_master_filename = ("bd_indicator-" +
				Utilities::int_to_string (timestep_number, 6) +
				".pvtu");
      std::ofstream pvtu_master (pvtu_master_filename.c_str());
      density_per_cell_data.write_pvtu_record (pvtu_master, filenames);
      
      const std::string
	visit_master_filename = ("bd_indicator-"  +
				 Utilities::int_to_string (timestep_number, 6) +
				 ".visit");
      std::ofstream visit_master (visit_master_filename.c_str());
    }
}


// The main function looks almost the same
// as in all other deal.II tuturial steps. 
int
main (int argc, char *argv[])
{
  Utilities::MPI::MPI_InitFinalize mpi_initialization(argc, argv, 1);
  try
    {
      deallog.depth_console(0);
      
      ParameterHandler prm;
      ParameterReader param(prm);

      //bDarcy = prm.get_bool("darcy switch");

      if (argc>1)
        param.read_parameters(argv[1]);
      else
        param.read_parameters("parameters_miehe_tension.prm");

      FracturePhaseFieldProblem<deal_II_dimension> fracture_problem(1, prm);
      fracture_problem.run();
      
    }
  catch (std::exception &exc)
    {
      std::cerr << std::endl << std::endl
          << "----------------------------------------------------"
          << std::endl;
      std::cerr << "Exception on processing: " << std::endl << exc.what()
          << std::endl << "Aborting!" << std::endl
          << "----------------------------------------------------"
          << std::endl;

      return 1;
    }
  catch (...)
    {
      std::cerr << std::endl << std::endl
          << "----------------------------------------------------"
          << std::endl;
      std::cerr << "Unknown exception!" << std::endl << "Aborting!" << std::endl
          << "----------------------------------------------------"
          << std::endl;
      return 1;
    }

  return 0;
}
