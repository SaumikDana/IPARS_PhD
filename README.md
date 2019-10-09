# IPARS_PhD
#Code framework I worked on during my PhD at UT Austin

The algorithm I developed has separate finite element grids for flow and poromechanics. That algorithm is built on top of the existing algorithm that solves flow and poromechanics sequentially on the same grid. Since the code is heavily parallelized using MPI, and since it is possible to have MPI processes not occupying flow elements, we design a separate MPI subcommunicator for flow.

All code chunks I have worked on can be found by "grep -irn "saumik" ./"

---------------------------------- Comments on code contributions from Saumik -----------------------------------

--------- /comp_mfmfe is the folder pertaining to compositional flow model

xarray.df has all memory allocation for extra parameters introduced for the two grid framework 

xarydat.h has the names of the extra parameters declared

xiadat.df gets the input for the extra parameters introduced

xstep.df tells the compiler to use the flag mbporoe to identify the two grid problem and proceed accordingly

xvisual.df calls the visualization part of the code framework to generate fields in the eventual vtk file

---------- /drive is the folder with the driver code ipars.df

ipars.df has the call to the flag mbporoe that ensures separate element index numbering for flow and poromechanics model

---------- /input is the folder pertaining to parameters common to flow and poromechanics

blkary.dh has declaration of all variables pertaining to design of MPI subcommunicator and the big field problem we solve

idata.df has declaration of flags pertinent to various types of problems we want to study -- routine setgeomnew is written with routine setgeomas a template with all the logic for declaration of MPI subcommunicator

---------- /memman is the folder pertaining to memory allocation and division of elements among MPI processes 

divide.df invokes the METIS library to divide elements among processes as an option

memman1.dc invokes a routine which help in visualization in the two-grid framework using the C/Fortran name mangling framework

memman2.dc invokes three routines which work to create grid-to-grid projection operators and transfer data from one grid to the other using the C/Fortran name mangling framework

added metis.dc for METIS partitioning

added tetgen.h and tetgen.cpp for calls to TETGEN delaunay triangulation library

---------- /porohex is the folder pertaining to poromechanics model

file eprojections.df has all the code with regards to computation of grid-to-grid projection operators

added functionality using sdpm flag in earray.df

added flag mbporoe in eidata.df and eivdat.df wherever necessary

routine eiter in estep.df calls routines written in memman2.dc for grid-to-grid transfers

added flag mbporoe in evisual.df wherever necessary

changes to bc0.f90 and post_prcss0.f90 for error indicators 

---------- /singlei_mfmfe is the folder pertaining to single phase flow model

added flag mbporoe in tisdat.df, tprop.df, tstep.df and tvisual.df wherever necessary

tarray.df has all memory allocation for extra parameters introduced for the two grid framework 

