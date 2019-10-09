# IPARS_PhD
Code framework I worked on during my PhD at UT Austin

All code chunks I have worked on can be found by "grep -irn "saumik" ./" or "grep -irn "SAUMIK" ./"

--------- /comp_mfmfe is the folder pertaining to compositional flow model with multipoint flux mixed finite element method

xarray.df has all memory allocation for extra parameters introduced for the two grid framework 

xarydat.h has the names of the extra parameters declared

xiadat.df gets the input for the extra parameters introduced

xstep.df tells the compiler to use the flag mbporoe to identify the two grid problem and proceed accordingly

xvisual.df calls the visualization part of the code framework to generate fields in the eventual vtk file
