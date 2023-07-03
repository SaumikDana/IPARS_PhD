The IPARS (Integrated Parallel Accurate Reservoir Simulator) is a new generation framework for petroleum reservoir simulation developed as part of the ACTI project. It serves as a test-bed for multiphase flow models, numerical discretizations, solvers, and upscaling.

Here are some more details about IPARS:

General Geometry on IPARS:

- Dipping Reservoir
- Multiblock
- Small Angle Approximation

Current Features:

- Portability between different platforms including PC under DOS/Windows or Linux and PC clusters, RS6K and SP2, SGI, Cray T3E, etc.
- Memory management for multiple fault blocks, multiblock nonmatching grids, and general geometry.
- Multiple processors (ghost region updates and global reduce operations)
- Time step control
- Error management, CPU timing system
- Keyword input
- Visualization output for scalar and vector variables, data tables, etc.
- A suite of linear solvers
- Well management tools for different types of injection and production wells
- Multiple physical models including two-phase (oil/water) implicit, two-phase (oil/water) sequential (Fast Accurate), black oil (three-phase, three-component), two-phase (air/water) implicit, single-phase (slightly compressible) explicit, single-phase (slightly compressible) implicit, EOS compositional (in review phase, also see EOS page), and much more (some still in development phase)
- Multiblock formulation using mortar spaces and implemented with MACE
- Multimodel formulation allowing for multinumerics and multiphysics

Recent Accomplishments:

- Running Mega-size problems with the two-phase and black oil models on PC-clusters. The parallel scaling of the code is very good.
- Achieved multiphysics; coupling in which IPARS models: black oil, two-phase, and single-phase models are coupled across the interface. In the earlier phase of this work, they completed multinumerics coupling.
- IPARS black oil model was successfully used in the projects:
    Coupling of multiphase flow with geomechanics, in collaboration with Sandia National Labs, see Coupled Geomechanical Deformation and Reservoir Simulation, or here.
    Coupling of multiphase flow with single-well injectivity modeling.
- Advances in parallel procedures for multiphase flow include preconditioned GMRES with variable multistage preconditioners (Uzawa, and IMPES family). These schemes have been tested in the IPARS framework for two and three-phase fully-implicit models, and are the foundation of the excellent parallel scalability of IPARS. Collaboration with Philippe Quandalle (IFP) and Yuri Vassilevski (Russian Academy of Sciences) was crucial to achieving the current level of performance.
- Collaboration with ICES (Institute for Computational Engineering and Sciences) at UT has raised interesting possibilities for applying recent computer science advances, such as coupling the low-level dynamic mesh data structures of the DAGH (Distributed Adaptive Grid Hierarchy) package with the domain knowledge framework embodied in IPARS, and using “space-filling curves” for ordering the grid elements upon the processors.
