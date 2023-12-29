```
IPARS_PhD/
├── blacki/                       # Black-Oil Model.
│   ├── .svn/                     # Subversion control metadata.
│   └── data/                     # Data files, possibly input or test data.
│       ├── .svn/
│       └── single_block/         # Data related to a 'single block' scenario or model.
│           ├── .svn/
│           ├── test1/            # Test or example data set 1.
│           │   └── .svn/
│           └── test2/            # Test or example data set 2.
│               └── .svn/
├── blocks/                       # Could contain scripts or code for block-based operations or models.
│   └── dual/                     # Specific type or method of block handling.
├── comp/                         # Compositional Flow Model.
│   ├── .svn/
│   ├── Benchmark/                # Benchmarking tests or data for performance evaluation.
│   │   ├── 1D_diff/              # 1D diffusion (or similar) benchmark.
│   │   │   └── .svn/
│   │   ├── 2D_diff_corrected/    # Corrected 2D diffusion benchmark.
│   │   │   └── .svn/
│   │   ├── 3D_grav/              # 3D gravity-related benchmark.
│   │   │   └── .svn/
│   │   └── ... (more benchmarks)
│   └── doc/                      # Documentation for this component.
│       └── .svn/
├── comp_mfmfe/                   # Compositional Flow Model with Multi Point Flux Mixed finite element implementation.
│   ├── Benchmark/                # Benchmarks specific to this module.
│   │   ├── 1D_diff/              # 1D diffusion benchmark.
│   │   │   └── .svn/
│   │   ├── 2D_brick_diff/        # 2D diffusion with a 'brick' model or scenario.
│   │   │   └── .svn/
│   │   └── ... (more benchmarks)
├── dealii/                       # May contain components related to the deal.II library (a C++ library for solving PDEs).
│   └── .svn/
├── doc/                          # General documentation for the entire project.
│   ├── .svn/
│   └── user/                     # User-specific documentation.
│       └── .svn/
├── hydroi/                       # 2phase flow model.
│   └── .svn/
├── hydroi_mfmfe/                 # 2phase flow model with MFMFE.
│   ├── .svn/
│   └── data/                     # Data for hydrological simulations or tests.
│       ├── Frio/                 # Data related to the 'Frio' scenario or model.
│       │   └── .svn/
│       └── Test_bric/            # Test data for a scenario named 'bric'.
│           └── .svn/
├── hydroia/                      # Another hydrological module or component.
│   ├── .svn/
│   └── data/                     # Data related to this module.
│       └── test_brick_mpfa/      # Test data for a 'brick' model with MPFA (Multipoint Flux Approximation).
│           └── .svn/
├── java/                         # Java code or scripts, possibly for utilities or interfacing.
│   └── .svn/
├── output/                       # Directory for output files from simulations, computations, or runs.
│   ├── print/                    # Printable output or logs.
│   │   └── .svn/
│   ├── restart/                  # Files to restart simulations or computations from a certain state.
│   │   └── .svn/
│   └── tecplot/                  # Output files specifically for Tecplot visualization.
│       └── .svn/
├── poroe/                        # Related to poroelasticity or similar computational models.
│   ├── .svn/
│   └── pesolve/                  # Solver or scripts for poroelasticity problems.
│       └── .svn/
├── porohex/                      # Variant or specific implementation related to 'poroe'.
│   ├── data/                     # Data for poroelasticity simulations or models.
│   │   ├── ben_singlephase1/     # Data set for a single-phase scenario.
│   │   │   └── .svn/
│   │   ├── bradley_1ph/          # Data for a Bradley 1-phase model or scenario.
│   │   │   └── .svn/
│   │   └── ... (more data)
│   └── .svn/
├── scripts/                      # Utility scripts for setup, execution, or other tasks.
│   └── .svn/
├── solve/                        # Solvers and algorithms for computational problems.
│   ├── .svn/
│   ├── bcgs/                     # Biconjugate gradient stabilized solver.
│   ├── gmres/                    # Generalized minimal residual solver.
│   ├── hypre/                    # Hypre solver (scalable linear solvers and multigrid methods).
│   └── ... (more solvers)
├── visual/                       # Visualization tools, scripts, and sources.
│   ├── PV3/                      # ParaView version 3 files or scripts.
│   │   └── .svn/
│   ├── scripts/                  # Scripts for visualization purposes.
│   │   └── .svn/
│   └── tecplot/                  # Tecplot-specific visualization files or scripts.
│       └── .svn/
└── README.md                     # Markdown file with an overview and instructions for the repository.
```
