#!/bin/bash
#SBATCH -J ipars              # Job name
#SBATCH -o ipars.o%j          # Name of stdout output file (%j expands to jobId)
#SBATCH -N 1                  # Total number of nodes requested
#SBATCH -n 4                  # Total number of mpi tasks requested

# Log START of batch job to logfile
msg="`date`: START  job=${SLURM_JOBID} pwd=`pwd` system=${SYSTEM} nodes=${SLURM_NNODES} cores=${SLURM_NTASKS}"
echo "$msg"
echo "$msg" >> ~/${SYSTEM}.log

# Log SLURM environment to standard out file
env | grep "SLURM"

STARTTIME=$(date +%s)
mpirun ./ipars         # Execute parallel run
STATUS=$?

# Compute elapsed time of job
ENDTIME=$(date +%s)
elap=$((ENDTIME - STARTTIME))
ELAPSED=`printf "$((elap / 86400))d";date -d "0 $elap sec" +"%Hh%Mm"`

# Log FINISH of batch job to logfile
msg="`date`: FINISH job=${SLURM_JOBID} pwd=`pwd` system=${SYSTEM} nodes=${SLURM_NNODES} cores=${SLURM_NTASKS} elapsed=${ELAPSED} status=${STATUS}"
echo "$msg"
echo "$msg" >> ~/${SYSTEM}.log

