#!/bin/bash
# Created by the University of Melbourne job script generator for SLURM
# Mon May 25 2026 21:18:38 GMT+1000 (Australian Eastern Standard Time)

# Partition for the job:
#SBATCH --partition=sapphire,cascade

# Multithreaded (SMP) job: must run on one node
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="m6long"

# The project ID which this job should run under:
#SBATCH --account="punim1422"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2

# The amount of memory in megabytes per node:
#SBATCH --mem=4096

# Use this email address:
#SBATCH --mail-user=ryange@unimelb.edu.au

# Send yourself an email when the job:
# aborts abnormally (fails)
#SBATCH --mail-type=FAIL
# begins
#SBATCH --mail-type=BEGIN
# ends successfully
#SBATCH --mail-type=END

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=0-0:20:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from this directory:
cd /data/gpfs/projects/punim1422/va_multispecies_sdm

# The modules to load:
module load foss/2022a
module load CMake/3.24.3
module load GCC/11.3.0
module load OpenMPI/4.1.4
module load R/4.5.0-update
module load FriBidi/1.0.12
module load HarfBuzz/4.2.1
module load R-bundle-CRAN/2025.05
module unload SciPy-bundle/2022.05
module unload Python/3.10.4

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
# The job command(s):
Rscript --vanilla spartan_model_comparison/run_m6.R

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s
