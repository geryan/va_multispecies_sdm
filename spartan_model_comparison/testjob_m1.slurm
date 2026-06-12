#!/bin/bash
# Created by the University of Melbourne job script generator for SLURM
# Mon May 25 2026 21:18:38 GMT+1000 (Australian Eastern Standard Time)

# Partition for the job:
#SBATCH --partition=sapphire,cascade

# Multithreaded (SMP) job: must run on one node
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="m1"

# The project ID which this job should run under:
#SBATCH --account="punim1422"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# The amount of memory in megabytes per node:
#SBATCH --mem=1536

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
#SBATCH --time=0-0:10:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from this directory:
cd /data/gpfs/projects/punim1422/va_multispecies_sdm

# The modules to load:
module load R/4.5.0
module load foss/2022a
module load CMake/3.24.3
module load GDAL/3.5.0

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
# The job command(s):
Rscript --vanilla spartan_model_comparison/run_m1.R

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s
