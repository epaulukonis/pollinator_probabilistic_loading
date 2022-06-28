#!/bin/csh

#SBATCH --job-name=AFFINIS_LOAD
#SBATCH --ntasks=32
#SBATCH --partition=compute
#SBATCH --constraint=cascadelake
#SBATCH --time=99:00:00
#SBATCH --mail-user=paulukonis.elizabeth@epa.gov
#SBATCH --mail-type=BEGIN,END

setenv TMPDIR /work/HONEYBEE/eap/run_affinis_load
setenv OMP_NUM_THREADS 32

module load intel/19.0.5      
module load R/4.1.0
module load gcc/6.1.0
module load geos/3.8.1
module load gdal-3.3.0/intel-19.0
module load proj-7.2.1/intel-19.0
module load udunits-2.2.26/intel-19.0

Rscript  00_setup_c.R