#!/bin/csh

#SBATCH --job-name=THRESHOLD_LOAD
#SBATCH --ntasks=32
#SBATCH --partition=compute
#SBATCH --constraint=cascadelake
#SBATCH --time=99:00:00
#SBATCH --mail-user=paulukonis.elizabeth@epa.gov
#SBATCH --mail-type=BEGIN,END

setenv TMPDIR /work/HONEYBEE/eap/pollinator_probabilistic_loading
setenv OMP_NUM_THREADS 32

module load intel/21.4     
module load R/4.2.0
module load gcc/6.1.0
module load geos/3.10.2
module load gdal-3.4.2/intel-21
module load proj-8.2.1/intel-21
module load udunits-2.2.28/intel-21



Rscript  00_setup_t.R