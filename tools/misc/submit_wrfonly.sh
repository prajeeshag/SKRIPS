#!/bin/bash
#SBATCH -N 50
#SBATCH --partition=workq
#SBATCH --ntasks-per-node=32
#SBATCH -o std_out_no_io
#SBATCH -t 24:00:00
#SBATCH --mail-user=prajeesh.athippattagopinathan@kaust.edu.sa
#SBATCH --mail-type=ALL

. env.shaheen_intel
time srun --ntasks 1600 /lustre2/project/k1028/pag/s2s/build_WRF/WRF/main/wrf.exe 
