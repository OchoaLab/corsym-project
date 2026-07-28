#! /bin/bash

###Ran with Slurm using 50G memory. This script runs ADMIXTURE on the merged trio and reference file to estimate ancestry proportions for each individual in the dataset.

###Load ADMIXTURE conda environment.
source activate admixture

###Load the input directory containing the merged trio and reference file
IN_DIR=/path/to/data/ADMIXTURE/

# this is number of ancestries to test for, which is set in the sbatch command using --array=2-6
K=${SLURM_ARRAY_TASK_ID} 
    
###Run ADMIXTURE on the merged trio and reference file for the specified number of ancestries (K) and save the output to the output directory
cd $IN_DIR
time admixture -j12 ${IN_DIR}MERGED_TRIOS_REFS_PHASED_LD_PRUNED.bed $K
###Run ADMIXTURE cross-validation on the merged trio and reference file for the specified number of ancestries (K) and save the output to the output directory
time admixture -j12 --cv  ${IN_DIR}MERGED_TRIOS_REFS_PHASED_LD_PRUNED.bed $K | tee $OUT_PATH/log${K}.out
