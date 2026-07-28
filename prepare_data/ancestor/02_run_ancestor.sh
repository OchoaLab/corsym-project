#! /bin/bash

###Ran with Slurm using 75G memory with an job array of 170 for our tested trios. This script takes the output from the rfmix pipeline and runs the ancestor.py script on each individual ancestry block file to generate the final output for each individual.

###Load the conda environment with python installed. If you are not using conda, you can remove this line and load python in a different way.
source activate python_env

###Load the directory containing the individual ancestry block files, the output directory for the ancestor.py output, and the path to the ancestor.py script. Designate the sample to process based on the SLURM_ARRAY_TASK_ID.
ANCESTOR=/path/to/ancestor_python_script/ancestor.py
DIR="/path/to/data/ANCESTOR"
OUT_DIR=$DIR/ANC_OUTPUT 
IN_DIR=$DIR/ANC_INPUT
mkdir -p ${OUT_DIR}
SAMPLE=$SLURM_ARRAY_TASK_ID

###Select the input file for the current sample based on the SLURM_ARRAY_TASK_ID. The input file is selected from the list of files in the input directory, sorted, and the line corresponding to the current sample number is chosen.
INPUT_SAMPLE=$IN_DIR/$(ls -1 $IN_DIR | sort | sed -n "${SAMPLE}p")

###Set the out prefix for the output files based on the input sample name. 
PREFIX=$OUT_DIR/$(basename "${INPUT_SAMPLE%%.*}")

###Run ANCESTOR and save the output to a text file with the same prefix as the input sample in the output directory. 
python ${ANCESTOR} ${INPUT_SAMPLE} ${PREFIX}.pickle > ${PREFIX}.txt
