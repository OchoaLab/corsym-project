#! /bin/bash
###Ran with Slurm using 50G memory. This script takes the RFMix output and prepares it for input into the ancestor pipeline. 

###Load the conda environment with R installed. If you are not using conda, you can remove this line and load R in a different way.
source activate r_env

###Load directory containing per-chromosome RFMix files depending on two vs three way admixture, the primary file extension to pull, and the R script to run.
IN_DIR=/path/to/data/RFMIX/TWO_WAY/
IN_DIR=/path/to/data/RFMIX/THREE_WAY/
OUT_DIR=/path/to/data/ANCESTOR
EXT=.msp.tsv
R_SCRIPT=/path/to/scripts/01b_R-call_for_rfmix_processing.R

###Make sure the output directory exists
mkdir -p ${OUT_DIR}

###Loop through the msp files in the input directory and run the R script on each file, saving the individual ancestry block data. Each iteration of the loop appends an additional chromosome onto a individual sample's file.
for FILE in ${IN_DIR}*.msp.tsv; do
  echo "Processing ${FILE}"
  CMD=$(echo Rscript ${R_SCRIPT} "${FILE}" "${OUT_DIR}")
  echo $CMD
  eval $CMD
done

###Make ancestor input directory and print ancestor input specific files by taking a subset of columns from the individuals ancestry block files.
ANC_INPUT=$WORK/anc_input
mkdir -p ${ANC_INPUT}

for FILE in ${OUT_DIR}/*.tsv; do
	awk -v FS='\t' -v OFS='\t' 'FNR > 1 { print $2, $3, $4, $5 }' "$FILE" > "${ANC_INPUT}/$(basename "$FILE")"
done
