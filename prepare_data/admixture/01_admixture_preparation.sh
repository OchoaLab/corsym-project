#! /bin/bash

###Ran with Slurm using 50G memory. This script takes the RFMix output and prepares it for input into the ancestor pipeline.

###Load modules plink and samtools.
source activate samtools
module load Plink/1.90

###Load the input directory containing the merged trio and reference file
IN_DIR=/path/to/data/BEAGLE/
OUT_PATH=/path/to/data/ADMIXTURE
mkdir -p $OUT_PATH
OUT_FILE=$OUT_PATH/MERGED_TRIOS_REFS.phased

###Index and merge the trio and reference files using plink. 
tabix -f -p vcf ${IN_DIR}/TRIOS.all_chrs.vcf.gz
tabix -f -p vcf ${IN_DIR}/REFS.all_chrs.vcf.gz
plink --bfile ${IN_DIR}/TRIOS.all_chrs_bfile --bmerge ${IN _DIR}/REFS.all_chrs_bfile --make-bed --out ${OUT_FILE}

###Prune the merged file for linkage disequilibrium and remove SNPs with high missingness. The pruned file will be used for input into ADMIXTURE.
plink --bfile ${OUT_FILE} --indep-pairwise 50 5 0.2 --out ${OUT_FILE}_pruned
plink --bfile ${OUT_FILE} \
    --extract ${OUT_FILE}_pruned.prune.in \
    --make-bed --out ${OUT_FILE}_ld_pruned

