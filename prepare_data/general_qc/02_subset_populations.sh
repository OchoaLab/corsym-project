#! /bin/bash

####Ran with Slurm using 50G memory and 1 CPU. This script takes both references and trio files and subsets each to only the populations of interest. The three references and the admixed population test cases. Also, created id keep files to be used downstream in RFMix preparation.

###Load modules samtools, plink, and plink2
source activate samtools
source activate plink2
module load Plink/1.90

###Set the input and output directories
IN_DIR="/path/to/data/"
IN_FILE_REF=$IN_DIR/tgp_hgdp_reference_idfixed
IN_FILE_TRIO=${IN_DIR}/tgp-nygc-autosomes-trios
REF_POPS='$1=="yri" || $1=="ibs" || $1=="HGDP"'
TEST_POPS='$1=="MXL" || $1=="PUR" || $1=="PEL" || $1=="CLM" || $1=="ASW" || $1=="ACB"'
POP_NAME="HGDP_with_TGP_trios"

###Filter out non-reference populations from the reference plink file and convert to plink2, pgen format
awk "$REF_POPS { print }" ${IN_FILE_REF}.psam > ${IN_DIR}/ref_keep.ids
plink2 --bfile ${IN_FILE_REF} --keep ${IN_DIR}/ref_keep.ids --make-pgen --out ${IN_FILE_REF}_popsubset

###Filter out non-target populations from the trio plink2 file
awk "$TEST_POPS { print }" ${IN_FILE_TRIO}.psam > ${IN_DIR}/trio_keep.ids
plink2 --pfile ${IN_FILE_TRIO} --keep ${IN_DIR}/trio_keep.ids --make-pgen --out ${IN_FILE_TRIO}_popsubset
