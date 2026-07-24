#! /bin/bash

###Ran with Slurm using 25G memory and 1 CPU. This script is used to prepare the data for BEAGLE splitting merged reference and target data into separate VCF files for each chromosome. 

###Load modules plink2 and samtools
source activate samtools
source activate plink2

###Set the input and output directories
IN_DIR=/path/to/data/
IN_FILE=${IN_DIR}/MERGED_REF_TRIOS_std_dups_removed
OUT_DIR=$IN_DIR/BEAGLE/
mkdir -p $OUT_DIR

####Split the merged reference and target data into separate VCF files for each chromosome. The output files are compressed and indexed using tabix for downstream analysis.
for chr in {1..22}; do
	plink2 --pfile ${IN_FILE} --keep $IN_DIR/trio_keep.ids --chr $chr --export vcf --out $OUT_DIR/TRIOS.chr${chr}_redone
	tabix -p vcf $OUT_DIR/TRIOS.chr${chr}_redone.vcf.gz

	plink2 --pfile ${IN_FILE} --keep $IN_DIR/ref_keep.ids --chr $chr --export vcf --out $OUT_DIR/REFS.chr${chr}_redone
	tabix -p vcf $OUT_DIR/REFS.chr${chr}_redone.vcf.gz
done
