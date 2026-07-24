#! /bin/bash

####Ran with Slurm using 25G memory. This script is used to combine the BEAGLE output VCF files for each chromosome into a single VCF file for the reference and target populations.

####Load modules bcftools
module load bcftools

###Set the input directories for the reference and target populations
IN_DIR=/path/to/data/BEAGLE/

####Combine the BEAGLE output VCF files for each chromosome into a single VCF file for the reference and target populations. The output files are compressed downstream analysis.
ls ${IN_DIR}/TRIOS.chr*_beagle.vcf.gz > ${IN_DIR}/beagle_vcf_list.txt
bcftools concat -Oz -o ${IN_DIR}/TRIOS.all_chrs.vcf.gz -f ${IN_DIR}/beagle_vcf_list.txt
ls ${IN_DIR}/REFS.chr*_beagle.vcf.gz > ${IN_DIR}/beagle_vcf_list.txt
bcftools concat -Oz -o ${IN_DIR}/REFS.all_chrs.vcf.gz -f ${IN_DIR}/beagle_vcf_list.txt
