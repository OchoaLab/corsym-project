#! /bin/bash

####Ran with Slurm using 5G memory and 1 CPU. This script is used to prepare the data for RFMix by filtering the samples to keep only those of interest and creating a new PLINK file with standardized variant IDs.
####QC is run on the data to remove variants with more than 2 alleles, missing genotypes, and low minor allele frequency. The script also removes duplicate variants and standardizes variant IDs.
####This scripts filters the reference panel and the target data separately, and then merges them together. The merged data is then checked for any remaining duplicates.

###Set the input and output directories
IN_DIR=/path/to/data/
IN_REF=tgp_hgdp_reference_idfixed_popsubset
IN_TRIOS=tgp-nygc-autosomes-trios_popsubset
OUT_DIR=path/to/output/trio_analysis
mkdir -p $OUT_DIR
OUT_FILE=MERGED_REF_TRIOS_std

###Run QC on the reference and target data separately to remove variants with more than 2 alleles, missing genotypes, and low minor allele frequency. 
###Exporting the data as bfile for merge.
plink2 --pfile $IN_DIR/$IN_REF \
  --set-all-var-ids @:#\$r,\$a \
  --max-alleles 2 --min-alleles 2 \
  --geno \
  --maf 0.01 \
  --rm-dup force-first \
  --snps-only just-acgt \
  --new-id-max-allele-len 1 missing \
  --make-bed --out ${OUT_DIR}/${IN_REF}_std

plink2 --bfile ${IN_DIR}/$IN_TRIOS  \
  --set-all-var-ids @:#\$r,\$a --max-alleles 2 --min-alleles 2 \
  --geno \
  --maf 0.01 \
  --snps-only just-acgt \
  --new-id-max-allele-len 1 missing \
  --make-bed --out ${OUT_DIR}/${IN_TRIOS}_std

###Merge the reference and target data together The merged data is then exported as a pfile for downstream analysis.
plink --bfile ${OUT_DIR}/${IN_REF}_std --bmerge ${OUT_DIR}/${IN_TRIOS}_std \
	--make-bed \
	--out ${OUT_DIR}/${OUT_FILE}

####Check for any remaining duplicates in the merged data and remove them. The final output is a pfile with standardized variant IDs and no duplicates.
plink2 --bfile ${OUT_DIR}/${OUT_FILE} \
  --set-all-var-ids @:#\$r,\$a \
  --rm-dup force-first \
  --make-pfile --out ${OUT_DIR}/${OUT_FILE}_dups_removed
