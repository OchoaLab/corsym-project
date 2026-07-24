#! /bin/bash

####Ran with Slurm using 5G memory and 1 CPU. This script is used to merge reference bcf files and convert them to PLINK format. 
####Extract and  update sample IDs from different HGDP native American populations to a single population label "HGDP" for analysis. 

###Set the input and output directories
IN_DIR=/path/to/data/hgdp_1kgp
OUT_DIR=/path/to/output/hgdp
OUT_FILE=tgp_hgdp_reference

####Load modules bcftools, plink, and plink2
module load bcftools
module load Plink/1.90
export PATH="~/miniconda3/envs/plink_test/bin:$PATH"

###Concatenate the reference bcf files and convert to VCF format
bcftools concat -Ob -o $OUT_DIR/$OUT_FILE.bcf $IN_DIR/*chr*.bcf
bcftools index $OUT_DIR/$OUT_FILE.bcf
bcftools view $OUT_DIR/$OUT_FILE.bcf -Ov -o $OUT_DIR/$OUT_FILE.vcf

###Convert the VCF file to PLINK format
plink2 --vcf $OUT_DIR/$OUT_FILE.vcf --make-bed --out $OUT_DIR/$OUT_FILE

####Update sample IDs for HGDP native American populations to a single population label "HGDP"
awk '{
  if ($1=="pima" || $1=="maya" || $1=="colombian" || $1=="karitiana" || $1=="surui" ) {
    print $1, $2, "HGDP", $2
  }
}' $OUT_DIR/$OUT_FILE.fam > $OUT_DIR/update_ids.txt

plink2 --bfile $OUT_DIR/$OUT_FILE \
      --update-ids $OUT_DIR/update_ids.txt \
      --make-bed \
      --out $OUT_DIR/${OUT_FILE}_idfixed
