#! /bin/bash

####Ran with Slurm using 75G memory and 22 tasks in the job array. This script is used to run RFMix on the BEAGLE output VCF files for each chromosome in parallel to infer local ancestry for the target population using the reference population.

####Load modules rfmix
source activate RF

###Set the input and output directories, set the SLURM_ARRAY_TASK_ID variable to determine which chromosome to process in each task of the job array.
DIR=/path/to/data/
OUT_DIR=$DIR/RFMIX/
IN_DIR=$DIR/BEAGLE/
MAPS=/path/to/HG38_genetic_map/
i=$SLURM_ARRAY_TASK_ID

###If running three way admixture, the ${DIR}/rfmix_two_way_ref.txt file should contain the sample IDs of the reference populations
###If running three way admixture, the ${DIR}/rfmix_three_way_ref.txt file should contain the sample IDs of the reference populations
SAMPLES=${DIR}/rfmix_two_way_ref.txt
SAMPLES=${DIR}/rfmix_three_way_ref.txt
OUT_DIR=$OUT_DIR/TWO_WAY/
OUT_DIR=$OUT_DIR/THREE_WAY/
mkdir -p $OUT_DIR
OUT_PREFIX=$OUT_DIR/TRIOS_RFMIX_TWO_WAY
OUT_PREFIX=$OUT_DIR/TRIOS_RFMIX_THREE_WAY

####Run RFMix on the BEAGLE output VCF files for the specified chromosome. 
CMD1=$(echo rfmix -f ${IN_DIR}/TRIOS.chr${i}_beagle.vcf.gz \
	-r ${IN_DIR}/REFS.chr${i}_beagle.vcf.gz \
    -m $SAMPLES \
	-g $MAPS/chr${i}.tsv \
    -o ${OUT_PREFIX}_chr${i} \
    --chromosome=${i})
echo $CMD1
eval $CMD1



