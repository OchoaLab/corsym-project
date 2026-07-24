#! /bin/bash

####Ran with Slurm using 10G memory and 22 tasks in the job array. This script is used to run BEAGLE on the reference and target VCF files for each chromosome in parallel.

###Load samtools
export PATH="~/miniconda3/envs/samtools/bin:$PATH"

###Set the input and output directories and the path to the BEAGLE jar file. Set the SLURM_ARRAY_TASK_ID variable to determine which chromosome to process in each task of the job array. 
###The map files for each chromosome where downloaded from https://bochet.gcc.biostat.washington.edu/beagle/genetic_maps/
###Beagle jar is available at https://faculty.washington.edu/browning/beagle/beagle.html
SCRIPTS=/path/to/beagle_jars/
IN_DIR=/path/to/data/BEAGLE/
MAPS=/path/to/HG38_recomb_map/
i=$SLURM_ARRAY_TASK_ID

###Run Beagle of the reference and target VCF files for the specified chromosome. The output files are compressed and indexed using tabix for downstream analysis.
CMD1=$(echo java -Xss5m -Xmx5g -jar ${SCRIPTS}beagle.27Feb25.75.jar \
		gt=${TRIO_DIR}/TRIOS.chr${i}_redone.vcf \
		out=${TRIO_DIR}/TRIOS.chr${i}_beagle \
		chrom=${i} \
		map=${MAPS}/plink.chr${i}.GRCh38.map
	)
echo $CMD1
eval $CMD1
tabix -p vcf ${TRIO_DIR}/TRIOS.chr${i}_beagle.vcf.gz

CMD2=$(echo java -Xss5m -Xmx5g -jar ${SCRIPTS}beagle.27Feb25.75.jar \
		gt=${REF_DIR}/REFS.chr${i}_redone.vcf \
		out=${REF_DIR}/REFS.chr${i}_beagle \
		chrom=${i} \
		map=${MAPS}/plink.chr${i}.GRCh38.map
	)
echo $CMD2
eval $CMD2
tabix -p vcf ${REF_DIR}/REFS.chr${i}_beagle.vcf.gz
