###This script takes the RFMix output and prepares it for input into the ancestor pipeline. It reads in a .msp.tsv file, reshapes the data, and outputs individual ancestry blocks for each individual in the dataset.

###Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)

###Load in arguments for the input file and output directory
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript 01b_R-call_for_rfmix_processing.R <input.msp.tsv> <output_dir>")
}

INPUT_FILE <- args[1]
OUTPUT_DIR <- args[2]

###Read in the RFMix output file, skipping the first line which contains subpopulation order codes. For our runs, the order was NAM=0, IBS=1, YRI=1.
DF <- read.table(INPUT_FILE,
                 header = TRUE, sep = "\t", 
                 stringsAsFactors = FALSE,
                 skip = 1,
                 comment.char="")

###Define the informational metadata and individual sample columns.
META_COLS <- c("X.chm", "spos", "epos", "sgpos", "egpos","n.snps")
IND_COLS  <- setdiff(colnames(DF), META_COLS)

###Reshape the data to long format, separating out the individual and haplotype information.
LONG_DF <- DF %>%
  pivot_longer(
    cols = all_of(IND_COLS),
    names_to = "ind_hap",
    values_to = "ancestry"
  ) %>%
  mutate(
    individual = str_remove(ind_hap, "\\.[01]$"),
    haplotype  = ifelse(str_detect(ind_hap, "\\.1$"), "hap1", "hap2")
  ) %>%
  arrange(individual, haplotype, X.chm, sgpos)

####Separate the haplotype data into two data frames and merge them back together per individuals and location, calculating the length in centimorgans for each ancestry block. Then select only the individual, chromosome, haplotype ancestry selected, and centimorgan length columns for output.
HAP1_DF<-subset(LONG_DF,haplotype=="hap1")
HAP2_DF<-subset(LONG_DF,haplotype=="hap2")
HAP_DF<-merge(HAP1_DF,HAP2_DF,by=c("spos","epos",'individual','X.chm',
                                   "sgpos","egpos","n.snps"))%>%
  mutate(length_CM=egpos - sgpos) %>%
  select(-c("n.snps","spos","epos","sgpos","egpos","ind_hap.x","ind_hap.y","haplotype.x","haplotype.y"))
colnames(HAP_DF)<-c("individual","chrom","h1","h2","cm")

###Get the unique individuals in the dataset.
INDIVIDUALS <- unique(HAP_DF$individual)

###For each individual, filter the data frame to that individual and write out a tab-separated file containing the ancestry blocks for that individual. The output files are named <individual>_ancestry_blocks.tsv and are saved in the specified output directory. If it already exists, the new data is appended to the existing file.
for (IND in INDIVIDUALS) {
	OUT_FILE<-file.path(OUTPUT_DIR,paste0(IND,"_ancestry_blocks.tsv"))
	IND_DF <- HAP_DF %>%
	  filter(individual == IND)
	write.table(
		    IND_DF,
		    file = OUT_FILE,
		    sep = "\t",
		    row.names = FALSE,
		    col.names = !file.exists(OUT_FILE),
		    quote = FALSE,
		    append = file.exists(OUT_FILE)
 )
}
