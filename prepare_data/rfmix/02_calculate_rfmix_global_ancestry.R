###Ran with 25Gs of memory

###Load libraries
library(data.table)
library(dplyr)
library(purrr)

###Load directory containing per-chromosome RFMix files depending on two vs three way admixture
RFMIX_DIR <- "/path/to/data/RFMIX/TWO_WAY/"
RFMIX_DIR <- "/path/to/data/RFMIX/THREE_WAY/"

###Grab all .Q RFMix output files
FILES <- list.files(path = RFMIX_DIR, pattern = "*.rfmix.Q", full.names = TRUE)
OUTPUT_FILE <- paste0(RFMIX_DIR, "RFMix_global_ancestry.tsv")

# Read and combine all chromosomes
ANC_DATA <- lapply(FILES, function(f) {
  fread(f,header = TRUE )}) %>%
  bind_rows()

# Calculate mean ancestry across chromosomes for each individual
GLOBAL_ANC <- ANC_DATA %>%
  group_by(`#sample`) %>%
  summarise(
    Pop1 = mean(HGDP, na.rm = TRUE),
    Pop2 = mean(ibs, na.rm = TRUE),
    Pop3 = mean(yri, na.rm = TRUE)
  ) %>%
  ungroup()

# Save output
write.table(
  GLOBAL_ANC,
  OUTPUT_FILE,
  quote = FALSE,
  sep = "\t",
  row.names = FALSE
)
