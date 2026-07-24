## Overview

## Overview
1. Corsym as an r function (cosym-project/corsym.R)
2. Simulated Data Analysis (cosym-project/analysis/260720_Simulated_Data_Analysis.Rmd)
3. General QC before any analysis (cosym-project/prepare_data/01_QC_and_Merge)
4. Phasing for RFMix analysis (cosym-project/prepare_data/02_Beagle)
5. RFMix Run (cosym-project/scripts/admixture_inference/RFMix)
6. Preparation for ANCESTOR Analysis (cosym-project/prepare_data/03_ANCESTOR)
7. ANCESTOR Run (cosym-project/admixture_inference/ANCESTOR)
8. Preparation for ADMIXTURE Analysis (cosym-project/prepare_data/04_ADMIXUTRE)
9. ADMIXTURE Run (cosym-project/admixture_inference/ADMIXTURE)
11. Process Data (cosym-project/data/)
12. Trio Analysis (corsym-project/analysis/260720_Trio_Data_Analysis.Rmd)

## Repository layout

```
admixture_inference/   Each method of running ancestry inference including RFMix, ANCESTOR, and ADMIXTURE
analyses/              Simulated and empirical data analyses
inference_data/        Exported inference runs
prepare_data/          Genotype QC, missingness, reference and test set merge, LD runs
scripts/               Corsym as a function in R 
```
