
# the oldest, first pass test showing order independence for corsym and not pearson
tests.R

# runs deep replicates to confirm that for corsym an offset of 1.5 is much better than 3 (the default for Pearson)
time Rscript CI-test-offset.R 
# 4m23.727s viiiaR5

# tests with more n and rho values (fewer replicates) confirming good coverages in broad cases
time Rscript CI-test.R
# 63m29.830s viiiaR5

