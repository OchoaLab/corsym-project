###This script simulates data from the simulated functions. Data is reordered as necessary for testing and corsym and pearsons' R are calculated for each run. This data acts as input into the plotting script

# load new function
source( 'corsym.R' )
source( 'simulation_analysis_functions.R' )

# load libraries
library(MASS)
library(tidyverse)
library(copula)

############################
#Figure 1A Data
############################
#Simulate data by rho for 1, -.5, 0, -0.5, and 1 for n=1000
rhos<-c(-1,-.5,0,.5,1)
n=1000

#Call simulation
dat <- do.call(rbind, lapply(rhos, simulate_cordata))
